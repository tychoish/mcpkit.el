;;; test-mcpkit-proxy.el --- Tests for mcpkit-proxy -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'url)

(require 'test-helper nil t)
(require 'mcpkit)
(require 'mcpkit-proxy)

;;; Pure Route-Resolution Logic

(ert-deftest test-mcpkit-proxy/route-registration ()
  "Registering and unregistering a route updates the routing table."
  (let ((mcpkit-proxy--routes (make-hash-table :test #'equal)))
    (should-not (mcpkit-proxy--route-port "t1"))
    (mcpkit-proxy-register-route "t1" 9001)
    (should (= (mcpkit-proxy--route-port "t1") 9001))
    (mcpkit-proxy-unregister-route "t1")
    (should-not (mcpkit-proxy--route-port "t1"))
    ;; Unregistering an unknown target-id is a no-op, not an error.
    (should-not (mcpkit-proxy-unregister-route "never-registered"))))

(ert-deftest test-mcpkit-proxy/path-parsing ()
  "URI path parsing extracts the target-id from `/sprite/<target-id>/mcp'."
  (should (equal (mcpkit-proxy--target-from-path "/sprite/abc-123/mcp") "abc-123"))
  (should-not (mcpkit-proxy--target-from-path "/mcp"))
  (should-not (mcpkit-proxy--target-from-path "/sprite/abc/other"))
  (should-not (mcpkit-proxy--target-from-path nil)))

(ert-deftest test-mcpkit-proxy/resolve-target-path-precedence ()
  "Path routing is tried first; header routing is only a fallback when enabled."
  (let ((mcpkit-proxy-enable-header-routing t))
    ;; Path present: header ignored even if present.
    (should (equal (mcpkit-proxy--resolve-target
                    (list (cons :POST "/sprite/from-path/mcp")
                          (cons :X-SPRITE-ID "from-header")))
                   (cons "from-path" 'path)))
    ;; No path match: header used.
    (should (equal (mcpkit-proxy--resolve-target
                    (list (cons :POST "/") (cons :X-SPRITE-ID "from-header")))
                   (cons "from-header" 'header))))
  ;; Header routing disabled: no routing info at all.
  (let ((mcpkit-proxy-enable-header-routing nil))
    (should (equal (mcpkit-proxy--resolve-target
                    (list (cons :POST "/") (cons :X-SPRITE-ID "from-header")))
                   (cons nil nil)))))

;;; Handler-Level Behavior (mocked transport, mirrors test-mcpkit.el conventions)

(defmacro test-mcpkit-proxy--with-mock-proc (proc-var &rest body)
  "Bind PROC-VAR to a throwaway process for the duration of BODY."
  (declare (indent 1))
  `(let ((,proc-var (make-process :name "mcpkit-proxy-test-proc" :buffer nil :command '("cat"))))
     (unwind-protect (progn ,@body)
       (delete-process ,proc-var))))

(ert-deftest test-mcpkit-proxy/missing-route-502 ()
  "A path match with no registered backend returns a 502 JSON-RPC error."
  (let ((mcpkit-proxy--routes (make-hash-table :test #'equal)))
    (test-mcpkit-proxy--with-mock-proc mock-proc
      (let (sent-code sent-body)
        (cl-letf (((symbol-function 'ws-response-header)
                   (lambda (_proc code &rest _headers) (setq sent-code code)))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc str) (setq sent-body str))))
          (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\"}")
                 (req (make-instance 'ws-request
                                     :process mock-proc
                                     :body body
                                     :headers (list (cons :POST "/sprite/unregistered-target/mcp")))))
            (mcpkit-proxy-handler req)
            (should (= sent-code 502))
            (should sent-body)
            (let* ((parsed (json-parse-string sent-body :object-type 'plist))
                   (err (plist-get parsed :error)))
              (should (equal (plist-get parsed :jsonrpc) "2.0"))
              (should (= (plist-get parsed :id) 7))
              (should err)
              (should (string-match-p "unregistered-target" (plist-get err :message))))))))))

(ert-deftest test-mcpkit-proxy/forwarding-failure-dead-port-502 ()
  "A route registered at a port nothing is listening on returns a 502 JSON-RPC
error rather than raising a Lisp error, and the error-logging path fires."
  (let* ((mcpkit-proxy--routes (make-hash-table :test #'equal))
         ;; Bind a real ephemeral server socket just to learn a genuinely
         ;; free port number, then close it immediately: nothing is
         ;; listening on that port by the time we register the route, so
         ;; `mcpkit-proxy--forward-request' really fails to connect
         ;; (connection refused), rather than us guessing a fixed port
         ;; number that might collide with something else on the runner.
         (probe (make-network-process :name " mcpkit-proxy-test-dead-port-probe"
                                      :server t :host 'local :service t
                                      :noquery t :family 'ipv4))
         (dead-port (process-contact probe :service)))
    (delete-process probe)
    (mcpkit-proxy-register-route "dead-target" dead-port)
    (test-mcpkit-proxy--with-mock-proc mock-proc
      (let (sent-code sent-body logged-errors)
        (cl-letf (((symbol-function 'ws-response-header)
                   (lambda (_proc code &rest _headers) (setq sent-code code)))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc str) (setq sent-body str)))
                  ((symbol-function 'mcpkit--log)
                   (lambda (status _target _dur msg)
                     (when (eq status 'error) (push msg logged-errors)))))
          (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":13,\"method\":\"tools/call\"}")
                 (req (make-instance 'ws-request
                                     :process mock-proc
                                     :body body
                                     :headers (list (cons :POST "/sprite/dead-target/mcp")))))
            ;; The handler must not signal -- a raised Lisp error here
            ;; would be a test failure via an uncaught condition, not
            ;; just a wrong status code.
            (mcpkit-proxy-handler req)
            (should (= sent-code 502))
            (should sent-body)
            (let* ((parsed (json-parse-string sent-body :object-type 'plist))
                   (err (plist-get parsed :error)))
              (should (equal (plist-get parsed :jsonrpc) "2.0"))
              (should (= (plist-get parsed :id) 13))
              (should err)
              (should (string-match-p "forwarding to target" (plist-get err :message)))
              (should (string-match-p "dead-target" (plist-get err :message))))
            (should logged-errors)
            (should (seq-some (lambda (m) (string-match-p "dead-target" m)) logged-errors))))))))

(ert-deftest test-mcpkit-proxy/header-routing-disabled-by-default ()
  "A request bearing only X-Sprite-ID with no path match is rejected, not routed."
  (should-not mcpkit-proxy-enable-header-routing)
  (let ((mcpkit-proxy--routes (make-hash-table :test #'equal)))
    (mcpkit-proxy-register-route "target-h" 9999)
    (test-mcpkit-proxy--with-mock-proc mock-proc
      (let (sent-code sent-body forward-called)
        (cl-letf (((symbol-function 'ws-response-header)
                   (lambda (_proc code &rest _headers) (setq sent-code code)))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc str) (setq sent-body str)))
                  ((symbol-function 'mcpkit-proxy--forward-request)
                   (lambda (&rest _args) (setq forward-called t) (cons 200 "{}"))))
          (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\"}")
                 (req (make-instance 'ws-request
                                     :process mock-proc
                                     :body body
                                     :headers (list (cons :POST "/")
                                                    (cons :X-SPRITE-ID "target-h")))))
            (mcpkit-proxy-handler req)
            (should-not forward-called)
            (should (= sent-code 400))
            (let* ((parsed (json-parse-string sent-body :object-type 'plist))
                   (err (plist-get parsed :error)))
              (should err)
              (should (string-match-p "routing" (plist-get err :message))))))))))

(ert-deftest test-mcpkit-proxy/header-routing-enabled ()
  "Once enabled, X-Sprite-ID header routes to the registered backend."
  (let ((mcpkit-proxy-enable-header-routing t)
        (mcpkit-proxy--routes (make-hash-table :test #'equal)))
    (mcpkit-proxy-register-route "target-h" 12345)
    (test-mcpkit-proxy--with-mock-proc mock-proc
      (let (sent-code sent-body forwarded-port)
        (cl-letf (((symbol-function 'ws-response-header)
                   (lambda (_proc code &rest _headers) (setq sent-code code)))
                  ((symbol-function 'process-send-string)
                   (lambda (_proc str) (setq sent-body str)))
                  ((symbol-function 'mcpkit-proxy--forward-request)
                   (lambda (port _body) (setq forwarded-port port) (cons 200 "{\"ok\":true}"))))
          (let* ((body "{\"jsonrpc\":\"2.0\",\"id\":11,\"method\":\"tools/call\"}")
                 (req (make-instance 'ws-request
                                     :process mock-proc
                                     :body body
                                     :headers (list (cons :POST "/")
                                                    (cons :X-SPRITE-ID "target-h")))))
            (mcpkit-proxy-handler req)
            (should (= forwarded-port 12345))
            (should (= sent-code 200))
            (should (equal sent-body "{\"ok\":true}"))))))))

;;; End-to-End Round Trip (real sockets, real backend mcpkit service)

(ert-deftest test-mcpkit-proxy/path-routing-round-trip ()
  "A request proxied via `/sprite/<target-id>/mcp' round-trips to a real backend."
  (let ((mcpkit-registry nil)
        (mcpkit--active-services nil)
        (mcpkit--active-server nil)
        (mcpkit-proxy--server nil)
        (mcpkit-proxy--routes (make-hash-table :test #'equal))
        (backend-port 18795)
        (proxy-port 18796))
    (unwind-protect
        (progn
          (mcpkit-define-service 'proxy-test-backend :port backend-port)
          (mcpkit-register-tool echo 'proxy-test-backend
            :description "Echo test tool"
            (format "Echo: %s" (plist-get args :msg)))
          (mcpkit-start-service 'proxy-test-backend :on-collision 'error)
          (mcpkit-proxy-start proxy-port)
          (mcpkit-proxy-register-route "target-a" backend-port)
          (let* ((url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data
                  (encode-coding-string
                   "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"msg\":\"hi\"}}}"
                   'utf-8))
                 (buf (url-retrieve-synchronously
                       (format "http://127.0.0.1:%d/sprite/target-a/mcp" proxy-port)
                       t t 10)))
            (should buf)
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (re-search-forward "HTTP/[0-9.]+[[:space:]]+200" nil t))
                  (goto-char (point-min))
                  (should (re-search-forward "\r?\n\r?\n" nil t))
                  (let ((json-body (buffer-substring-no-properties (point) (point-max))))
                    (should (string-match-p "Echo: hi" json-body))
                    (should (string-match-p "\"id\":1" json-body))))
              (kill-buffer buf))))
      (mcpkit-proxy-stop)
      (mcpkit-stop-service 'proxy-test-backend))))

(provide 'test-mcpkit-proxy)
;;; test-mcpkit-proxy.el ends here
