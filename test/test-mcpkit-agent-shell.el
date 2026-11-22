;;; test-mcpkit-agent-shell.el --- Optional bridge tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'cl-lib)
(when (locate-library "agent-shell") (require 'agent-shell))
(require 'mcpkit-agent-shell)
(defvar agent-shell-mcp-servers nil)
(defvar sprite-instance-id nil)
(defvar server-name nil)
(defconst mcpkit-agent-shell-test--source
  (expand-file-name "../mcpkit-agent-shell.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest mcpkit-agent-shell-test-load-does-not-require-services ()
  "Loading the bridge does not require the service manager."
  (let ((original-require (symbol-function 'require))
        (after-load-alist nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (when (eq feature 'mcpkit-services)
                   (ert-fail "Bridge eagerly required mcpkit-services"))
                 (apply original-require feature args))))
      (load mcpkit-agent-shell-test--source nil t))))

(ert-deftest mcpkit-agent-shell-test-real-deferred-load ()
  (when (featurep 'agent-shell)
    (ert-skip "agent-shell is already loaded"))
  (let ((after-load-alist nil)
        (agent-shell-mcp-servers nil)
        (mcpkit-agent-shell-mode nil)
        (file (make-temp-file "agent-shell-test" nil ".el" "(provide 'agent-shell)")))
    (unwind-protect
        (progn
          (dotimes (_ 3)
            (mcpkit-agent-shell-mode 1)
            (mcpkit-agent-shell-mode -1))
          (should (= 1 (length (cdr (assq 'agent-shell after-load-alist)))))
          (load file nil t)
          (should-not agent-shell-mcp-servers))
      (mcpkit-agent-shell-mode -1)
      (when (featurep 'agent-shell)
        (unload-feature 'agent-shell t))
      (delete-file file))))

(ert-deftest mcpkit-agent-shell-test-real-deferred-enable ()
  (when (featurep 'agent-shell)
    (ert-skip "agent-shell is already loaded"))
  (let ((after-load-alist nil)
        (agent-shell-mcp-servers nil)
        (mcpkit-agent-shell-mode nil)
        (file (make-temp-file "agent-shell-test" nil ".el" "(provide 'agent-shell)")))
    (unwind-protect
        (progn
          (mcpkit-agent-shell-mode 1)
          (load file nil t)
          (should (= 1 (length agent-shell-mcp-servers)))
          (mcpkit-agent-shell-mode -1)
          (should-not agent-shell-mcp-servers))
      (mcpkit-agent-shell-mode -1)
      (when (featurep 'agent-shell)
        (unload-feature 'agent-shell t))
      (delete-file file))))

(when (featurep 'agent-shell)
(defmacro mcpkit-agent-shell-test--with-mode (&rest body)
  "Run BODY with isolated bridge registration."
  (declare (indent 0))
  `(let ((agent-shell-mcp-servers nil)
         (agent-shell--state nil)
         (mcpkit-agent-shell-mode nil))
     (unwind-protect
         (progn (mcpkit-agent-shell-mode 1) ,@body)
       (mcpkit-agent-shell-mode -1))))

(ert-deftest mcpkit-agent-shell-test-named-resolver-is-not-evaluated ()
  (let ((agent-shell--state nil)
        (agent-shell-mcp-servers (list #'mcpkit-agent-shell--server-spec)))
    (should-error (agent-shell--mcp-servers) :type 'cl-no-applicable-method)))

(ert-deftest mcpkit-agent-shell-test-deduplicate-and-clean-resolvers ()
  (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765)))
    (mcpkit-agent-shell-test--with-mode
      ;; Simulate an older duplicate closure already in agent-shell-mcp-servers
      (push (lambda () (mcpkit-agent-shell--server-spec)) agent-shell-mcp-servers)
      (should (= (length agent-shell-mcp-servers) 2))
      ;; Resolution should deduplicate mcpkit resolvers rather than erroring with duplicate name
      (let ((servers (agent-shell--mcp-servers)))
        (should (= (length servers) 1))
        (should (equal (map-elt (aref servers 0) 'name) "emacs")))
      ;; Re-installing cleans up duplicates
      (mcpkit-agent-shell--install)
      (should (= (length agent-shell-mcp-servers) 1)))))

(ert-deftest mcpkit-agent-shell-test-resolution-and-cleanup ()
  (let ((port 8765))
    (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () port)))
      (mcpkit-agent-shell-test--with-mode
        (let ((other '((name . "other") (command . "true"))))
          (push other agent-shell-mcp-servers)
          (should (functionp mcpkit-agent-shell--resolver))
          (should (equal (map-elt (aref (agent-shell--mcp-servers) 1) 'url)
                         "http://127.0.0.1:8765/mcp"))
          (setq port 8766)
          (should (equal (map-elt (aref (agent-shell--mcp-servers) 1) 'url)
                         "http://127.0.0.1:8766/mcp"))
          (setq port nil)
          (should (= (length (agent-shell--mcp-servers)) 1))
          (mcpkit-agent-shell-mode -1)
          (should (equal agent-shell-mcp-servers (list other))))))))

(ert-deftest mcpkit-agent-shell-test-server-list-replaced-after-enable ()
  "Preserve the bridge when later customization replaces the server list."
  (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765)))
    (mcpkit-agent-shell-test--with-mode
      (setq agent-shell-mcp-servers '(((name . "other") (command . "true"))))
      (let ((servers (agent-shell--mcp-servers)))
        (should (= (length servers) 2))
        (should (equal (map-elt (aref servers 0) 'name) "emacs"))
        (should (equal (map-elt (aref servers 1) 'name) "other"))))))

(ert-deftest mcpkit-agent-shell-test-dynamic-collision-and-override ()
  (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765)))
    (mcpkit-agent-shell-test--with-mode
      (push (lambda () '((name . "emacs") (command . "true"))) agent-shell-mcp-servers)
      (should-error (agent-shell--mcp-servers) :type 'user-error)
      (let ((agent-shell--state '(:agent-config (:mcp-servers (((name . "other") (command . "true")))))))
        (should (equal (map-elt (aref (agent-shell--mcp-servers) 0) 'name) "other"))))))

(ert-deftest mcpkit-agent-shell-test-caller-environment ()
  (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765)))
    (let ((out (mcpkit-agent-shell--inject-environment
                (lambda (&rest args) (plist-get args :environment-variables))
                :environment-variables '("MCPKIT_PORT=custom" "OTHER=yes"))))
      (should (equal (car out) "MCPKIT_PORT=custom"))
      (should (= 1 (length (seq-filter (lambda (v) (string-prefix-p "MCPKIT_PORT=" v)) out))))
      (should (member "EMACS_MCP_URL=http://127.0.0.1:8765/mcp" out)))))

(ert-deftest mcpkit-agent-shell-test-static-collision ()
  (let ((agent-shell-mcp-servers '(((name . "emacs") (command . "true"))))
        (mcpkit-agent-shell-mode nil))
    (should-error (mcpkit-agent-shell-mode 1) :type 'user-error)
    (should-not mcpkit-agent-shell-mode)))

(ert-deftest mcpkit-agent-shell-test-deferred-toggle ()
  "Install and remove the bridge even before agent-shell loads."
  (let ((agent-shell-mcp-servers nil)
        (mcpkit-agent-shell-mode nil))
    (unwind-protect
        (cl-letf (((symbol-function 'featurep) (lambda (&rest _) nil)))
          (mcpkit-agent-shell-mode 1)
          (should (= 1 (length agent-shell-mcp-servers)))
          (should (advice-member-p #'mcpkit-agent-shell--resolve-servers
                                   'agent-shell--mcp-servers))
          (mcpkit-agent-shell-mode 1)
          (should (= 1 (length agent-shell-mcp-servers)))
          (mcpkit-agent-shell-mode -1)
          (should-not agent-shell-mcp-servers)
          (should-not (advice-member-p #'mcpkit-agent-shell--resolve-servers
                                       'agent-shell--mcp-servers)))
      (mcpkit-agent-shell-mode -1))))

(ert-deftest mcpkit-agent-shell-test-proxy-and-identity ()
  (let ((sprite-instance-id "test-instance")
        (server-name "test-server"))
    (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765))
              ((symbol-function 'mcpkit-agent-shell--proxy-port)
               (lambda (port instance)
                 (should (= port 8765))
                 (should (equal instance "test-instance"))
                 9876)))
      (should (equal (map-elt (mcpkit-agent-shell--server-spec) 'url)
                     "http://127.0.0.1:9876/sprite/test-instance/mcp"))
      (should (member "EMACS_SPRITE_ID=test-instance" (mcpkit-agent-shell--environment)))
      (should (member "EMACS_SERVER_NAME=test-server" (mcpkit-agent-shell--environment)))
      (should (member "EMACS_MCP_URL=http://127.0.0.1:8765/mcp" (mcpkit-agent-shell--environment))))))

(ert-deftest mcpkit-agent-shell-test-real-proxy-route ()
  (require 'mcpkit-proxy)
  (let ((mcpkit-proxy--server nil)
        (mcpkit-proxy--routes (make-hash-table :test #'equal))
        (sprite-instance-id "test-instance"))
    (unwind-protect
        (let ((proxy-port (mcpkit-allocate-free-port)))
          (mcpkit-proxy-register-route sprite-instance-id 8765)
          (mcpkit-proxy-start proxy-port)
          (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () 8765)))
            (should (equal (map-elt (mcpkit-agent-shell--server-spec) 'url)
                           (format "http://127.0.0.1:%d/sprite/test-instance/mcp" proxy-port)))))
      (mcpkit-proxy-stop))))

(ert-deftest mcpkit-agent-shell-test-port-reads-cached-active-port ()
  "When the MCP server is already active, reading the port is immediate."
  (let ((mcpkit--active-server 'live-server)
        (mcpkit--active-port 21226)
        (init-called nil))
    (cl-letf (((symbol-function 'mcpkit-init-services-sync)
               (lambda () (setq init-called t))))
      (should (= (mcpkit-agent-shell--port) 21226))
      (should-not init-called))))

(ert-deftest mcpkit-agent-shell-test-port-initializes-when-not-yet-up ()
  "When called before services are up, sync init runs and sets the port."
  (let ((mcpkit--active-server nil)
        (mcpkit--active-port nil)
        (starts 0))
    (cl-letf (((symbol-function 'mcpkit-init-services-sync)
               (lambda ()
                 (setq starts (1+ starts)
                       mcpkit--active-server 'started-server
                       mcpkit--active-port 8765))))
      (should (= (mcpkit-agent-shell--port) 8765))
      (should (= starts 1))
      ;; Subsequent calls read the cached value without re-running init
      (should (= (mcpkit-agent-shell--port) 8765))
      (should (= starts 1)))))

(ert-deftest mcpkit-agent-shell-test-port-self-heals-on-retry ()
  "Transient startup failure returns nil without poisoning subsequent retries."
  (let ((mcpkit--active-server nil)
        (mcpkit--active-port nil)
        (attempt 0))
    (cl-letf (((symbol-function 'mcpkit-init-services-sync)
               (lambda ()
                 (setq attempt (1+ attempt))
                 (if (= attempt 1)
                     (error "transient bind error")
                   (setq mcpkit--active-server 'recovered-server
                         mcpkit--active-port 8766)))))
      (should-not (mcpkit-agent-shell--port))
      (should (= attempt 1))
      (should (= (mcpkit-agent-shell--port) 8766))
      (should (= attempt 2)))))

(ert-deftest mcpkit-agent-shell-test-proxy-requires-live-process ()
  (let ((mcpkit-proxy--server 'stale-server))
    (cl-letf (((symbol-function 'mcpkit-proxy-list-routes)
               (lambda () '((:target_id "test-instance" :port 8765))))
              ((symbol-function 'ws-process) (lambda (_) nil)))
      (should-not (mcpkit-agent-shell--proxy-port 8765 "test-instance")))))

(ert-deftest mcpkit-agent-shell-test-repeat-collision-cleans-up ()
  (mcpkit-agent-shell-test--with-mode
    (push '((name . "emacs") (command . "true")) agent-shell-mcp-servers)
    (should-error (mcpkit-agent-shell-mode 1) :type 'user-error)
    (should-not mcpkit-agent-shell-mode)
    (should-not (memq mcpkit-agent-shell--resolver agent-shell-mcp-servers))
    (should-not (advice-member-p #'mcpkit-agent-shell--resolve-servers 'agent-shell--mcp-servers))))

(ert-deftest mcpkit-agent-shell-test-unrelated-errors-propagate ()
  (cl-letf (((symbol-function 'mcpkit-agent-shell--port) (lambda () nil)))
    (mcpkit-agent-shell-test--with-mode
      (push (lambda () (error "unrelated resolver error")) agent-shell-mcp-servers)
      (should-error (agent-shell--mcp-servers) :type 'error))))

)
(provide 'test-mcpkit-agent-shell)
;;; test-mcpkit-agent-shell.el ends here
