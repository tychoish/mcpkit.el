;;; test-mcpkit.el --- Tests for mcpkit -*- lexical-binding: t; -*-

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-ask)

(defmacro mcpkit-test--with-clean-env (&rest body)
  "Run BODY with clean `mcpkit-registry' and `mcpkit--active-services'."
  (declare (indent 0))
  `(let ((mcpkit-registry nil)
         (mcpkit--active-services nil)
         (mcpkit--active-server nil)
         (mcpkit-log-verbose nil))
     (unwind-protect
         (progn ,@body)
       (when mcpkit--active-server
         (ignore-errors (ws-stop mcpkit--active-server))
         (setq mcpkit--active-server nil)))))

;;; Service Registration and Lookup Tests

(ert-deftest mcpkit-test-service-registration-and-lookup ()
  "Test service definition, lookup, and removal."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'test-svc
                 :port 9999
                 :description "Test service description")))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'test-svc))
      (should (= (mcpkit-service-port svc) 9999))
      (should (equal (mcpkit-service-description svc) "Test service description"))
      (should (equal (mcpkit-service-log-buffer-name svc) "*mcpkit-test-svc*"))

      ;; Lookup by symbol, string, and instance
      (should (eq (mcpkit-get-service 'test-svc) svc))
      (should (eq (mcpkit-get-service "test-svc") svc))
      (should (eq (mcpkit-get-service svc) svc))
      (should-not (mcpkit-get-service 'non-existent))

      ;; Removal
      (mcpkit-remove-service 'test-svc)
      (should-not (mcpkit-get-service 'test-svc)))))

;;; Tool Definition Tests

(ert-deftest mcpkit-test-tool-definition-sync ()
  "Test synchronous tool registration and invocation."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'calc-svc)))
      (mcpkit-register-tool add svc
        :description "Add two numbers."
        :input-schema '(:type "object"
                        :properties (:a (:type "number") :b (:type "number"))
                        :required ["a" "b"])
        (+ (plist-get args :a) (plist-get args :b)))

      (let ((tool (gethash "add" (mcpkit-service-tools svc))))
        (should (mcpkit-tool-p tool))
        (should (equal (mcpkit-tool-name tool) "add"))
        (should (equal (mcpkit-tool-description tool) "Add two numbers."))
        (should (equal (plist-get (mcpkit-tool-input-schema tool) :type) "object"))

        ;; Verify handler invocation
        (let (cb-err cb-res)
          (funcall (mcpkit-tool-handler tool)
                   '(:a 10 :b 32)
                   (lambda (err res)
                     (setq cb-err err
                           cb-res res)))
          (should-not cb-err)
          (should (= cb-res 42)))))))

(ert-deftest mcpkit-test-tool-definition-async ()
  "Test asynchronous tool registration with explicit done callback."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'async-svc)))
      (mcpkit-register-tool delayed-echo svc
        :description "Async echo tool."
        :async t
        (funcall done nil (format "echo:%s" (plist-get args :val))))

      (let ((tool (gethash "delayed-echo" (mcpkit-service-tools svc))))
        (should (mcpkit-tool-p tool))
        (let (cb-err cb-res)
          (funcall (mcpkit-tool-handler tool)
                   '(:val "hello")
                   (lambda (err res)
                     (setq cb-err err
                           cb-res res)))
          (should-not cb-err)
          (should (equal cb-res "echo:hello")))))))

;;; Parameter Schema and Custom Encoders/Decoders

(ert-deftest mcpkit-test-tool-custom-decode-and-encode ()
  "Test custom decoder and encoder functions on tools."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'custom-codec-svc)))
      (mcpkit-register-tool uppercase-val svc
        :description "Uppercase string tool."
        :decode (lambda (raw) (list :text (upcase (plist-get raw :text))))
        :encode (lambda (res) (list (list :type "text" :text (concat "RESULT: " res))))
        (plist-get args :text))

      (let ((tool (gethash "uppercase-val" (mcpkit-service-tools svc))))
        (should (functionp (mcpkit-tool-decode tool)))
        (should (functionp (mcpkit-tool-encode tool)))

        ;; Decode transforms args
        (let ((decoded (funcall (mcpkit-tool-decode tool) '(:text "emacs"))))
          (should (equal decoded '(:text "EMACS"))))

        ;; Encode wraps return value
        (let ((encoded (funcall (mcpkit-tool-encode tool) "EMACS")))
          (should (equal encoded '((:type "text" :text "RESULT: EMACS")))))))))

(ert-deftest mcpkit-test-default-encode ()
  "Test `mcpkit--default-encode' with strings, plists, and general objects."
  (should (equal (mcpkit--default-encode "hello")
                 '((:type "text" :text "hello"))))
  (should (equal (mcpkit--default-encode '(:type "image" :data "xyz"))
                 '((:type "image" :data "xyz"))))
  (should (equal (mcpkit--default-encode '((:type "text" :text "1") (:type "text" :text "2")))
                 '((:type "text" :text "1") (:type "text" :text "2"))))
  (should (equal (mcpkit--default-encode 123)
                 '((:type "text" :text "123")))))

;;; JSON-RPC Payload Parsing and Serialization

(ert-deftest mcpkit-test-json-rpc-codec ()
  "Test low-level JSON-RPC serialization and error/success response structures."
  (let* ((parsed (mcpkit--parse-request "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\"}")))
    (should (equal (plist-get parsed :jsonrpc) "2.0"))
    (should (= (plist-get parsed :id) 1))
    (should (equal (plist-get parsed :method) "initialize")))

  (let* ((resp (mcpkit--make-success-response 42 '(:ok t)))
         (serialized (mcpkit--serialize-response resp)))
    (should (equal (plist-get resp :jsonrpc) "2.0"))
    (should (= (plist-get resp :id) 42))
    (should (string-search "\"id\":42" serialized)))

  (let ((err-resp (mcpkit--make-error-response 42 mcpkit-error-invalid-params "Bad param" "details")))
    (should (equal (plist-get err-resp :jsonrpc) "2.0"))
    (let ((err (plist-get err-resp :error)))
      (should (= (plist-get err :code) mcpkit-error-invalid-params))
      (should (equal (plist-get err :message) "Bad param"))
      (should (equal (plist-get err :data) "details")))))

;;; Request Dispatching: Initialize, tools/list, tools/call

(ert-deftest mcpkit-test-dispatch-initialize ()
  "Test JSON-RPC initialize dispatch."
  (mcpkit-test--with-clean-env
    (let (dispatched-status dispatched-resp)
      (mcpkit--handle-request-payload
       "{\"jsonrpc\":\"2.0\",\"id\":\"init-1\",\"method\":\"initialize\"}"
       mcpkit--active-services
       (lambda (status resp &rest _rest)
         (setq dispatched-status status
               dispatched-resp resp)))
      (should (eq dispatched-status 'success))
      (should (equal (plist-get dispatched-resp :id) "init-1"))
      (let ((res (plist-get dispatched-resp :result)))
        (should (equal (plist-get res :protocolVersion) "2024-11-05"))
        (should (equal (plist-get (plist-get res :serverInfo) :name) "mcpkit"))))))

(ert-deftest mcpkit-test-dispatch-tools-list ()
  "Test JSON-RPC tools/list dispatch."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'svc-tools)))
      (mcpkit-register-tool test-tool-1 svc
        :description "Tool one"
        :input-schema '(:type "object" :properties (:x (:type "string")))
        "result")
      (push (cons svc 'error) mcpkit--active-services)

      (let (dispatched-status dispatched-resp)
        (mcpkit--handle-request-payload
         "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"tools/list\"}"
         mcpkit--active-services
         (lambda (status resp &rest _rest)
           (setq dispatched-status status
                 dispatched-resp resp)))
        (should (eq dispatched-status 'success))
        (let* ((tools (plist-get (plist-get dispatched-resp :result) :tools))
               (first-tool (aref tools 0)))
          (should (vectorp tools))
          (should (= (length tools) 1))
          (should (equal (plist-get first-tool :name) "test-tool-1"))
          (should (equal (plist-get first-tool :description) "Tool one")))))))

(ert-deftest mcpkit-test-dispatch-tools-call-success ()
  "Test JSON-RPC tools/call dispatch with successful execution."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'math-svc)))
      (mcpkit-register-tool multiply svc
        :description "Multiply a and b"
        (* (plist-get args :a) (plist-get args :b)))
      (push (cons svc 'error) mcpkit--active-services)

      (let (dispatched-status dispatched-resp dispatched-tool)
        (mcpkit--handle-request-payload
         "{\"jsonrpc\":\"2.0\",\"id\":\"call-1\",\"method\":\"tools/call\",\"params\":{\"name\":\"multiply\",\"arguments\":{\"a\":6,\"b\":7}}}"
         mcpkit--active-services
         (lambda (status resp tool-name &rest _rest)
           (setq dispatched-status status
                 dispatched-resp resp
                 dispatched-tool tool-name)))
        (should (eq dispatched-status 'success))
        (should (equal dispatched-tool "multiply"))
        (let* ((result (plist-get dispatched-resp :result))
               (content (plist-get result :content)))
          (should (vectorp content))
          (should (equal (plist-get (aref content 0) :text) "42")))))))

(ert-deftest mcpkit-test-dispatch-tools-call-errors ()
  "Test JSON-RPC tools/call error handling for missing tools, invalid params, and handler errors."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'err-svc)))
      (mcpkit-register-tool fail-tool svc
        (error "Deliberate failure in handler"))
      (push (cons svc 'error) mcpkit--active-services)

      ;; 1. Tool not found
      (let (status resp)
        (mcpkit--handle-request-payload
         "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"nonexistent\"}}"
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'error))
        (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-invalid-params)))

      ;; 2. Missing tool name parameter
      (let (status resp)
        (mcpkit--handle-request-payload
         "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{}}"
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'error))
        (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-invalid-params)))

      ;; 3. Handler error
      (let (status resp)
        (mcpkit--handle-request-payload
         "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"fail-tool\"}}"
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'error))
        (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-internal-error))))))

(ert-deftest mcpkit-test-dispatch-protocol-errors ()
  "Test handling of malformed JSON and unsupported methods."
  (mcpkit-test--with-clean-env
    ;; 1. Malformed JSON
    (let (status resp)
      (mcpkit--handle-request-payload
       "{bad json"
       mcpkit--active-services
       (lambda (s r &rest _rest) (setq status s resp r)))
      (should (eq status 'error))
      (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-parse-error)))

    ;; 2. Method not found
    (let (status resp)
      (mcpkit--handle-request-payload
       "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"unknown/method\"}"
       mcpkit--active-services
       (lambda (s r &rest _rest) (setq status s resp r)))
      (should (eq status 'error))
      (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-method-not-found)))))

;;; Collision Handling Tests

(ert-deftest mcpkit-test-collision-handling ()
  "Test tool collision resolution via namespacing and collision error checking."
  (mcpkit-test--with-clean-env
    (let ((svc-a (mcpkit-define-service 'alpha))
          (svc-b (mcpkit-define-service 'beta)))
      (mcpkit-register-tool ping svc-a "Alpha ping" "alpha-pong")
      (mcpkit-register-tool ping svc-b "Beta ping" "beta-pong")

      ;; Test namespacing policy
      (let* ((active-list (list (cons svc-a 'namespace) (cons svc-b 'namespace)))
             (normalized (mcpkit--normalize-tools active-list))
             (names (mapcar #'mcpkit-tool-name normalized)))
        (should (member "alpha__ping" names))
        (should (member "beta__ping" names)))

      ;; Test collision error policy in start-service
      (push (cons svc-a 'error) mcpkit--active-services)
      (should-error (mcpkit-start-service 'beta :on-collision 'error)
                    :type 'user-error))))

;;; Service List Mode Tabulation Tests

(ert-deftest mcpkit-test-service-list-tabulation ()
  "Test service list entries and tabular format generation."
  (mcpkit-test--with-clean-env
    (let ((svc1 (mcpkit-define-service 'svc-first :port 8001 :description "First Svc"))
          (svc2 (mcpkit-define-service 'svc-second :port 8002 :description "Second Svc")))
      (mcpkit-register-tool t1 svc1 "T1" "res1")
      (mcpkit-register-tool t2 svc1 "T2" "res2")

      (let ((entries (mcpkit--service-list-entries)))
        (should (= (length entries) 2))
        (let* ((first-entry (assoc 'svc-first entries))
               (row (cadr first-entry)))
          (should (equal (aref row 0) "svc-first"))
          (should (equal (aref row 1) "inactive"))
          (should (equal (aref row 2) "8001"))
          (should (equal (aref row 3) "2"))
          (should (equal (aref row 4) "First Svc")))))))

;;; Agent-Shell-Ask Tools Registration Test

(ert-deftest mcpkit-test-ask-tools-registration ()
  "Test that `mcpkit-register-ask-tools' registers all expected ask tools."
  (mcpkit-test--with-clean-env
    (mcpkit-register-ask-tools)
    (let ((svc (mcpkit-get-service 'agent-shell-ask)))
      (should (mcpkit-service-p svc))
      (should (= (mcpkit-service-port svc) 8766))
      (let ((tool-table (mcpkit-service-tools svc)))
        (should (gethash "ask_user" tool-table))
        (should (gethash "poll_question" tool-table))
        (should (gethash "get_next_question" tool-table))
        (should (gethash "list_pending_questions" tool-table))
        (should (gethash "cancel_question" tool-table))))))

(provide 'test-mcpkit)
;;; test-mcpkit.el ends here
