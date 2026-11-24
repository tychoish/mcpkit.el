;;; test-mcpkit.el --- Tests for mcpkit -*- lexical-binding: t; -*-

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-stats)

(defmacro mcpkit-test--with-clean-env (&rest body)
  "Run BODY with clean `mcpkit-registry' and `mcpkit--active-services'.
Also binds a private `mcpkit-stats--table' so tool calls made by tests are
not recorded in the live session's statistics."
  (declare (indent 0))
  `(let ((mcpkit-registry nil)
         (mcpkit-stats--table (make-hash-table :test #'equal))
         (mcpkit--active-services nil)
         (mcpkit--active-server nil)
         (mcpkit--active-port nil)
         (mcpkit-log-verbose nil))
     (unwind-protect
         (progn ,@body)
       (when mcpkit--active-server
         (ignore-errors (ws-stop mcpkit--active-server))
         (setq mcpkit--active-server nil
               mcpkit--active-port nil)))))

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
      (should (equal (mcpkit-service-log-buffer-name svc) " *mcpkit-test-svc*"))

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
        (should (eq (mcpkit-tool-service tool) 'calc-svc))
        (should (mcpkit-tool-source-file tool))

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

(ert-deftest mcpkit-test-dispatch-tools-list-search-gating ()
  "Test tools/list gating behind search_tools and pinned tools when count exceeds threshold."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'svc-many-tools)))
      ;; Register 5 tools
      (dotimes (i 5)
        (let ((tname (format "tool_%d" i)))
          (puthash tname
                   (make-mcpkit-tool
                    :name tname
                    :description (format "Description for tool %d" i)
                    :handler (lambda (_args cb) (funcall cb "ok"))
                    :service 'svc-many-tools)
                   (mcpkit-service-tools svc))))
      (push (cons svc 'error) mcpkit--active-services)

      ;; Threshold = 3, mode = auto -> should gate
      (let ((mcpkit-search-mode 'auto)
            (mcpkit-search-threshold 3)
            (mcpkit-search-pinned-tools '("tool_0" "search_tools")))
        (let (dispatched-status dispatched-resp)
          (mcpkit--handle-request-payload
           "{\"jsonrpc\":\"2.0\",\"id\":20,\"method\":\"tools/list\"}"
           mcpkit--active-services
           (lambda (status resp &rest _)
             (setq dispatched-status status
                   dispatched-resp resp)))
          (should (eq dispatched-status 'success))
          (let* ((tools (plist-get (plist-get dispatched-resp :result) :tools))
                 (names (mapcar (lambda (td) (plist-get td :name)) (append tools nil))))
            ;; Pinned tool_0 and synthetic search_tools
            (should (member "tool_0" names))
            (should (member "search_tools" names))
            ;; Unpinned tool_1 should not be listed
            (should-not (member "tool_1" names))))

        ;; Unlisted tool_1 should still be directly callable via tools/call!
        (let (call-status call-resp)
          (mcpkit--handle-request-payload
           "{\"jsonrpc\":\"2.0\",\"id\":21,\"method\":\"tools/call\",\"params\":{\"name\":\"tool_1\",\"arguments\":{}}}"
           mcpkit--active-services
           (lambda (status resp &rest _)
             (setq call-status status
                   call-resp resp)))
          (should (eq call-status 'success))
          (let* ((content (plist-get (plist-get call-resp :result) :content))
                 (text (plist-get (aref content 0) :text)))
            (should (equal text "ok"))))))))

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

(ert-deftest mcpkit-test-start-service-free-port ()
  "A `t' port resolves to a real free port, not the literal `t'."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'free-port-svc)))
      (unwind-protect
          (progn
            (mcpkit-start-service svc :port t)
            (should (integerp (mcpkit-active-port)))
            (should-not (= (mcpkit-active-port) 8765)))
        (mcpkit-stop-all-services)))))

(ert-deftest mcpkit-test-service-modules-cover-known-services ()
  "Every first-party service module is loaded by default."
  (dolist (m '(orgx-mcp builder-elpaish hitl-plan-mcp))
    (should (memq m mcpkit-service-modules))))

(ert-deftest mcpkit-test-server-discover ()
  "server/discover answers with server info instead of method-not-found."
  (mcpkit-test--with-clean-env
    (let (status resp)
      (mcpkit--handle-request-payload
       "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"server/discover\"}"
       mcpkit--active-services
       (lambda (s r &rest _rest) (setq status s resp r)))
      (should (eq status 'success))
      (let ((result (plist-get resp :result)))
        (should (equal (plist-get result :resultType) "complete"))
        (should (equal (plist-get (plist-get result :serverInfo) :name) "mcpkit"))
        (should (equal (plist-get (plist-get result :_meta)
                                  :io.modelcontextprotocol/serverInfo)
                       (plist-get result :serverInfo)))
        (should (plist-get result :instructions))
        (should-not (plist-member result :protocolVersion))))))

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

      (should (equal (mcpkit-registered-services) '(svc-second svc-first)))
      (should (equal (mcpkit--registered-services) '(svc-second svc-first)))

      (let ((entries (mcpkit--service-list-entries)))
        (should (= (length entries) 2))
        (let* ((first-entry (assoc 'svc-first entries))
               (row (cadr first-entry)))
          (should (equal (aref row 0) "svc-first"))
          (should (equal (aref row 1) "inactive"))
          (should (equal (aref row 2) "-"))
          (should (equal (aref row 3) "2"))
          (should (equal (aref row 4) "First Svc"))))

      (push (cons svc1 'namespace) mcpkit--active-services)
      (setq mcpkit--active-server 'mock-server
            mcpkit--active-port 21226)
      (let* ((entries (mcpkit--service-list-entries))
             (first-row (cadr (assoc 'svc-first entries)))
             (second-row (cadr (assoc 'svc-second entries))))
        (should (equal (aref first-row 1) "active"))
        (should (equal (aref first-row 2) "21226"))
        (should (equal (aref second-row 1) "inactive"))
        (should (equal (aref second-row 2) "-"))
        (setq mcpkit--active-server nil
              mcpkit--active-port nil)))))

;;; Agent-Shell-Ask Tools Registration Test


;;; Lifecycle & Multi-Service Tests

(ert-deftest mcpkit-test-startup-announcement-debug-only ()
  "Report the active port only when Emacs debugging is enabled."
  (mcpkit-test--with-clean-env
    (let ((debug-on-error nil)
          (init-file-debug nil)
          messages)
      (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () 8123))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (= 8123 (mcpkit-start-all-services 8123 :modules nil)))
        (should-not messages)
        (let ((debug-on-error t))
          (should (= 8123 (mcpkit-start-all-services 8123 :modules nil))))
        (should (equal messages '("[mcpkit]: active on port 8123 with 0 services")))
        (setq messages nil)
        (let ((init-file-debug t))
          (should (= 8123 (mcpkit-start-all-services 8123 :modules nil))))
        (should (equal messages '("[mcpkit]: active on port 8123 with 0 services")))))))

(ert-deftest mcpkit-test-lifecycle-management ()
  "Test multi-service load, start, free-port migration, and teardown."
  (mcpkit-test--with-clean-env
    (let ((port (mcpkit-allocate-free-port)))
      (should (integerp port))
      (should (> port 1024)))
    (let ((svc1 (mcpkit-define-service 'svc-a :description "Service A"))
          (svc2 (mcpkit-define-service 'svc-b :description "Service B")))
      (mcpkit-register-tool t1 svc1 "T1" "res1")
      (mcpkit-register-tool t2 svc2 "T2" "res2")
      ;; Start on free port without requiring external modules
      (let ((p1 (mcpkit-start-all-services 'free :modules nil)))
        (should (integerp p1))
        (should (equal (mcpkit-active-port) p1))
        (should (= (length mcpkit--active-services) 2))
        ;; Restart on a new free port
        (let ((p2 (mcpkit-restart-all-services 'free :modules nil)))
          (should (integerp p2))
          (should (equal (mcpkit-active-port) p2))
          (should (= (length mcpkit--active-services) 2)))
        ;; Teardown
        (should (mcpkit-stop-all-services))
        (should-not (mcpkit-active-port))
        (should-not mcpkit--active-services)))))



;;; Data Type Fidelity Tests

(ert-deftest mcpkit-test-datatype-fidelity-coercion ()
  "Test vector-to-list coercion across all nested and heterogeneous structures."
  ;; 1. Scalars remain untouched
  (should (equal (mcpkit--coerce-vector-to-list "hello") "hello"))
  (should (equal (mcpkit--coerce-vector-to-list 42) 42))
  (should (equal (mcpkit--coerce-vector-to-list 3.14) 3.14))
  (should (equal (mcpkit--coerce-vector-to-list t) t))
  (should (equal (mcpkit--coerce-vector-to-list nil) nil))
  (should (equal (mcpkit--coerce-vector-to-list :json-false) :json-false))

  ;; 2. Empty vector converts to nil (empty list)
  (should (equal (mcpkit--coerce-vector-to-list (vector)) nil))

  ;; 3. Flat vectors of strings, numbers, mixed
  (should (equal (mcpkit--coerce-vector-to-list (vector "a" "b" "c")) '("a" "b" "c")))
  (should (equal (mcpkit--coerce-vector-to-list (vector 1 2 3)) '(1 2 3)))
  (should (equal (mcpkit--coerce-vector-to-list (vector "text" 42 3.14 t nil :json-false))
                 '("text" 42 3.14 t nil :json-false)))

  ;; 4. Nested vectors (2D and 3D)
  (should (equal (mcpkit--coerce-vector-to-list (vector (vector 1 2) (vector 3 4)))
                 '((1 2) (3 4))))
  (should (equal (mcpkit--coerce-vector-to-list (vector (vector (vector "x"))))
                 '((("x")))))

  ;; 5. Plists with vectors
  (should (equal (mcpkit--coerce-vector-to-list (list :name "test" :tags (vector "x" "y")))
                 '(:name "test" :tags ("x" "y"))))

  ;; 6. Deeply nested plists with vectors
  (should (equal (mcpkit--coerce-vector-to-list
                  (list :outer (list :inner (list :tags (vector "a" "b")))))
                 '(:outer (:inner (:tags ("a" "b"))))))

  ;; 7. Vectors of plists containing vectors
  (should (equal (mcpkit--coerce-vector-to-list
                  (vector (list :id 1 :tags (vector "a" "b"))
                          (list :id 2 :tags (vector "c"))))
                 '((:id 1 :tags ("a" "b"))
                   (:id 2 :tags ("c")))))

  ;; 8. Lists of vectors
  (should (equal (mcpkit--coerce-vector-to-list
                  (list (vector 1 2) (vector 3 4)))
                 '((1 2) (3 4)))))

(ert-deftest mcpkit-test-datatype-fidelity-required-params ()
  "Test required parameter validation across schema formats and falsy values."
  (let ((tool-str (make-mcpkit-tool :name "t-str"
                                    :input-schema '(:type "object" :required ["foo"])))
        (tool-sym (make-mcpkit-tool :name "t-sym"
                                    :input-schema '(:type "object" :required [foo])))
        (tool-kw (make-mcpkit-tool :name "t-kw"
                                   :input-schema '(:type "object" :required [:foo])))
        (tool-list (make-mcpkit-tool :name "t-list"
                                     :input-schema '(:type "object" :required ("foo")))))

    ;; Missing in all formats
    (should (equal (mcpkit--missing-required-argument tool-str '(:other 1)) "foo"))
    (should (equal (mcpkit--missing-required-argument tool-sym '(:other 1)) "foo"))
    (should (equal (mcpkit--missing-required-argument tool-kw '(:other 1)) "foo"))
    (should (equal (mcpkit--missing-required-argument tool-list '(:other 1)) "foo"))

    ;; Falsy values must NOT be considered missing
    (should-not (mcpkit--missing-required-argument tool-str '(:foo nil)))
    (should-not (mcpkit--missing-required-argument tool-str '(:foo :json-false)))
    (should-not (mcpkit--missing-required-argument tool-str '(:foo 0)))
    (should-not (mcpkit--missing-required-argument tool-str '(:foo "")))
    (should-not (mcpkit--missing-required-argument tool-str '(:foo [])))))

(ert-deftest mcpkit-test-datatype-fidelity-roundtrip ()
  "Test end-to-end JSON-RPC dispatch preserving data type fidelity."
  (mcpkit-test--with-clean-env
    (let ((svc (mcpkit-define-service 'fidelity-svc)))
      ;; Register a tool that inspects all data types and returns structured verification
      (mcpkit-register-tool inspect-types svc
        :description "Tool to verify type fidelity"
        :input-schema '(:type "object"
                        :properties (:str (:type "string")
                                     :int_val (:type "integer")
                                     :float_val (:type "number")
                                     :bool_true (:type "boolean")
                                     :bool_false (:type "boolean")
                                     :empty_arr (:type "array")
                                     :str_arr (:type "array" :items (:type "string"))
                                     :nested_obj (:type "object")
                                     :arr_of_objs (:type "array")
                                     :nested_matrix (:type "array"))
                        :required ["str" "int_val" "bool_false"])
        (list :str_is_string (stringp (plist-get args :str))
              :str_val (plist-get args :str)
              :int_is_integer (integerp (plist-get args :int_val))
              :int_val (plist-get args :int_val)
              :float_is_float (floatp (plist-get args :float_val))
              :float_val (plist-get args :float_val)
              :bool_true_val (plist-get args :bool_true)
              :bool_false_val (plist-get args :bool_false)
              :empty_arr_is_list (listp (plist-get args :empty_arr))
              :empty_arr_nil (null (plist-get args :empty_arr))
              :str_arr_is_list (listp (plist-get args :str_arr))
              :str_arr_val (plist-get args :str_arr)
              :nested_obj_tags_is_list (listp (plist-get (plist-get args :nested_obj) :tags))
              :nested_obj_tags (plist-get (plist-get args :nested_obj) :tags)
              :arr_of_objs_len (length (plist-get args :arr_of_objs))
              :first_obj_slug (plist-get (car (plist-get args :arr_of_objs)) :slug)
              :matrix_is_list_of_lists (and (listp (plist-get args :nested_matrix))
                                            (listp (car (plist-get args :nested_matrix))))
              :matrix_val (plist-get args :nested_matrix)))
      (push (cons svc 'error) mcpkit--active-services)

      ;; 1. Successful dispatch with full payload
      (let* ((json-payload "{\"jsonrpc\":\"2.0\",\"id\":\"fid-1\",\"method\":\"tools/call\",\"params\":{\"name\":\"inspect-types\",\"arguments\":{\"str\":\"alpha\",\"int_val\":42,\"float_val\":3.1415,\"bool_true\":true,\"bool_false\":false,\"empty_arr\":[],\"str_arr\":[\"x\",\"y\",\"z\"],\"nested_obj\":{\"flag\":true,\"tags\":[\"t1\",\"t2\"]},\"arr_of_objs\":[{\"slug\":\"s1\",\"order\":1},{\"slug\":\"s2\",\"order\":2}],\"nested_matrix\":[[1,2],[3,4]]}}}")
             status resp)
        (mcpkit--handle-request-payload
         json-payload
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'success))
        (let* ((result (plist-get resp :result))
               (content (plist-get result :content)))
          (should (vectorp content))
          (let ((text (plist-get (aref content 0) :text)))
            (should (string-search ":str_is_string t" text))
            (should (string-search ":str_val \"alpha\"" text))
            (should (string-search ":int_is_integer t" text))
            (should (string-search ":int_val 42" text))
            (should (string-search ":float_is_float t" text))
            (should (string-search ":float_val 3.1415" text))
            (should (string-search ":bool_true_val t" text))
            (should (string-search ":bool_false_val nil" text))
            (should (string-search ":empty_arr_is_list t" text))
            (should (string-search ":empty_arr_nil t" text))
            (should (string-search ":str_arr_is_list t" text))
            (should (string-search ":str_arr_val (\"x\" \"y\" \"z\")" text))
            (should (string-search ":nested_obj_tags_is_list t" text))
            (should (string-search ":nested_obj_tags (\"t1\" \"t2\")" text))
            (should (string-search ":arr_of_objs_len 2" text))
            (should (string-search ":first_obj_slug \"s1\"" text))
            (should (string-search ":matrix_is_list_of_lists t" text))
            (should (string-search ":matrix_val ((1 2) (3 4))" text)))))

      ;; 2. Falsy required parameters are accepted
      (let* ((json-payload "{\"jsonrpc\":\"2.0\",\"id\":\"fid-2\",\"method\":\"tools/call\",\"params\":{\"name\":\"inspect-types\",\"arguments\":{\"str\":\"\",\"int_val\":0,\"bool_false\":false}}}")
             status resp)
        (mcpkit--handle-request-payload
         json-payload
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'success)))

      ;; 3. Missing required parameter returns error
      (let* ((json-payload "{\"jsonrpc\":\"2.0\",\"id\":\"fid-3\",\"method\":\"tools/call\",\"params\":{\"name\":\"inspect-types\",\"arguments\":{\"str\":\"present\",\"int_val\":1}}}")
             status resp)
        (mcpkit--handle-request-payload
         json-payload
         mcpkit--active-services
         (lambda (s r &rest _rest) (setq status s resp r)))
        (should (eq status 'error))
        (should (= (plist-get (plist-get resp :error) :code) mcpkit-error-invalid-params))
        (should (string-search "missing required parameter: bool_false"
                               (plist-get (plist-get resp :error) :message)))))))

(ert-deftest mcpkit-test-datatype-fidelity-return-encoding ()
  "Test return value encoding and serialization fidelity across diverse types."
  ;; String
  (let* ((enc (mcpkit--default-encode "plain text")))
    (should (equal enc '((:type "text" :text "plain text")))))

  ;; Number
  (let* ((enc (mcpkit--default-encode 100)))
    (should (equal enc '((:type "text" :text "100")))))

  ;; Float
  (let* ((enc (mcpkit--default-encode 3.1415)))
    (should (equal enc '((:type "text" :text "3.1415")))))

  ;; Structured Plist
  (let* ((enc (mcpkit--default-encode '(:status "ok" :count 5 :items ("a" "b")))))
    (should (equal (plist-get (car enc) :type) "text"))
    (should (string-search ":status \"ok\"" (plist-get (car enc) :text)))
    (should (string-search ":count 5" (plist-get (car enc) :text))))

  ;; Single custom content block
  (let* ((enc (mcpkit--default-encode '(:type "resource" :uri "file:///foo"))))
    (should (equal enc '((:type "resource" :uri "file:///foo")))))

  ;; Multiple content blocks
  (let* ((blocks '((:type "text" :text "chunk 1") (:type "text" :text "chunk 2")))
         (enc (mcpkit--default-encode blocks)))
    (should (equal enc blocks)))

  ;; Full serialization to JSON string and parse back
  (let* ((resp (mcpkit--make-success-response
                "res-1"
                (list :content (vector (list :type "text" :text "hello")
                                       (list :type "image" :data "base64data"))
                      :isError :json-false)))
         (json (mcpkit--serialize-response resp))
         (parsed (json-parse-string json :object-type 'plist :false-object :json-false :array-type 'list)))
    (should (equal (plist-get parsed :jsonrpc) "2.0"))
    (should (equal (plist-get parsed :id) "res-1"))
    (let ((result (plist-get parsed :result)))
      (should (equal (plist-get result :isError) :json-false))
      (should (= (length (plist-get result :content)) 2))
      (should (equal (plist-get (car (plist-get result :content)) :type) "text"))
      (should (equal (plist-get (car (plist-get result :content)) :text) "hello"))
      (should (equal (plist-get (cadr (plist-get result :content)) :type) "image"))
      (should (equal (plist-get (cadr (plist-get result :content)) :data) "base64data")))))

(ert-deftest mcpkit-test-standard-reporting ()
  "Verify standard reporting metrics (tool name, input/output size, duration) in mcpkit--log."
  (mcpkit-test--with-clean-env
    (let ((logx-events nil)
          (test-buf (get-buffer-create "*test-mcpkit-reporting*")))
      (unwind-protect
          (let ((mcpkit-log-buffer-name (buffer-name test-buf)))
            (cl-letf (((symbol-function 'logx--ring-push)
                       (lambda (ev) (push ev logx-events) ev)))
              ;; 1. Successful call reporting
              (mcpkit--log 'success "emacs_describe" 12.5 nil
                           "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"emacs_describe\"}}"
                           "{\"jsonrpc\":\"2.0\",\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"doc\"}]}}")
              (should logx-events)
              (let* ((ev (car logx-events))
                     (data (plist-get ev :data)))
                (should (eq (plist-get ev :lvl) 'info))
                (should (eq (plist-get ev :cat) 'mcpkit-tool))
                (should (equal (plist-get data :tool) "emacs_describe"))
                (should (equal (plist-get data :status) "ok"))
                (should (= (plist-get data :duration_ms) 12.5))
                (should (> (plist-get data :input_bytes) 0))
                (should (> (plist-get data :output_bytes) 0)))

              ;; Verify text buffer contains formatted line with input/output size
              (with-current-buffer test-buf
                (let ((text (buffer-string)))
                  (should (string-match-p "OK emacs_describe 12.50ms (in: [0-9]+B, out: [0-9]+B)" text))))

              ;; 2. Failed call reporting
              (mcpkit--log 'error "unknown_tool" 3.2 "Tool not found: unknown_tool"
                           "{\"tool\":\"unknown_tool\"}"
                           "{\"error\":-32601}")
              (let* ((err-ev (car logx-events))
                     (err-data (plist-get err-ev :data)))
                (should (eq (plist-get err-ev :lvl) 'error))
                (should (eq (plist-get err-ev :cat) 'mcpkit-tool))
                (should (equal (plist-get err-data :tool) "unknown_tool"))
                (should (equal (plist-get err-data :status) "error"))
                (should (= (plist-get err-data :duration_ms) 3.2))
                (should (equal (plist-get err-data :error) "Tool not found: unknown_tool"))
                (should (> (plist-get err-data :input_bytes) 0))
                (should (> (plist-get err-data :output_bytes) 0)))))
        (when (buffer-live-p test-buf)
          (kill-buffer test-buf))))))

(ert-deftest mcpkit-test-log-origin-and-rpc-labels ()
  "Verify mcpkit--log resolves tool source coordinates and formats RPC/Request labels."
  (mcpkit-test--with-clean-env
    (let ((logx-events nil)
          (svc (make-mcpkit-service--raw :name 'test-svc))
          (test-buf (get-buffer-create "*test-mcpkit-origin*")))
      (unwind-protect
          (let ((mcpkit-log-buffer-name (buffer-name test-buf)))
            ;; Register tool with known source file and line
            (let ((tool (make-mcpkit-tool
                         :name "test-svc__my_tool"
                         :service 'test-svc
                         :source-file "/path/to/my-tool.el"
                         :source-line 123)))
              (setf (map-elt (mcpkit-service-tools svc) "my_tool") tool)
              (setq mcpkit--active-services (list (cons svc 'namespace))))

            (cl-letf (((symbol-function 'logx--ring-push)
                       (lambda (ev) (push ev logx-events) ev)))
              ;; 1. Tool call should link to its registered call site
              (mcpkit--log 'success "test-svc__my_tool" 5.0 nil "{}" "{}")
              (should logx-events)
              (let* ((ev (car logx-events))
                     (src (plist-get ev :src)))
                (should (eq (plist-get ev :cat) 'mcpkit-tool))
                (should (string-match-p "Tool test-svc__my_tool completed" (plist-get ev :msg)))
                (should (equal (plist-get src :file) "my-tool.el"))
                (should (equal (plist-get src :line) 123)))

              ;; 2. Anonymous/nil tool call logs as Request under mcpkit-rpc (never "Tool - completed")
              (mcpkit--log 'success nil 0.5 nil "{}" "{}")
              (let ((ev (car logx-events)))
                (should (eq (plist-get ev :cat) 'mcpkit-rpc))
                (should (string-match-p "Request completed" (plist-get ev :msg)))
                (should-not (string-match-p "Tool -" (plist-get ev :msg))))

              ;; 3. RPC initialize logs as RPC initialize under mcpkit-rpc with resolved handler line
              (mcpkit--log 'success "initialize" 1.2 nil "{}" "{}")
              (let* ((ev (car logx-events))
                     (src (plist-get ev :src)))
                (should (eq (plist-get ev :cat) 'mcpkit-rpc))
                (should (string-match-p "RPC initialize completed" (plist-get ev :msg)))
                (should (equal (plist-get src :file) "mcpkit.el"))
                (should (> (plist-get src :line) 1))
                (should (eq (plist-get src :fn) 'mcpkit--handle-initialize)))

              ;; 4. Notification logs as INFO (never error) with Notification label
              (mcpkit--log 'notification "notifications/roots/list_changed" 0.05 nil "{}" nil)
              (let* ((ev (car logx-events))
                     (src (plist-get ev :src)))
                (should (eq (plist-get ev :lvl) 'info))
                (should (eq (plist-get ev :cat) 'mcpkit-rpc))
                (should (string-match-p "Notification notifications/roots/list_changed received" (plist-get ev :msg)))
                (should (equal (plist-get src :file) "mcpkit.el"))
                (should (> (plist-get src :line) 1)))

              ;; 5. Bare/unqualified tool name resolves to namespaced tool definition
              (mcpkit--log 'success "my_tool" 3.0 nil "{}" "{}")
              (let* ((ev (car logx-events))
                     (src (plist-get ev :src)))
                (should (eq (plist-get ev :cat) 'mcpkit-tool))
                (should (equal (plist-get src :file) "my-tool.el"))
                (should (= (plist-get src :line) 123)))))
        (when (buffer-live-p test-buf)
          (kill-buffer test-buf))))))

(provide 'test-mcpkit)
;;; test-mcpkit.el ends here
