;;; test-mcpkit-search.el --- Tests for mcpkit-search -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, search

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-search)
(require 'mcpkit-stats)

(ert-deftest mcpkit-search-test-parameter-extraction ()
  "Test parameter extraction from various schema forms."
  (let ((t1 (make-mcpkit-tool
             :name "t1"
             :input-schema '(:type "object"
                             :properties (:buffer_name (:type "string")
                                          :line_count (:type "integer")))))
        (t2 (make-mcpkit-tool
             :name "t2"
             :input-schema '(:type "object" :properties nil)))
        (t3 (make-mcpkit-tool :name "t3" :input-schema nil)))
    (should (equal (mcpkit-search--tool-parameter-names t1) '("buffer_name" "line_count")))
    (should (null (mcpkit-search--tool-parameter-names t2)))
    (should (null (mcpkit-search--tool-parameter-names t3)))))

(ert-deftest mcpkit-search-test-candidate-string ()
  "Test candidate string formatting for tools."
  (let ((tool (make-mcpkit-tool
               :name "buffer_append"
               :description "Append text to buffer"
               :input-schema '(:type "object"
                               :properties (:file_or_buffer (:type "string")
                                            :content (:type "string")))
               :service 'emacs)))
    (let ((cand (mcpkit-search--tool-candidate-string tool)))
      (should (string-prefix-p "buffer_append [emacs]: Append text to buffer (params: file_or_buffer content)" cand)))))

(ert-deftest mcpkit-search-test-fallback-filter ()
  "Test fallback multi-token filtering."
  (let ((cands '("buffer_read [emacs]: Read file into buffer (params: file start)"
                 "buffer_append [emacs]: Append text to buffer (params: file content)"
                 "dired [emacs]: Open dired buffer (params: dir)"
                 "org_task [org]: Capture org task (params: heading)")))
    ;; Out of order tokens
    (let ((res (mcpkit-search--fallback-filter "append buffer" cands)))
      (should (= (length res) 1))
      (should (string-prefix-p "buffer_append" (car res))))
    ;; Single token
    (let ((res (mcpkit-search--fallback-filter "org" cands)))
      (should (= (length res) 1))
      (should (string-prefix-p "org_task" (car res))))
    ;; Non-matching query
    (should (null (mcpkit-search--fallback-filter "nonexistentxyz" cands)))))

(ert-deftest mcpkit-search-test-ranking ()
  "Test relevance ranking prioritizing exact name, prefix, and descriptions."
  (let ((t-exact (make-mcpkit-tool :name "buffer" :description "Some tool"))
        (t-prefix (make-mcpkit-tool :name "buffer_write" :description "Write to buffer"))
        (t-desc (make-mcpkit-tool :name "read_file" :description "Reads into a buffer")))
    (let ((ranked (mcpkit-search--rank-tools "buffer" (list t-desc t-exact t-prefix))))
      (should (equal (mapcar #'mcpkit-tool-name ranked)
                     '("buffer" "buffer_write" "read_file"))))))

(ert-deftest mcpkit-search-test-query-search-and-limit ()
  "Test mcpkit-search-query with limits and active services."
  (let* ((svc (make-mcpkit-service--raw :name 'test-svc))
         (tools-hash (make-hash-table :test #'equal))
         (t1 (make-mcpkit-tool :name "open_file" :description "Open a file" :service 'test-svc))
         (t2 (make-mcpkit-tool :name "save_file" :description "Save a file" :service 'test-svc))
         (t3 (make-mcpkit-tool :name "find_file" :description "Find file in project" :service 'test-svc))
         (t4 (make-mcpkit-tool :name "close_buffer" :description "Close current buffer" :service 'test-svc)))
    (puthash "open_file" t1 tools-hash)
    (puthash "save_file" t2 tools-hash)
    (puthash "find_file" t3 tools-hash)
    (puthash "close_buffer" t4 tools-hash)
    (setf (mcpkit-service-tools svc) tools-hash)

    ;; Search for "file" with limit 2
    (let ((res (mcpkit-search-query "file" :limit 2 :include-proxies nil :active-services (list svc))))
      (should (= (length res) 2))
      (should (seq-every-p (lambda (t-match)
                             (string-match-p "file" (mcpkit-tool-name t-match)))
                           res)))

    ;; Search for "buffer"
    (let ((res (mcpkit-search-query "buffer" :limit 5 :include-proxies nil :active-services (list svc))))
      (should (= (length res) 1))
      (should (equal (mcpkit-tool-name (car res)) "close_buffer")))

    ;; Service filtering
    (let ((res-svc (mcpkit-search-query "file" :service 'other-svc :include-proxies nil :active-services (list svc))))
      (should (null res-svc)))))

(ert-deftest mcpkit-search-test-tool-collation ()
  "Test deduplication and instance collation of tools."
  (let ((local-t (make-mcpkit-tool :name "select" :description "Select text" :service 'core))
        (proxy-t1 (make-mcpkit-tool :name "select" :description "Select text" :service 'proxy:worker-1))
        (proxy-t2 (make-mcpkit-tool :name "select" :description "Select text" :service 'proxy:worker-2))
        (proxy-t3 (make-mcpkit-tool :name "grep" :description "Fast grep" :service 'proxy:worker-1)))
    (let ((collated (mcpkit-search--collate-tools (list local-t) (list proxy-t1 proxy-t2 proxy-t3))))
      ;; Should have exactly 2 unique tools: select and grep
      (should (= (length collated) 2))
      (let ((select-tool (seq-find (lambda (t0) (equal (mcpkit-tool-name t0) "select")) collated))
            (grep-tool (seq-find (lambda (t0) (equal (mcpkit-tool-name t0) "grep")) collated)))
        (should select-tool)
        (should grep-tool)
        ;; Description of select should mention available processes
        (should (string-match-p "\\[Available in processes: local, sprite:worker-1, sprite:worker-2\\]"
                                (mcpkit-tool-description select-tool)))
        ;; Single instance tool should not have multiple instance annotation
        (should-not (string-match-p "\\[Available in processes:"
                                    (mcpkit-tool-description grep-tool)))))))

(ert-deftest mcpkit-search-test-search-tools-handler ()
  "Test synthetic search_tools tool handler and schema."
  (let* ((svc (make-mcpkit-service--raw :name 'emacs))
         (tools-hash (make-hash-table :test #'equal))
         (t1 (make-mcpkit-tool
              :name "emacs_eval"
              :description "Evaluate Elisp expression"
              :input-schema '(:type "object" :properties (:expression (:type "string")))
              :service 'emacs)))
    (puthash "emacs_eval" t1 tools-hash)
    (setf (mcpkit-service-tools svc) tools-hash)

    (let ((mcpkit--active-services (list svc)))
      (let* ((search-tool (mcpkit-search-make-search-tool)))
        (should (equal (mcpkit-tool-name search-tool) "search_tools"))
        (should (functionp (mcpkit-tool-handler search-tool)))

        ;; Execute search_tools handler
        (let ((result (funcall (mcpkit-tool-handler search-tool)
                               '(:query "eval elisp" :limit 5 :include_proxies :json-false))))
          (should (listp result))
          (should (plist-get result :content))
          (let ((text (plist-get (aref (plist-get result :content) 0) :text)))
            (should (string-match-p "emacs_eval" text))
            (should (string-match-p "expression (string)" text)))
          ;; Structured descriptors are omitted by default.
          (should-not (plist-member result :tools))
          (let* ((mcpkit-search-structured-tools t)
                 (result2 (funcall (mcpkit-tool-handler search-tool)
                                   '(:query "eval elisp" :limit 5 :include_proxies :json-false)))
                 (tools-vec (plist-get result2 :tools)))
            (should (vectorp tools-vec))
            (should (= (length tools-vec) 1))
            (should (equal (plist-get (aref tools-vec 0) :name) "emacs_eval"))))))))

(defun test-mcpkit-search--invoke-fixture ()
  "Return an active-services list with an `echo_tool' handler."
  (let* ((svc (make-mcpkit-service--raw :name 'emacs))
         (tools-hash (make-hash-table :test #'equal))
         (tool (make-mcpkit-tool
                :name "echo_tool"
                :description "Echo text"
                :input-schema '(:type "object"
                                :properties (:text (:type "string"))
                                :required ["text"])
                :handler (lambda (args &optional done)
                           (let ((res (format "echo: %s" (plist-get args :text))))
                             (if done
                                 (funcall done nil res)
                               res)))
                :service 'emacs)))
    (setf (map-elt tools-hash "echo_tool") tool)
    (setf (mcpkit-service-tools svc) tools-hash)
    (list svc)))

(ert-deftest mcpkit-search-test-invoke-tool-success ()
  "invoke_tool dispatches to the named tool and returns its content."
  (let ((mcpkit--active-services (test-mcpkit-search--invoke-fixture)))
    (let ((res (mcpkit-search-invoke-tool-handler
                '(:name "echo_tool" :arguments (:text "hi")))))
      (should (equal (plist-get (car res) :text) "echo: hi")))
    ;; Async form via the tools/call path.
    (let (captured)
      (mcpkit--handle-tools-call
       1 '(:name "invoke_tool" :arguments (:name "echo_tool" :arguments (:text "yo")))
       mcpkit--active-services
       (lambda (status resp &rest _) (setq captured (cons status resp))))
      (should (eq (car captured) 'success))
      (should (equal (plist-get (aref (plist-get (plist-get (cdr captured) :result) :content) 0) :text)
                     "echo: yo")))))

(ert-deftest mcpkit-search-test-invoke-tool-unknown ()
  "invoke_tool errors on an unknown tool name."
  (let ((mcpkit--active-services (test-mcpkit-search--invoke-fixture)))
    (let ((err (should-error (mcpkit-search-invoke-tool-handler
                              '(:name "no_such_tool" :arguments nil)))))
      (should (string-match-p "Unknown tool: no_such_tool" (cadr err))))
    (should-error (mcpkit-search-invoke-tool-handler '(:arguments nil)))
    (should-error (mcpkit-search-invoke-tool-handler '(:name "invoke_tool")))))

(ert-deftest mcpkit-search-test-invoke-tool-bad-arguments ()
  "invoke_tool rejects non-object arguments and missing required params."
  (let ((mcpkit--active-services (test-mcpkit-search--invoke-fixture)))
    (let ((err (should-error (mcpkit-search-invoke-tool-handler
                              '(:name "echo_tool" :arguments "oops")))))
      (should (string-match-p "arguments must be an object" (cadr err))))
    (let ((err (should-error (mcpkit-search-invoke-tool-handler
                              '(:name "echo_tool" :arguments (:other 1))))))
      (should (string-match-p "missing required parameter: text" (cadr err))))))

(ert-deftest mcpkit-search-test-invoke-tool-definition-and-search-hint ()
  "invoke_tool is findable and search_tools results point to it."
  (let ((mcpkit--active-services (test-mcpkit-search--invoke-fixture)))
    (should (equal (mcpkit-tool-name (mcpkit--find-tool "invoke_tool" nil)) "invoke_tool"))
    (let* ((res (mcpkit-search-tools-handler '(:query "echo" :include_proxies :json-false)))
           (text (plist-get (aref (plist-get res :content) 0) :text)))
      (should (string-match-p "echo_tool" text))
      (should (string-match-p "invoke_tool" text)))))

(ert-deftest mcpkit-search-test-discover-services ()
  "discover_services returns active/registered services and tool counts."
  (let ((mcpkit--active-services (test-mcpkit-search--invoke-fixture)))
    (let ((res (mcpkit-search-discover-services-handler nil)))
      (should (plist-get res :services))
      (let* ((svcs (append (plist-get res :services) nil))
             (first-svc (car svcs)))
        (should (equal (plist-get first-svc :service) "emacs"))
        (should (= (plist-get first-svc :tool_count) 1))))))

(ert-deftest mcpkit-search-test-gated-tools-list-pinned-tools ()
  "Gated tools/list exposes search_tools, invoke_tool, and discover_services by default."
  (let* ((svc (make-mcpkit-service--raw
               :name 'test-svc
               :port 8765
               :tools (make-hash-table :test #'equal)))
         (mcpkit-search-mode 'auto)
         (mcpkit-search-threshold 0)
         (mcpkit-search-pinned-tools '("search_tools" "invoke_tool" "discover_services")))
    (dotimes (i 5)
      (setf (map-elt (mcpkit-service-tools svc) (format "tool_%d" i))
            (make-mcpkit-tool :name (format "tool_%d" i) :service 'test-svc)))
    (let* ((resp (mcpkit--handle-tools-list 1 (list (cons svc 'error))))
           (tools (plist-get (plist-get resp :result) :tools))
           (names (mapcar (lambda (td) (plist-get td :name)) (append tools nil))))
      (should (member "search_tools" names))
      (should (member "invoke_tool" names))
      (should (member "discover_services" names))
      (should-not (member "tool_0" names)))))

;;; Name resolution, aliases, pinning, output shape

(defun test-mcpkit-search--ns-fixture (&rest specs)
  "Build namespaced active services from SPECS of (SERVICE TOOL-NAME...)."
  (mapcar
   (lambda (spec)
     (let ((svc (make-mcpkit-service--raw :name (car spec)))
           (hash (make-hash-table :test #'equal)))
       (dolist (name (cdr spec))
         (puthash name
                  (make-mcpkit-tool
                   :name name :description (format "Tool %s" name)
                   :input-schema '(:type "object"
                                   :properties (:text (:type "string")
                                                :n (:type "integer"))
                                   :required ["text"])
                   :handler (lambda (args &optional done)
                              (let ((res (format "ran:%s" (plist-get args :text))))
                                (if done (funcall done nil res) res)))
                   :service (car spec))
                  hash))
       (setf (mcpkit-service-tools svc) hash)
       (cons svc 'namespace)))
   specs))

(ert-deftest mcpkit-search-test-resolve-exact-and-unprefixed ()
  "Exact and unambiguous unprefixed names resolve to the canonical name."
  (let ((mcpkit-tool-aliases nil)
        (svcs (test-mcpkit-search--ns-fixture '(emacs "emacs_get_buffer" "eval") '(org "org_get"))))
    (should (equal (mcpkit-resolve-tool-name "emacs__eval" svcs) "emacs__eval"))
    (should (equal (mcpkit-resolve-tool-name "eval" svcs) "emacs__eval"))
    (should (equal (mcpkit-resolve-tool-name "emacs_get_buffer" svcs) "emacs__emacs_get_buffer"))
    (should (equal (mcpkit-resolve-tool-name "org_get" svcs) "org__org_get"))
    (should-not (mcpkit-resolve-tool-name "nonexistent" svcs))))

(ert-deftest mcpkit-search-test-resolve-ambiguous ()
  "Ambiguous bare names resolve to nil and invoke_tool lists candidates."
  (let* ((mcpkit-tool-aliases nil)
         (svcs (test-mcpkit-search--ns-fixture '(a "read") '(b "read")))
         (mcpkit--active-services svcs))
    (should-not (mcpkit-resolve-tool-name "read" svcs))
    (should (equal (mcpkit-resolve-tool-name "b__read" svcs) "b__read"))
    (let ((err (should-error (mcpkit-search-invoke-tool-handler
                              '(:name "read" :arguments (:text "x"))))))
      (should (string-match-p "Ambiguous" (cadr err)))
      (should (string-match-p "a__read" (cadr err)))
      (should (string-match-p "b__read" (cadr err))))))

(ert-deftest mcpkit-search-test-resolve-aliases ()
  "Aliases resolve old names (bare or namespaced) but are not tools."
  (let ((mcpkit-tool-aliases nil)
        (svcs (test-mcpkit-search--ns-fixture '(emacs "eval" "emacs_list_buffers"))))
    (mcpkit-register-tool-alias "emacs_eval" "eval")
    (mcpkit-register-tool-alias "mcp_buffer_list" "emacs_list_buffers")
    (should (equal (mcpkit-resolve-tool-name "emacs_eval" svcs) "emacs__eval"))
    (should (equal (mcpkit-resolve-tool-name "emacs__emacs_eval" svcs) "emacs__eval"))
    (should (equal (mcpkit-resolve-tool-name "mcp_buffer_list" svcs) "emacs__emacs_list_buffers"))
    ;; Re-registering replaces rather than duplicating.
    (mcpkit-register-tool-alias "emacs_eval" "eval")
    (should (= 1 (seq-count (lambda (c) (equal (car c) "emacs_eval")) mcpkit-tool-aliases)))
    ;; Aliases never appear as tools.
    (let ((names (mapcar #'mcpkit-tool-name (mcpkit--normalize-tools svcs))))
      (should-not (member "emacs_eval" names)))
    (let ((mcpkit--active-services svcs))
      (let ((res (mcpkit-search-query "emacs_eval" :include-proxies nil)))
        (should-not (seq-some (lambda (tl) (equal (mcpkit-tool-name tl) "emacs_eval")) res))))))

(ert-deftest mcpkit-search-test-invoke-tool-resolves-names ()
  "invoke_tool and tools/call accept unprefixed and aliased names."
  (let* ((mcpkit-tool-aliases '(("old_echo" . "echo")))
         (svcs (test-mcpkit-search--ns-fixture '(emacs "echo")))
         (mcpkit--active-services svcs))
    (dolist (n '("emacs__echo" "echo" "old_echo" "emacs__old_echo"))
      (should (equal (plist-get (car (mcpkit-search-invoke-tool-handler
                                      (list :name n :arguments '(:text "hi"))))
                                :text)
                     "ran:hi")))
    ;; Direct tools/call resolves too, and reports the canonical name.
    (let (captured)
      (mcpkit--handle-tools-call
       1 '(:name "old_echo" :arguments (:text "yo")) svcs
       (lambda (status _resp tname &rest _) (setq captured (list status tname))))
      (should (equal captured '(success "emacs__echo"))))
    (let (captured)
      (mcpkit--handle-tools-call
       1 '(:name "nope" :arguments nil) svcs
       (lambda (status _resp tname &rest _) (setq captured (list status tname))))
      (should (equal captured '(error "nope"))))))

(ert-deftest mcpkit-search-test-pinning-matches-bare-and-aliased ()
  "Pinned names match namespaced, bare, and aliased tool names."
  (let* ((mcpkit-search-mode 'always)
         (mcpkit-tool-aliases '(("old_get" . "emacs_get_buffer")))
         (svcs (test-mcpkit-search--ns-fixture
                '(emacs "emacs_get_buffer" "eval" "emacs_open") '(org "org_get")))
         (names-for
          (lambda (pinned &optional services)
            (let ((mcpkit-search-pinned-tools pinned)
                  (mcpkit-search-pinned-services services))
              (sort (seq-remove
                     (lambda (n) (member n '("search_tools" "invoke_tool" "discover_services")))
                     (mapcar (lambda (td) (plist-get td :name))
                             (append (plist-get (plist-get (mcpkit--handle-tools-list 1 svcs) :result)
                                                :tools)
                                     nil)))
                    #'string<)))))
    (should (equal (funcall names-for '("emacs_get_buffer")) '("emacs__emacs_get_buffer")))
    (should (equal (funcall names-for '("emacs__emacs_get_buffer")) '("emacs__emacs_get_buffer")))
    (should (equal (funcall names-for '("old_get")) '("emacs__emacs_get_buffer")))
    (should (equal (funcall names-for '("search_tools")) nil))
    (should (equal (funcall names-for '("search_tools") '(org)) '("org__org_get")))
    (should (equal (funcall names-for '("search_tools") '(emacs))
                   '("emacs__emacs_get_buffer" "emacs__emacs_open" "emacs__eval")))))

(ert-deftest mcpkit-search-test-search-output-parameters-and-hint ()
  "Text output renders parameters compactly; zero results show the hint."
  (let* ((svcs (test-mcpkit-search--ns-fixture '(emacs "echo")))
         (mcpkit--active-services svcs)
         (res (mcpkit-search-tools-handler '(:query "echo" :include_proxies :json-false)))
         (text (plist-get (aref (plist-get res :content) 0) :text)))
    (should (string-match-p "text (string, required)" text))
    (should (string-match-p "n (integer)" text))
    (should-not (string-match-p "n (integer, required)" text))
    (should-not (plist-member res :tools))
    (let* ((none (mcpkit-search-tools-handler '(:query "zzzzqq" :include_proxies :json-false)))
           (ntext (plist-get (aref (plist-get none :content) 0) :text)))
      (should (string-match-p "\\`No matching tool\\." ntext))
      (should (string-match-p "emacs__eval" ntext)))
    (let* ((mcpkit-search-no-match-hint "Try elsewhere.")
           (none (mcpkit-search-tools-handler '(:query "zzzzqq" :include_proxies :json-false)))
           (ntext (plist-get (aref (plist-get none :content) 0) :text)))
      (should (string-match-p "Try elsewhere\\." ntext))
      (should-not (string-match-p "emacs__eval" ntext)))))

;;; Usage pinning and catalog

(defmacro test-mcpkit-search--with-usage (svcs &rest body)
  "Run BODY with SVCS active, a private stats table and no usage snapshots."
  (declare (indent 1))
  `(let ((mcpkit-stats--table (make-hash-table :test #'equal))
         (mcpkit-stats-file nil)
         (mcpkit--usage-pins nil)
         (mcpkit--catalog-snapshot nil)
         (mcpkit-search-mode 'always)
         (mcpkit-search-pinned-tools nil)
         (mcpkit-search-pinned-services nil)
         (mcpkit-search-pinned-by-usage t)
         (mcpkit-search-pinned-usage-limit 8)
         (mcpkit-search-pinned-usage-min-calls 5)
         (mcpkit--active-services ,svcs))
     ,@body))

(defun test-mcpkit-search--calls (name n)
  "Record N successful calls of tool NAME."
  (dotimes (_ n) (mcpkit-stats-record-call name 'success 1.0)))

(defun test-mcpkit-search--listed (svcs)
  "Return sorted non-gateway tool names exposed by gated tools/list for SVCS."
  (sort (seq-remove
         (lambda (n) (member n mcpkit--gateway-tool-names))
         (mapcar (lambda (td) (plist-get td :name))
                 (append (plist-get (plist-get (mcpkit--handle-tools-list 1 svcs) :result)
                                    :tools)
                         nil)))
        #'string<))

(ert-deftest mcpkit-search-test-usage-pins-basic ()
  "Used tools are pinned by score; thresholds, limit and exclusions apply."
  (let ((svcs (test-mcpkit-search--ns-fixture
               '(emacs "eval" "emacs_open" "emacs_rare") '(org "org_get" "org_set" "org_cold"))))
    (test-mcpkit-search--with-usage svcs
      ;; No data: nothing extra pinned.
      (should (equal (test-mcpkit-search--listed svcs) nil))
      (mcpkit--invalidate-snapshots)
      (test-mcpkit-search--calls "emacs__eval" 20)
      (test-mcpkit-search--calls "emacs__emacs_open" 7)
      (test-mcpkit-search--calls "org__org_get" 9)
      (test-mcpkit-search--calls "org__org_set" 5)
      (test-mcpkit-search--calls "emacs__emacs_rare" 4)
      (let ((mcpkit-search-pinned-usage-limit 3))
        (should (equal (mcpkit--compute-usage-pins (mcpkit--normalize-tools svcs))
                       '("org__org_get" "emacs__emacs_open" "org__org_set"))))
      (should (equal (test-mcpkit-search--listed svcs)
                     '("emacs__emacs_open" "org__org_get" "org__org_set")))
      (should (equal (mcpkit-search-pin-kind "org__org_get" svcs) "usage"))
      (should-not (mcpkit-search-pin-kind "emacs__eval" svcs))
      (should-not (mcpkit-search-pin-kind "emacs__emacs_rare" svcs)))))

(ert-deftest mcpkit-search-test-usage-pins-options ()
  "Disabling, exclusion, hidden tools, unknown tools and static precedence."
  (let ((svcs (test-mcpkit-search--ns-fixture '(emacs "emacs_open" "emacs_x") '(org "org_get"))))
    (test-mcpkit-search--with-usage svcs
      (test-mcpkit-search--calls "emacs__emacs_open" 6)
      (test-mcpkit-search--calls "org__org_get" 6)
      (let ((mcpkit-search-pinned-by-usage nil))
        (should (equal (test-mcpkit-search--listed svcs) nil)))
      (mcpkit--invalidate-snapshots)
      (let ((mcpkit-search-pinned-usage-exclude '("emacs_open")))
        (should (equal (test-mcpkit-search--listed svcs) '("org__org_get"))))
      (mcpkit--invalidate-snapshots)
      (let ((mcpkit-stats-hidden-tools '("org_get")))
        (should (equal (test-mcpkit-search--listed svcs) '("emacs__emacs_open"))))
      (mcpkit--invalidate-snapshots)
      (let ((mcpkit-search-pinned-tools '("org_get"))
            (mcpkit-search-pinned-services '(emacs)))
        (should (equal (mcpkit-search-pin-kind "org__org_get" svcs) "static"))
        (should (equal (mcpkit-search-pin-kind "emacs__emacs_open" svcs) "service"))
        (should-not (mcpkit-search-pin-kind "nope" svcs))))))

(ert-deftest mcpkit-search-test-usage-pins-are-a-stable-snapshot ()
  "The pin set is computed once; refresh and service changes recompute it."
  (let ((svcs (test-mcpkit-search--ns-fixture '(emacs "emacs_a" "emacs_b"))))
    (test-mcpkit-search--with-usage svcs
      (test-mcpkit-search--calls "emacs__emacs_a" 6)
      (should (equal (test-mcpkit-search--listed svcs) '("emacs__emacs_a")))
      (let ((desc-before (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))))
        (test-mcpkit-search--calls "emacs__emacs_b" 50)
        (should (equal (test-mcpkit-search--listed svcs) '("emacs__emacs_a")))
        (should (equal (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))
                       desc-before))
        (should (equal (mcpkit-search-refresh-usage-pins)
                       '("emacs__emacs_b" "emacs__emacs_a")))
        (should (equal (test-mcpkit-search--listed svcs)
                       '("emacs__emacs_a" "emacs__emacs_b")))
        (should-not (equal (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))
                           desc-before))
        (mcpkit--invalidate-snapshots)
        (should-not mcpkit--usage-pins)
        (should-not mcpkit--catalog-snapshot)))))

(ert-deftest mcpkit-search-test-start-stop-invalidates-snapshots ()
  "Starting or stopping a service drops the snapshots."
  (let* ((svc (make-mcpkit-service--raw :name 'snapsvc))
         (mcpkit--active-services nil)
         (mcpkit--active-server 'fake)
         (mcpkit--usage-pins '(x . ("a")))
         (mcpkit--catalog-snapshot '(x . "c")))
    (mcpkit-start-service svc)
    (should-not mcpkit--usage-pins)
    (should-not mcpkit--catalog-snapshot)
    (setq mcpkit--usage-pins '(x . ("a")))
    (let ((mcpkit--active-server nil))
      (mcpkit-stop-service svc))
    (should-not mcpkit--usage-pins)))

(defun test-mcpkit-search--described-fixture ()
  "Return fixture services with descriptions for catalog tests."
  (let ((svcs (test-mcpkit-search--ns-fixture
               '(emacs "eval" "emacs_a" "emacs_b" "emacs_c" "emacs_d") '(org "org_get"))))
    (setf (mcpkit-service-description (car (nth 0 svcs))) "Editor tools")
    (setf (mcpkit-service-description (car (nth 1 svcs))) "Org mode tools")
    svcs))

(ert-deftest mcpkit-search-test-catalog-in-description ()
  "The description lists services, counts, descriptions and popular tools."
  (let ((svcs (test-mcpkit-search--described-fixture)))
    (test-mcpkit-search--with-usage svcs
      (let ((desc (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))))
        (should (string-prefix-p "Search available MCP tools" desc))
        (should (string-match-p "^- emacs (5 tools): Editor tools$" desc))
        (should (string-match-p "^- org (1 tools): Org mode tools$" desc))
        (should-not (string-match-p "popular" desc))
        (should (string-suffix-p "search_tools with `service' and an empty query lists every tool in that service."
                                 desc)))
      (mcpkit--invalidate-snapshots)
      (test-mcpkit-search--calls "emacs__emacs_a" 5)
      (test-mcpkit-search--calls "emacs__emacs_b" 4)
      (test-mcpkit-search--calls "emacs__emacs_c" 3)
      (test-mcpkit-search--calls "emacs__emacs_d" 2)
      (test-mcpkit-search--calls "emacs__eval" 1)
      (test-mcpkit-search--calls "search_tools" 9)
      (let ((desc (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))))
        (should (string-match-p
                 "^- emacs (5 tools): Editor tools; popular: emacs_a, emacs_b, emacs_c$" desc))
        (should-not (string-match-p "search_tools (" desc))
        (should-not (string-match-p "^- org.*popular" desc))
        ;; tools/list and find-tool agree.
        (let* ((resp (mcpkit--handle-tools-list 1 svcs))
               (listed (seq-find (lambda (td) (equal (plist-get td :name) "search_tools"))
                                 (append (plist-get (plist-get resp :result) :tools) nil))))
          (should (equal (plist-get listed :description) desc))))
      (mcpkit--invalidate-snapshots)
      (let ((mcpkit-search-catalog-in-description nil))
        (should (equal (mcpkit-tool-description (mcpkit--find-tool "search_tools" svcs))
                       "Search available MCP tools using keywords, names, descriptions, or parameters."))))))

(ert-deftest mcpkit-search-test-catalog-cap ()
  "Popular lists are dropped first, then descriptions truncated."
  (let* ((svcs (test-mcpkit-search--ns-fixture
                '(a "t1" "t2") '(b "t3") '(c "t4") '(d "t5"))))
    (dolist (e svcs)
      (setf (mcpkit-service-description (car e)) (make-string 600 ?x)))
    (test-mcpkit-search--with-usage svcs
      (test-mcpkit-search--calls "a__t1" 3)
      (let* ((catalog (mcpkit-search--catalog svcs)))
        (should (<= (length catalog) 1500))
        (should-not (string-match-p "popular" catalog))
        (should (string-match-p "\\.\\.\\." catalog))
        (should (= (length (split-string catalog "\n")) 4))))
    (let* ((svcs2 (test-mcpkit-search--ns-fixture '(a "t1" "t2")))
           (mcpkit--catalog-snapshot nil))
      (setf (mcpkit-service-description (car (car svcs2))) (make-string 1490 ?y))
      (let ((mcpkit-stats--table (make-hash-table :test #'equal))
            (mcpkit--active-services svcs2))
        (test-mcpkit-search--calls "a__t1" 3)
        (let ((catalog (mcpkit-search--catalog svcs2)))
          (should (<= (length catalog) 1500))
          (should-not (string-match-p "popular" catalog)))))))

(ert-deftest mcpkit-search-test-empty-query-with-service-lists-service ()
  "An empty query with a service filter returns that service's tools."
  (let ((svcs (test-mcpkit-search--ns-fixture
               '(emacs "emacs_a" "emacs_b" "emacs_c" "emacs_d" "emacs_e" "emacs_f" "emacs_g")
               '(org "org_get"))))
    (let ((mcpkit--active-services svcs))
      (should (= (length (mcpkit-search-query "" :service 'org :include-proxies nil)) 1))
      (should (= (length (mcpkit-search-query "" :service "emacs" :limit 3 :include-proxies nil)) 3))
      (should (= (length (mcpkit-search-query "" :service "emacs" :limit 50 :include-proxies nil)) 7))
      ;; The handler lists the whole service when no limit is given.
      (let* ((res (mcpkit-search-tools-handler
                   '(:query "" :service "emacs" :include_proxies :json-false)))
             (text (plist-get (aref (plist-get res :content) 0) :text)))
        (should (string-match-p "Found 7 matching tools" text))
        (should-not (string-match-p "org__org_get" text)))
      ;; Without a service the default limit still applies.
      (let* ((res (mcpkit-search-tools-handler '(:query "" :include_proxies :json-false)))
             (text (plist-get (aref (plist-get res :content) 0) :text)))
        (should (string-match-p "Found 5 matching tools" text))))))

(provide 'test-mcpkit-search)

;;; test-mcpkit-search.el ends here
