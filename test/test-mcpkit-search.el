;;; test-mcpkit-search.el --- Tests for mcpkit-search -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, search

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-search)

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
            (should (string-match-p "emacs_eval" text)))
          (let ((tools-vec (plist-get result :tools)))
            (should (vectorp tools-vec))
            (should (= (length tools-vec) 1))
            (should (equal (plist-get (aref tools-vec 0) :name) "emacs_eval"))))))))

(provide 'test-mcpkit-search)

;;; test-mcpkit-search.el ends here
