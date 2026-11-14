;;; test-mcpkit-search-integration.el --- Integration tests for search, gating, and stats -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, search, integration

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-search)
(require 'mcpkit-stats)

(ert-deftest mcpkit-search-test-e2e-gating-and-stats-over-http ()
  "Test end-to-end search transform gating, HTTP search_tools call, and stats tracking."
  (let ((mcpkit-registry nil)
        (mcpkit--active-services nil)
        (mcpkit--active-server nil)
        (mcpkit-log-verbose nil)
        (server-port 18888)
        (mcpkit-search-mode 'auto)
        (mcpkit-search-threshold 2)
        (mcpkit-search-pinned-tools '("pinned_tool" "search_tools")))
    (mcpkit-stats-reset)
    (unwind-protect
        (progn
          ;; Define a service with 4 tools (exceeding threshold 2)
          (let ((svc (mcpkit-define-service 'search-e2e-svc :port server-port)))
            (mcpkit-register-tool pinned_tool svc
              :description "Always visible pinned tool"
              "pinned-result")
            (mcpkit-register-tool buffer_read svc
              :description "Read buffer contents"
              :input-schema '(:type "object" :properties (:file (:type "string")))
              (format "read:%s" (plist-get args :file)))
            (mcpkit-register-tool buffer_write svc
              :description "Write buffer contents"
              :input-schema '(:type "object" :properties (:file (:type "string") :text (:type "string")))
              (format "wrote:%s" (plist-get args :file)))
            (mcpkit-register-tool calculate_sum svc
              :description "Sum two integers"
              :input-schema '(:type "object" :properties (:a (:type "integer") :b (:type "integer")))
              (+ (or (plist-get args :a) 0) (or (plist-get args :b) 0)))

            (mcpkit-start-service 'search-e2e-svc :on-collision 'error))

          ;; 1. Request tools/list over HTTP: should be gated to pinned_tool + search_tools
          (let* ((url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data
                  (encode-coding-string
                   "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}"
                   'utf-8))
                 (buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/mcp" server-port) t t 5)))
            (should buf)
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (re-search-forward "\r?\n\r?\n" nil t))
                  (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                         (parsed (mcpkit--parse-request json-str))
                         (res (plist-get parsed :result))
                         (tools (plist-get res :tools))
                         (names (mapcar (lambda (td) (plist-get td :name)) (append tools nil))))
                    (should (= (length tools) 2))
                    (should (member "pinned_tool" names))
                    (should (member "search_tools" names))
                    (should-not (member "buffer_read" names))
                    (should-not (member "calculate_sum" names))))
              (kill-buffer buf)))

          ;; 2. Call search_tools over HTTP looking for "buffer"
          (let* ((url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data
                  (encode-coding-string
                   "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"search_tools\",\"arguments\":{\"query\":\"buffer\",\"limit\":5}}}"
                   'utf-8))
                 (buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/mcp" server-port) t t 5)))
            (should buf)
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (re-search-forward "\r?\n\r?\n" nil t))
                  (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                         (parsed (mcpkit--parse-request json-str))
                         (res (plist-get parsed :result))
                         (content (plist-get res :content))
                         (text (plist-get (aref content 0) :text)))
                    (should (string-match-p "Found 2 matching tools" text))
                    (should (string-match-p "buffer_read" text))
                    (should (string-match-p "buffer_write" text))))
              (kill-buffer buf)))

          ;; 3. Direct HTTP call to unlisted tool "buffer_read" succeeds
          (let* ((url-request-method "POST")
                 (url-request-extra-headers '(("Content-Type" . "application/json")))
                 (url-request-data
                  (encode-coding-string
                   "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"buffer_read\",\"arguments\":{\"file\":\"foo.txt\"}}}"
                   'utf-8))
                 (buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/mcp" server-port) t t 5)))
            (should buf)
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (should (re-search-forward "\r?\n\r?\n" nil t))
                  (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                         (parsed (mcpkit--parse-request json-str))
                         (res (plist-get parsed :result))
                         (content (plist-get res :content))
                         (text (plist-get (aref content 0) :text)))
                    (should (equal text "read:foo.txt"))))
              (kill-buffer buf)))

          ;; 4. Verify telemetry in mcpkit-stats reflects HTTP calls
          (let ((rec-search (mcpkit-stats-get "search_tools"))
                (rec-buf-read (mcpkit-stats-get "buffer_read")))
            (should (>= (mcpkit-stats-record-call-count rec-search) 1))
            (should (= (mcpkit-stats-record-success-count rec-search) 1))
            (should (>= (mcpkit-stats-record-call-count rec-buf-read) 1))
            (should (= (mcpkit-stats-record-success-count rec-buf-read) 1))
            (should (> (mcpkit-stats-record-total-duration-ms rec-buf-read) 0.0))))

      ;; Cleanup
      (mcpkit-stop-service 'search-e2e-svc))))

(provide 'test-mcpkit-search-integration)

;;; test-mcpkit-search-integration.el ends here
