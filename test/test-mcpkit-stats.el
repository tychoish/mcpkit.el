;;; test-mcpkit-stats.el --- Tests for mcpkit-stats -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, stats

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-stats)

(ert-deftest mcpkit-stats-test-record-lifecycle ()
  "Test stats retrieval, update, and reset."
  (mcpkit-stats-reset)
  (let ((rec (mcpkit-stats-get "test_tool")))
    (should (equal (mcpkit-stats-record-tool-name rec) "test_tool"))
    (should (= (mcpkit-stats-record-call-count rec) 0))
    (should (= (mcpkit-stats-record-success-count rec) 0))
    (should (= (mcpkit-stats-record-error-count rec) 0))

    ;; Record successful call
    (mcpkit-stats-record-call "test_tool" 'success 42.5 nil '(:arg "val"))
    (should (= (mcpkit-stats-record-call-count rec) 1))
    (should (= (mcpkit-stats-record-success-count rec) 1))
    (should (= (mcpkit-stats-record-error-count rec) 0))
    (should (= (mcpkit-stats-record-last-duration-ms rec) 42.5))
    (should (= (mcpkit-stats-record-min-duration-ms rec) 42.5))
    (should (= (mcpkit-stats-record-max-duration-ms rec) 42.5))
    (should (stringp (mcpkit-stats-record-last-called-at rec)))

    ;; Record error call
    (mcpkit-stats-record-call "test_tool" 'error 10.0 "Invalid input" '(:bad "arg"))
    (should (= (mcpkit-stats-record-call-count rec) 2))
    (should (= (mcpkit-stats-record-success-count rec) 1))
    (should (= (mcpkit-stats-record-error-count rec) 1))
    (should (= (mcpkit-stats-record-min-duration-ms rec) 10.0))
    (should (= (mcpkit-stats-record-max-duration-ms rec) 42.5))

    ;; Check error ring
    (let ((ring (mcpkit-stats-record-recent-errors rec)))
      (should (= (ring-length ring) 1))
      (let ((item (ring-ref ring 0)))
        (should (equal (plist-get item :message) "Invalid input"))
        (should (equal (plist-get item :args) '(:bad "arg")))))

    ;; Reset specific tool
    (mcpkit-stats-reset "test_tool")
    (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "test_tool")) 0))))

(ert-deftest mcpkit-stats-test-dispatcher-hook-integration ()
  "Test that executing tools via mcpkit--handle-tools-call triggers stats update."
  (mcpkit-stats-reset)
  (let* ((svc (make-mcpkit-service--raw :name 'math))
         (tools-hash (make-hash-table :test #'equal))
         (t1 (make-mcpkit-tool
              :name "double"
              :description "Double a number"
              :input-schema '(:type "object"
                              :properties (:n (:type "number"))
                              :required ["n"])
              :handler (lambda (args cb) (funcall cb (* 2 (plist-get args :n))))
              :service 'math)))
    (puthash "double" t1 tools-hash)
    (setf (mcpkit-service-tools svc) tools-hash)

    (let ((mcpkit--active-services (list svc)))
      ;; Successful invocation
      (let ((done-called nil))
        (mcpkit--handle-tools-call
         1
         '(:name "double" :arguments (:n 21))
         (list svc)
         (lambda (status resp tool-name dur msg &rest _)
           (setq done-called t)
           (should (eq status 'success))))
        (should done-called)
        (let ((rec (mcpkit-stats-get "double")))
          (should (= (mcpkit-stats-record-call-count rec) 1))
          (should (= (mcpkit-stats-record-success-count rec) 1))
          (should (eq (mcpkit-stats-record-service rec) 'math))))

      ;; Failing invocation (missing required param)
      (let ((done-called nil))
        (mcpkit--handle-tools-call
         2
         '(:name "double" :arguments (:wrong 42))
         (list svc)
         (lambda (status resp tool-name dur msg &rest _)
           (setq done-called t)
           (should (eq status 'error))))
        (should done-called)
        (let ((rec (mcpkit-stats-get "double")))
          (should (= (mcpkit-stats-record-call-count rec) 2))
          (should (= (mcpkit-stats-record-error-count rec) 1))
          (should (string-match-p "missing required parameter"
                                  (plist-get (ring-ref (mcpkit-stats-record-recent-errors rec) 0) :message))))))))

(ert-deftest mcpkit-stats-test-tabulated-entries ()
  "Test tabulated list entries formatting."
  (mcpkit-stats-reset)
  (mcpkit-stats-record-call "tool_a" 'success 100.0)
  (mcpkit-stats-record-call "tool_a" 'error 200.0 "fail")
  (let ((mcpkit--active-services nil))
    (let ((entries (mcpkit-stats-tabulated-entries)))
      (should (= (length entries) 1))
      (let* ((entry (car entries))
             (id (car entry))
             (vec (cadr entry)))
        (should (equal id "tool_a"))
        (should (equal (aref vec 0) "tool_a"))
        (should (equal (aref vec 2) "2")) ;; calls
        (should (equal (aref vec 3) "1")) ;; succ
        (should (equal (aref vec 4) "1")) ;; err
        (should (equal (aref vec 5) "50.0%")) ;; err pct
        (should (equal (aref vec 6) "150.00")))))) ;; avg dur

(ert-deftest mcpkit-stats-test-detail-rendering ()
  "Test rendering tool detail buffer."
  (mcpkit-stats-reset)
  (mcpkit-stats-record-call "demo_tool" 'error 75.0 "Connection timeout" '(:timeout 5))
  (with-temp-buffer
    (mcpkit-stats-render-detail "demo_tool")
    (let ((content (buffer-string)))
      (should (string-match-p "Tool: demo_tool" content))
      (should (string-match-p "Total Invocations: 1" content))
      (should (string-match-p "Failed Calls:      1 (100.0% error rate)" content))
      (should (string-match-p "Connection timeout" content))
      (should (string-match-p "arguments: (:timeout 5)" content)))))

(provide 'test-mcpkit-stats)

;;; test-mcpkit-stats.el ends here
