;;; test-mcpkit-stats.el --- Tests for mcpkit-stats -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, stats

(require 'ert)
(require 'mcpkit)
(require 'mcpkit-stats)

(defconst mcpkit-stats-test--tool-names
  '("test_tool" "tool_a" "demo_tool" "real_tool" "emacs__eval" "emacs_eval" "digest_tool")
  "Tool names registered in the fixture service used by isolated tests.")

(defun mcpkit-stats-test--services ()
  "Return active services exposing `mcpkit-stats-test--tool-names'."
  (let ((svc (make-mcpkit-service--raw :name 'fixture))
        (hash (make-hash-table :test #'equal)))
    (dolist (name mcpkit-stats-test--tool-names)
      (puthash name (make-mcpkit-tool :name name :description "fixture" :service 'fixture) hash))
    (setf (mcpkit-service-tools svc) hash)
    (list (cons svc 'error))))

(defmacro mcpkit-stats-test--isolated (&rest body)
  "Run BODY with a private stats table so the live session's stats are untouched.
Active services expose fixture tools so calls to them are recorded."
  (declare (indent 0))
  `(let ((mcpkit-stats--table (make-hash-table :test #'equal))
         (mcpkit--active-services (mcpkit-stats-test--services)))
     ,@body))

(ert-deftest mcpkit-stats-test-record-lifecycle ()
  "Test stats retrieval, update, and reset."
  (mcpkit-stats-test--isolated
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
      (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "test_tool")) 0)))))

(ert-deftest mcpkit-stats-test-dispatcher-hook-integration ()
  "Test that executing tools via mcpkit--handle-tools-call triggers stats update."
  (mcpkit-stats-test--isolated
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
                                    (plist-get (ring-ref (mcpkit-stats-record-recent-errors rec) 0) :message)))))))))

(ert-deftest mcpkit-stats-test-tabulated-entries ()
  "Test tabulated list entries formatting."
  (mcpkit-stats-test--isolated
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
          (should (equal (aref vec 6) "150.00"))))))) ;; avg dur

(ert-deftest mcpkit-stats-test-detail-rendering ()
  "Test rendering tool detail buffer."
  (mcpkit-stats-test--isolated
    (mcpkit-stats-reset)
    (mcpkit-stats-record-call "demo_tool" 'error 75.0 "Connection timeout" '(:timeout 5))
    (with-temp-buffer
      (mcpkit-stats-render-detail "demo_tool")
      (let ((content (buffer-string)))
        (should (string-match-p "Tool: demo_tool" content))
        (should (string-match-p "Total Invocations: 1" content))
        (should (string-match-p "Failed Calls:      1 (100.0% error rate)" content))
        (should (string-match-p "Connection timeout" content))
        (should (string-match-p "arguments: (:timeout 5)" content))))))

(ert-deftest mcpkit-stats-test-hidden-tools-omitted-from-list ()
  "Gateway tools are recorded but not listed."
  (mcpkit-stats-test--isolated
    (mcpkit-stats-record-call "invoke_tool" 'success 1.0)
    (mcpkit-stats-record-call "real_tool" 'success 1.0)
    (let ((mcpkit--active-services nil))
      (should (equal (mapcar #'car (mcpkit-stats-tabulated-entries)) '("real_tool")))
      (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "invoke_tool")) 1))
      (let ((mcpkit-stats-hidden-tools nil))
        (should (= (length (mcpkit-stats-tabulated-entries)) 2))))))

(ert-deftest mcpkit-stats-test-unknown-tool-not-recorded ()
  "Calls to tools absent from the active services create no record."
  (mcpkit-stats-test--isolated
    (mcpkit-stats-record-call "nonexistent" 'error 1.0 "Tool not found")
    (should-not (gethash "nonexistent" mcpkit-stats--table))
    ;; Synthetic gateway tools are still recorded.
    (mcpkit-stats-record-call "invoke_tool" 'success 1.0)
    (should (gethash "invoke_tool" mcpkit-stats--table))))

(ert-deftest mcpkit-stats-test-eval-digest ()
  "The eval digester returns the head of the first real call."
  (let ((d (lambda (expr) (mcpkit-stats--eval-digest (list :expression expr)))))
    (should (equal (funcall d "(+ 1 2)") "+"))
    (should (equal (funcall d "(sprite-create :name \"x\")") "sprite-create"))
    (should (equal (funcall d "(progn (setq x 1) (sprite-create 1))") "sprite-create"))
    (should (equal (funcall d "(let ((a 1)) (if a 1 2) (my-fn a))") "my-fn"))
    (should (equal (funcall d "(let* ((a 1)) (my-fn a))") "my-fn"))
    (should (equal (funcall d "(condition-case e (do-it) (error nil))") "do-it"))
    (should (equal (funcall d "(unwind-protect (do-it) (cleanup))") "do-it"))
    (should (equal (funcall d "(progn (let ((a 1)) (deep a)))") "deep"))
    (should (equal (funcall d "(setq x 1)") "setq"))
    (should (equal (funcall d "(unbalanced") "<unparsed>"))
    (should (equal (mcpkit-stats--eval-digest '(:other 1)) "<unparsed>"))))

(ert-deftest mcpkit-stats-test-digests-recorded-and-rendered ()
  "Digests are counted per tool, shown in detail, and errors never break recording."
  (mcpkit-stats-test--isolated
    (dolist (e '("(sprite-create 1)" "(sprite-create 2)" "(buffer-list)" "(oops"))
      (mcpkit-stats-record-call "emacs__eval" 'success 1.0 nil (list :expression e)))
    (mcpkit-stats-record-call "emacs_eval" 'success 1.0 nil (list :expression "(foo)"))
    (let* ((rec (gethash "emacs__eval" mcpkit-stats--table))
           (digests (mcpkit-stats-record-digests rec)))
      (should (= (gethash "sprite-create" digests) 2))
      (should (= (gethash "buffer-list" digests) 1))
      (should (= (gethash "<unparsed>" digests) 1)))
    (should (= 1 (gethash "foo" (mcpkit-stats-record-digests
                                 (gethash "emacs_eval" mcpkit-stats--table)))))
    (with-temp-buffer
      (mcpkit-stats-render-detail "emacs__eval")
      (should (string-match-p "sprite-create" (buffer-string)))
      (should (string-match-p "sprite-create.*2\\|2.*sprite-create" (buffer-string))))
    ;; A failing digester must not prevent counting.
    (let ((mcpkit-stats-argument-digesters
           (list (cons "digest_tool" (lambda (_) (error "boom"))))))
      (mcpkit-stats-record-call "digest_tool" 'success 1.0 nil '(:a 1))
      (should (= 1 (mcpkit-stats-record-call-count (gethash "digest_tool" mcpkit-stats--table))))
      (should (= 0 (hash-table-count
                    (mcpkit-stats-record-digests (gethash "digest_tool" mcpkit-stats--table))))))
    ;; Custom digester
    (let ((mcpkit-stats-argument-digesters
           (list (cons "digest_tool" (lambda (a) (plist-get a :kind))))))
      (mcpkit-stats-record-call "digest_tool" 'success 1.0 nil '(:kind "k1"))
      (should (= 1 (gethash "k1" (mcpkit-stats-record-digests
                                  (gethash "digest_tool" mcpkit-stats--table))))))))

(ert-deftest mcpkit-stats-test-rows ()
  "`mcpkit-stats-rows' returns structured rows honoring hidden tools, tool and limit."
  (mcpkit-stats-test--isolated
    (mcpkit-stats-record-call "invoke_tool" 'success 1.0)
    (mcpkit-stats-record-call "real_tool" 'success 10.0)
    (mcpkit-stats-record-call "real_tool" 'error 20.0 "bad")
    (mcpkit-stats-record-call "test_tool" 'success 5.0)
    (mcpkit-stats-record-call "emacs__eval" 'success 1.0 nil '(:expression "(foo)"))
    (let ((rows (mcpkit-stats-rows)))
      (should-not (seq-some (lambda (r) (equal (plist-get r :tool) "invoke_tool")) rows))
      (should (equal (plist-get (car rows) :tool) "real_tool"))
      (should (= (plist-get (car rows) :calls) 2))
      (should (= (plist-get (car rows) :errors) 1))
      (should (= (plist-get (car rows) :avg_ms) 15.0))
      (should (equal (plist-get (car rows) :service) "fixture")))
    (should (= (length (mcpkit-stats-rows nil 1)) 1))
    (let ((rows (mcpkit-stats-rows "emacs__eval")))
      (should (= (length rows) 1))
      (should (equal (plist-get (aref (plist-get (car rows) :top_digests) 0) :digest) "foo")))
    (should-not (mcpkit-stats-rows "nope"))
    (let ((mcpkit-stats-hidden-tools nil))
      (should (seq-some (lambda (r) (equal (plist-get r :tool) "invoke_tool"))
                        (mcpkit-stats-rows))))))

;;; Persistence and scoring

(defmacro mcpkit-stats-test--persisted (&rest body)
  "Run BODY with a private table treated as canonical and a temp stats file.
The file is bound to `file' and deleted afterwards; timers are cancelled."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "mcpkit-stats-test" t))
          (file (expand-file-name "sub/stats.eld" dir))
          (mcpkit-stats-file file)
          (mcpkit-stats--table (make-hash-table :test #'equal))
          (mcpkit-stats--canonical-table mcpkit-stats--table)
          (mcpkit-stats--dirty nil)
          (mcpkit-stats--last-saved nil)
          (mcpkit-stats--synced-stamp nil)
          (mcpkit-stats--timer nil)
          (mcpkit-stats-half-life-days 14)
          (mcpkit--active-services (mcpkit-stats-test--services)))
     (unwind-protect (progn ,@body)
       (when (timerp mcpkit-stats--timer) (cancel-timer mcpkit-stats--timer))
       (delete-directory dir t))))

(defun mcpkit-stats-test--read-file (file)
  "Return the form stored in FILE."
  (with-temp-buffer (insert-file-contents file) (read (current-buffer))))

(ert-deftest mcpkit-stats-test-save-format-and-roundtrip ()
  "Saved file is a versioned form with mode 0600 and no temp leftovers."
  (mcpkit-stats-test--persisted
    (mcpkit-stats-record-call "test_tool" 'success 10.0)
    (mcpkit-stats-record-call "test_tool" 'error 30.0 "bad")
    (mcpkit-stats-record-call "emacs__eval" 'success 1.0 nil '(:expression "(buffer-list)"))
    (should (equal (mcpkit-stats-save) file))
    (should (= (file-modes file) #o600))
    (should (equal (directory-files (file-name-directory file) nil "\\`[^.]") '("stats.eld")))
    (should-not (directory-files (file-name-directory file) nil "\\`\\.mcpkit-stats-.*"))
    (let* ((form (mcpkit-stats-test--read-file file))
           (tools (plist-get (cdr form) :tools)))
      (should (eq (car form) 'mcpkit-stats))
      (should (eql (plist-get (cdr form) :version) 1))
      (should (stringp (plist-get (cdr form) :saved)))
      (should (= (length tools) 2))
      (let ((tt (seq-find (lambda (x) (equal (plist-get x :tool) "test_tool")) tools)))
        (should (= (plist-get tt :calls) 2))
        (should (= (plist-get tt :errors) 1))
        (should (= (plist-get tt :min-ms) 10.0))
        (should (= (plist-get tt :max-ms) 30.0))
        (should (numberp (plist-get tt :score))))
      (should (equal (plist-get (seq-find (lambda (x) (equal (plist-get x :tool) "emacs__eval")) tools)
                                :digests)
                     '(("buffer-list" . 1)))))
    ;; Reload into an empty table.
    (clrhash mcpkit-stats--table)
    (setq mcpkit-stats--synced-stamp nil)
    (should (mcpkit-stats-load))
    (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "test_tool")) 2))
    ;; Same file again is not double counted, unless forced.
    (should-not (mcpkit-stats-load))
    (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "test_tool")) 2))
    (should (mcpkit-stats-load t))
    (should (= (mcpkit-stats-record-call-count (mcpkit-stats-get "test_tool")) 4))))

(ert-deftest mcpkit-stats-test-save-omits-arguments-and-errors ()
  "Neither request arguments nor error messages reach the file."
  (mcpkit-stats-test--persisted
    (mcpkit-stats-record-call "test_tool" 'error 5.0 "leaky-error-message"
                              '(:content "PRIVATE-ARGUMENT-TEXT"))
    (mcpkit-stats-record-call "emacs__eval" 'error 5.0 "leaky-two"
                              '(:expression "(my-private-fn \"PRIVATE-EVAL-ARG\")"))
    (mcpkit-stats-save)
    (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
          (case-fold-search nil))
      (should-not (string-match-p "PRIVATE" text))
      (should-not (string-match-p "leaky" text))
      (should-not (string-match-p "recent" text))
      ;; The digest (function name) is an aggregate and is kept.
      (should (string-match-p "my-private-fn" text)))))

(ert-deftest mcpkit-stats-test-private-table-never-touches-file ()
  "Save, load and reset against a non-canonical table leave the file alone."
  (let* ((dir (make-temp-file "mcpkit-stats-test" t))
         (file (expand-file-name "stats.eld" dir))
         (mcpkit-stats-file file)
         (mcpkit-stats--table (make-hash-table :test #'equal))
         (mcpkit-stats--timer nil)
         (mcpkit--active-services (mcpkit-stats-test--services)))
    (unwind-protect
        (progn
          (should-not (eq mcpkit-stats--table mcpkit-stats--canonical-table))
          (mcpkit-stats-record-call "test_tool" 'success 1.0)
          (should-not mcpkit-stats--timer)
          (should-not (mcpkit-stats-save))
          (should-not (file-exists-p file))
          (mcpkit-stats-reset)
          (mcpkit-stats-reset "test_tool")
          (should-not (file-exists-p file))
          ;; An existing file is untouched by reset and load.
          (with-temp-file file (insert "SENTINEL"))
          (mcpkit-stats-reset)
          (should-not (mcpkit-stats-load t))
          (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                         "SENTINEL")))
      (delete-directory dir t))))

(ert-deftest mcpkit-stats-test-reset-updates-file ()
  "Reset rewrites the persisted aggregates immediately."
  (mcpkit-stats-test--persisted
    (mcpkit-stats-record-call "test_tool" 'success 1.0)
    (mcpkit-stats-record-call "tool_a" 'success 1.0)
    (mcpkit-stats-save)
    (should (equal (mcpkit-stats-reset "test_tool") '("test_tool")))
    (should (equal (mapcar (lambda (x) (plist-get x :tool))
                           (plist-get (cdr (mcpkit-stats-test--read-file file)) :tools))
                   '("tool_a")))
    (should (equal (mcpkit-stats-reset) '("tool_a")))
    (should-not (plist-get (cdr (mcpkit-stats-test--read-file file)) :tools))
    (should-not mcpkit-stats--dirty)))

(ert-deftest mcpkit-stats-test-load-merges ()
  "Loading adds counts, keeps earlier min and later max, unions digests."
  (mcpkit-stats-test--persisted
    (mcpkit-stats-record-call "emacs__eval" 'success 10.0 nil '(:expression "(foo 1)"))
    (mcpkit-stats-record-call "emacs__eval" 'success 20.0 nil '(:expression "(foo 1)"))
    (mcpkit-stats-save)
    (let ((rec (mcpkit-stats-get "emacs__eval")))
      (setf (mcpkit-stats-record-last-called-at rec) "2000-01-01 00:00:00")
      (clrhash (mcpkit-stats-record-digests rec))
      (puthash "bar" 4 (mcpkit-stats-record-digests rec))
      (setf (mcpkit-stats-record-min-duration-ms rec) 5.0
            (mcpkit-stats-record-max-duration-ms rec) 15.0
            (mcpkit-stats-record-call-count rec) 1))
    (should (mcpkit-stats-load t))
    (let ((rec (mcpkit-stats-get "emacs__eval")))
      (should (= (mcpkit-stats-record-call-count rec) 3))
      (should (= (mcpkit-stats-record-min-duration-ms rec) 5.0))
      (should (= (mcpkit-stats-record-max-duration-ms rec) 20.0))
      (should (string> (mcpkit-stats-record-last-called-at rec) "2001"))
      (should (equal (gethash "foo" (mcpkit-stats-record-digests rec)) 2))
      (should (equal (gethash "bar" (mcpkit-stats-record-digests rec)) 4))
      (should (> (mcpkit-stats-score "emacs__eval") 1.9)))))

(ert-deftest mcpkit-stats-test-load-bad-files-ignored ()
  "Unknown versions and garbage are ignored without signaling."
  (mcpkit-stats-test--persisted
    (make-directory (file-name-directory file) t)
    (dolist (content '("(mcpkit-stats :version 99 :tools ((:tool \"test_tool\" :calls 5)))"
                       "((((not closed"
                       "(other-thing :version 1)"))
      (with-temp-file file (insert content))
      (should-not (mcpkit-stats-load t))
      (should (= (hash-table-count mcpkit-stats--table) 0)))))

(ert-deftest mcpkit-stats-test-score-decay ()
  "Score halves each half-life and gains 1 per call."
  (mcpkit-stats-test--persisted
    (should (= (mcpkit-stats-score "test_tool") 0.0))
    (mcpkit-stats-record-call "test_tool" 'success 1.0)
    (let* ((rec (mcpkit-stats-get "test_tool"))
           (t0 (mcpkit-stats-record-score-time rec)))
      (should (< (abs (- (mcpkit-stats-score "test_tool" t0) 1.0)) 1e-6))
      (should (< (abs (- (mcpkit-stats-score "test_tool" (+ t0 (* 14 86400.0))) 0.5)) 1e-6))
      (should (< (abs (- (mcpkit-stats-score "test_tool" (+ t0 (* 28 86400.0))) 0.25)) 1e-6))
      ;; A second call at t0 + one half-life: 0.5 + 1.
      (setf (mcpkit-stats-record-score-time rec) (- t0 (* 14 86400.0)))
      (mcpkit-stats-record-call "test_tool" 'success 1.0)
      (should (< (abs (- (mcpkit-stats-score "test_tool") 1.5)) 1e-3)))))

(ert-deftest mcpkit-stats-test-dirty-and-timer ()
  "Recording marks dirty and schedules one idle save; saving clears both."
  (mcpkit-stats-test--persisted
    (should-not mcpkit-stats--dirty)
    (mcpkit-stats-record-call "test_tool" 'success 1.0)
    (should mcpkit-stats--dirty)
    (should (timerp mcpkit-stats--timer))
    (let ((first mcpkit-stats--timer))
      (mcpkit-stats-record-call "test_tool" 'success 1.0)
      (should (eq first mcpkit-stats--timer)))
    (mcpkit-stats--idle-save)
    (should-not mcpkit-stats--dirty)
    (should-not mcpkit-stats--timer)
    (should (file-exists-p file))
    ;; Idle timer does nothing when clean.
    (delete-file file)
    (mcpkit-stats--idle-save)
    (should-not (file-exists-p file))
    (should (memq 'mcpkit-stats--save-on-exit kill-emacs-hook))))

(ert-deftest mcpkit-stats-test-sprite-worker-skips-persistence ()
  "A sprite worker daemon neither saves nor loads."
  (mcpkit-stats-test--persisted
    (cl-letf (((symbol-function 'sprite-worker-p) (lambda () t)))
      (mcpkit-stats-record-call "test_tool" 'success 1.0)
      (should-not (mcpkit-stats-save))
      (should-not (file-exists-p file)))
    (cl-letf (((symbol-function 'sprite-worker-p) (lambda () (error "boom"))))
      (should (mcpkit-stats-save)))))

(ert-deftest mcpkit-stats-test-no-file-no-persistence ()
  "With `mcpkit-stats-file' nil nothing is written and no timer is set."
  (mcpkit-stats-test--persisted
    (let ((mcpkit-stats-file nil))
      (mcpkit-stats-record-call "test_tool" 'success 1.0)
      (should-not mcpkit-stats--timer)
      (should-not (mcpkit-stats-save))
      (should-not (plist-get (mcpkit-stats-persistence-info) :persisted)))))

(provide 'test-mcpkit-stats)

;;; test-mcpkit-stats.el ends here
