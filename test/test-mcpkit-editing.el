;;; test-mcpkit-editing.el --- Tests for mcpkit-editing -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'mcpkit)
(require 'mcpkit-editing)

(defun test-mcpkit-editing--make-service ()
  "Create and return a fresh test service with editing tools registered."
  (let ((svc (mcpkit-define-service 'test-editing-service
               :port 0
               :description "Test service for mcpkit-editing")))
    (mcpkit-editing-register-tools svc)
    svc))

(defun test-mcpkit-editing--call (svc tool-name args)
  "Invoke TOOL-NAME on SVC's tool table with ARGS and return the raw result."
  (let ((tool (gethash tool-name (mcpkit-service-tools svc))))
    (should tool)
    (funcall (mcpkit-tool-handler tool) args (lambda (_status r) r))))

(ert-deftest test-mcpkit-editing/registration ()
  "Test that all four editing tools are registered on the passed-in service."
  (let ((mcpkit-registry nil))
    (let ((svc (test-mcpkit-editing--make-service)))
      (should (mcpkit-service-p svc))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "mcp_buffer_read" tools))
        (should (gethash "mcp_buffer_replace_region" tools))
        (should (gethash "mcp_ast_grep" tools))
        (should (gethash "mcp_project_diagnostics" tools))))))

(ert-deftest test-mcpkit-editing/buffer-read-from-disk ()
  "Test `mcp_buffer_read' against a real temp file with no open buffer."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-test" nil ".txt" "line1\nline2\nline3\n")))
      (unwind-protect
          (progn
            ;; Ensure no buffer is visiting the file.
            (should (not (find-buffer-visiting tmpfile)))
            (let ((res (test-mcpkit-editing--call svc "mcp_buffer_read" (list :file tmpfile))))
              (should (plist-get res :found))
              (should (string-search "line1" (plist-get res :content)))
              (should (string-search "line3" (plist-get res :content))))
            ;; Sliced read.
            (let ((res (test-mcpkit-editing--call
                        svc "mcp_buffer_read"
                        (list :file tmpfile :start 2 :line-count 1))))
              (should (equal (string-trim (plist-get res :content)) "line2"))))
        (delete-file tmpfile)))))

(ert-deftest test-mcpkit-editing/buffer-read-prefers-open-buffer ()
  "Test that `mcp_buffer_read' prefers an already-visited buffer's unsaved contents."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-test" nil ".txt" "on-disk-content\n")))
      (unwind-protect
          (let ((buf (find-file-noselect tmpfile)))
            (unwind-protect
                (progn
                  (with-current-buffer buf
                    (goto-char (point-max))
                    (insert "UNSAVED-EDIT\n"))
                  (let ((res (test-mcpkit-editing--call svc "mcp_buffer_read" (list :file tmpfile))))
                    (should (string-search "UNSAVED-EDIT" (plist-get res :content)))
                    (should (string-search "on-disk-content" (plist-get res :content)))))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf)))
        (delete-file tmpfile)))))

(ert-deftest test-mcpkit-editing/buffer-replace-region-success ()
  "Test `mcp_buffer_replace_region' successfully replaces text and saves."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-test" nil ".txt" "hello world\n")))
      (unwind-protect
          (let* ((buf (find-file-noselect tmpfile))
                 (start (with-current-buffer buf (goto-char (point-min)) (+ (point) 6)))
                 (end (with-current-buffer buf (goto-char (point-min)) (+ (point) 11))))
            (unwind-protect
                (let ((res (test-mcpkit-editing--call
                            svc "mcp_buffer_replace_region"
                            (list :file tmpfile :start start :end end :replacement "emacs"))))
                  (should (equal (plist-get res :file) tmpfile))
                  (should (plist-get res :bytes-written))
                  (with-current-buffer buf
                    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                                   "hello emacs\n")))
                  (should (equal (with-temp-buffer
                                   (insert-file-contents tmpfile)
                                   (buffer-string))
                                 "hello emacs\n")))
              (kill-buffer buf)))
        (delete-file tmpfile)))))

(ert-deftest test-mcpkit-editing/buffer-replace-region-out-of-range ()
  "Test `mcp_buffer_replace_region' signals a clear error for out-of-range positions."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-test" nil ".txt" "short\n")))
      (unwind-protect
          (let* ((buf (find-file-noselect tmpfile)))
            (unwind-protect
                (let ((tool (gethash "mcp_buffer_replace_region" (mcpkit-service-tools svc)))
                      (result nil))
                  (funcall (mcpkit-tool-handler tool)
                           (list :file tmpfile :start 1 :end 9999 :replacement "x")
                           (lambda (err-msg r) (setq result (cons err-msg r))))
                  (should (stringp (car result)))
                  (should (null (cdr result)))
                  (should (string-search "out of range" (car result))))
              (kill-buffer buf)))
        (delete-file tmpfile)))))

(ert-deftest test-mcpkit-editing/ast-grep-missing-binary-clear-error ()
  "Test that a missing `ast-grep' binary surfaces a clear error, not a crash."
  (let ((mcpkit-registry nil)
        (mcpkit-editing-ast-grep-executable "mcpkit-editing-nonexistent-binary-xyz"))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tool (gethash "mcp_ast_grep" (mcpkit-service-tools svc)))
           (result nil))
      (funcall (mcpkit-tool-handler tool)
               (list :pattern "foo")
               (lambda (err-msg r) (setq result (cons err-msg r))))
      (should (stringp (car result)))
      (should (null (cdr result)))
      (should (string-search "ast-grep" (car result))))))

(ert-deftest test-mcpkit-editing/ast-grep-real-binary-if-available ()
  "Test `mcp_ast_grep' against the real binary, when available on PATH."
  (if (not (executable-find "ast-grep"))
      (ert-skip "ast-grep executable not found on PATH; skipping real-invocation test")
    (let ((mcpkit-registry nil))
      (let* ((svc (test-mcpkit-editing--make-service))
             (tmpdir (make-temp-file "mcpkit-editing-astgrep" t))
             (tmpfile (expand-file-name "sample.el" tmpdir)))
        (unwind-protect
            (progn
              (with-temp-file tmpfile
                (insert "(defun foo (x) (+ x 1))\n"))
              (let ((res (test-mcpkit-editing--call
                          svc "mcp_ast_grep"
                          (list :pattern "(defun $NAME ($$$) $$$)" :path tmpdir :language "elisp"))))
                (should (plist-member res :matches))))
          (delete-directory tmpdir t))))))

(ert-deftest test-mcpkit-editing/project-diagnostics-empty-no-buffer ()
  "Test `mcp_project_diagnostics' returns an empty list when no buffer is open."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (res (test-mcpkit-editing--call
                 svc "mcp_project_diagnostics"
                 (list :path "/nonexistent/path/does-not-exist.txt"))))
      (should (equal (plist-get res :diagnostics) [])))))

(ert-deftest test-mcpkit-editing/project-diagnostics-real-flymake-diagnostic ()
  "Test `mcp_project_diagnostics' extracts :line/:severity/:message from a real Flymake diagnostic.
Seeds a genuine `flymake--diag' via `flymake-make-diagnostic' and installs
it as a real overlay via `flymake--highlight-line' (Flymake's own internal
mechanism for registering a diagnostic, used instead of a fake linter
process so the test exercises real `flymake-diagnostics' machinery
synchronously)."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-diag" nil ".txt" "line one\nline two\nline three\n")))
      (unwind-protect
          (let ((buf (find-file-noselect tmpfile)))
            (unwind-protect
                (with-current-buffer buf
                  (setq-local flymake-mode t)
                  (goto-char (point-min))
                  (forward-line 1)
                  (let* ((beg (point))
                         (end (line-end-position))
                         (diag (flymake-make-diagnostic (current-buffer) beg end
                                                        :error "synthetic diagnostic for test")))
                    (flymake--highlight-line diag))
                  (let ((res (test-mcpkit-editing--call
                              svc "mcp_project_diagnostics"
                              (list :path tmpfile))))
                    (let ((diags (append (plist-get res :diagnostics) nil)))
                      (should (= (length diags) 1))
                      (let ((d (car diags)))
                        (should (= (plist-get d :line) 2))
                        (should (equal (plist-get d :severity) ":error"))
                        (should (equal (plist-get d :message) "synthetic diagnostic for test"))))))
              (kill-buffer buf)))
        (delete-file tmpfile)))))

(ert-deftest test-mcpkit-editing/project-diagnostics-empty-no-flymake ()
  "Test `mcp_project_diagnostics' returns an empty list for an open buffer without Flymake active."
  (let ((mcpkit-registry nil))
    (let* ((svc (test-mcpkit-editing--make-service))
           (tmpfile (make-temp-file "mcpkit-editing-diag" nil ".txt" "no diagnostics here\n")))
      (unwind-protect
          (let ((buf (find-file-noselect tmpfile)))
            (unwind-protect
                (let ((res (test-mcpkit-editing--call
                            svc "mcp_project_diagnostics"
                            (list :path tmpfile))))
                  (should (equal (plist-get res :diagnostics) [])))
              (kill-buffer buf)))
        (delete-file tmpfile)))))

(provide 'test-mcpkit-editing)
;;; test-mcpkit-editing.el ends here
