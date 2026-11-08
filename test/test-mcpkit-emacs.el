;;; test-mcpkit-emacs.el --- Tests for mcpkit-emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'mcpkit)
(require 'mcpkit-emacs)

(ert-deftest test-mcpkit-emacs/top-level-definition ()
  "Test that `mcpkit-emacs-service' is defined and has tools at load time."
  (should (mcpkit-service-p mcpkit-emacs-service))
  (should (eq (mcpkit-service-name mcpkit-emacs-service) 'emacs))
  (should (>= (hash-table-count (mcpkit-service-tools mcpkit-emacs-service)) 3)))

(ert-deftest test-mcpkit-emacs/registration ()
  "Test that core Emacs tools are registered on the emacs service."
  (let ((mcpkit-registry nil))
    (let ((svc (mcpkit-emacs-register)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'emacs))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "emacs_server_status" tools))
        (should (gethash "emacs_get_buffer" tools))
        (should (gethash "emacs_eval" tools))))))

(ert-deftest test-mcpkit-emacs/eval-and-buffer ()
  "Test evaluation and buffer inspection."
  (let ((mcpkit-registry nil))
    (let ((svc (mcpkit-emacs-register)))
      ;; Eval tool
      (let* ((eval-tool (gethash "emacs_eval" (mcpkit-service-tools svc)))
             (res (funcall (mcpkit-tool-handler eval-tool)
                           (list :expression "(+ 40 2)")
                           (lambda (_status r) r))))
        (should (equal (plist-get res :status) "success"))
        (should (equal (plist-get res :result) "42")))

      ;; Get buffer
      (with-current-buffer (get-buffer-create "*test-mcpkit-buf*")
        (insert "Buffer test payload")
        (let* ((buf-tool (gethash "emacs_get_buffer" (mcpkit-service-tools svc)))
               (res (funcall (mcpkit-tool-handler buf-tool)
                             (list :buffer_or_file "*test-mcpkit-buf*")
                             (lambda (_status r) r))))
          (should (plist-get res :found))
          (should (equal (plist-get res :name) "*test-mcpkit-buf*"))
          (should (string-search "Buffer test payload" (plist-get res :content))))
        (kill-buffer "*test-mcpkit-buf*")))))

(ert-deftest test-mcpkit-emacs/buffer-append ()
  "Append to files and buffers, respecting newline and narrowing."
  (let* ((file (make-temp-file "mcpkit-append-"))
         (mcpkit-registry nil)
         (svc (mcpkit-emacs-register))
         (tool (map-elt (mcpkit-service-tools svc) "emacs_buffer_append"))
         (handler (and tool (mcpkit-tool-handler tool))))
    (unwind-protect
        (progn
          (should handler)
          (with-temp-file file (insert "first"))
          (funcall handler (list :file_or_buffer file :content "second") #'ignore)
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "first\nsecond"))
          (with-current-buffer (find-buffer-visiting file)
            (narrow-to-region (point-min) (+ (point-min) 2))
            (funcall handler (list :file_or_buffer (buffer-name)
                                   :content "!" :ensure_newline nil)
                     #'ignore)
            (widen))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         "first\nsecond!")))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest test-mcpkit-emacs/buffer-append-unvisited-buffer ()
  "Append to an in-memory buffer without attempting to save it."
  (let* ((mcpkit-registry nil)
         (svc (mcpkit-emacs-register))
         (tool (map-elt (mcpkit-service-tools svc) "emacs_buffer_append")))
    (with-temp-buffer
      (let ((name (buffer-name)))
        (funcall (mcpkit-tool-handler tool)
                 (list :file_or_buffer name :content "hello") #'ignore)
        (should (equal (buffer-string) "hello"))))))

(ert-deftest test-mcpkit-emacs/buffer-append-preserves-coding ()
  "Preserve a file's detected encoding when appending."
  (let* ((file (make-temp-file "mcpkit-append-" nil ".latin1"))
         (auto-coding-alist (cons '("\\.latin1\\'" . iso-latin-1) auto-coding-alist))
         (mcpkit-registry nil)
         (svc (mcpkit-emacs-register))
         (tool (map-elt (mcpkit-service-tools svc) "emacs_buffer_append")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'iso-latin-1))
            (with-temp-file file (insert "café")))
          (funcall (mcpkit-tool-handler tool)
                   (list :file_or_buffer file :content "été") #'ignore)
          (should (equal (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string))
                         (unibyte-string ?c ?a ?f 233 ?\n 233 ?t 233))))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(provide 'test-mcpkit-emacs)
;;; test-mcpkit-emacs.el ends here
