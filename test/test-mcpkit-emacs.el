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

(provide 'test-mcpkit-emacs)
;;; test-mcpkit-emacs.el ends here
