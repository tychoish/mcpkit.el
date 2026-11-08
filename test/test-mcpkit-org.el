;;; test-mcpkit-org.el --- Tests for Org MCP service -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'map)
(require 'mcpkit-org)

(ert-deftest mcpkit-org-test-child-by-custom-id ()
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n:PROPERTIES:\n:CUSTOM_ID: tasks\n:END:\n* Next\n")
    (mcpkit-org-append-entry (buffer-name) "work" :custom-id "tasks"
                             :headline-title "Example" :todo-state "TODO"
                             :priority "A" :properties '(:OWNER "Sam"))
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\* TODO \\[#[A]\\] Example$" nil t))
    (should (equal (org-entry-get nil "OWNER") "Sam"))
    (should (re-search-forward "^work\n\\* Next$" nil t))))

(ert-deftest mcpkit-org-test-target-errors-do-not-change-buffer ()
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n* TASKS\n")
    (let ((before (buffer-string)))
      (should-error (mcpkit-org-append-entry (buffer-name) "x" :target-heading "tasks"))
      (should-error (mcpkit-org-append-entry (buffer-name) "x" :custom-id "missing"))
      (should (equal before (buffer-string))))))

(ert-deftest mcpkit-org-test-position-and-save ()
  (let* ((file (make-temp-file "mcpkit-org-" nil ".org" "* Tasks\nbody\n** Existing\n* Next\n"))
         (buffer nil))
    (unwind-protect
        (progn
          (mcpkit-org-append-entry file "intro" :target-heading "tasks" :position "beginning_of_subtree")
          (setq buffer (find-buffer-visiting file))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "\\* Tasks\nintro\nbody\n\\*\\* Existing" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest mcpkit-org-test-registration ()
  (should (mcpkit-service-p mcpkit-org-service))
  (should (map-elt (mcpkit-service-tools mcpkit-org-service) "org_append_entry")))

(ert-deftest mcpkit-org-test-parent-body-before-child ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nbody\n** Child\nchild body\n* Sibling\n")
    (mcpkit-org-append-entry (buffer-name) "new body" :target-heading "Parent")
    (should (string-match-p "body\nnew body\n\\*\\* Child" (buffer-string)))))

(ert-deftest mcpkit-org-test-first-and-last-child-positions ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nbody\n** Existing\nold\n* Next\n")
    (mcpkit-org-append-entry (buffer-name) "first body" :target-heading "Parent"
                             :position "beginning_of_subtree" :headline-title "First")
    (mcpkit-org-append-entry (buffer-name) "last body" :target-heading "Parent"
                             :position "as_last_child" :headline-title "Last")
    (should (equal (buffer-string)
                   "* Parent\nbody\n** First\nfirst body\n** Existing\nold\n** Last\nlast body\n* Next\n"))))

(ert-deftest mcpkit-org-test-invalid-property-rolls-back ()
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nbody\n* Next\n")
    (let ((before (buffer-string)))
      (should-error (mcpkit-org-append-entry
                     (buffer-name) "text" :target-heading "Parent"
                     :headline-title "Child" :properties '(:GOOD "ok" :BAD "bad\nvalue")))
      (should (equal before (buffer-string))))))

(ert-deftest mcpkit-org-test-duplicate-custom-id-and-missing-title ()
  (with-temp-buffer
    (org-mode)
    (insert "* One\n:PROPERTIES:\n:CUSTOM_ID: repeated\n:END:\n* Two\n:PROPERTIES:\n:CUSTOM_ID: repeated\n:END:\n")
    (let ((before (buffer-string)))
      (should-error (mcpkit-org-append-entry (buffer-name) "x" :custom-id "repeated"))
      (should-error (mcpkit-org-append-entry (buffer-name) "x" :target-heading "missing"))
      (should (equal before (buffer-string))))))

(ert-deftest mcpkit-org-test-preserve-file-encoding ()
  (let ((file (make-temp-file "mcpkit-org-encoding-" nil ".org"))
        (buffer nil))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "* Tasks\nExisting café\n")
            (let ((coding-system-for-write 'utf-16le-with-signature))
              (write-region (point-min) (point-max) file nil 'silent)))
          (mcpkit-org-append-entry file "Added café" :target-heading "Tasks")
          (setq buffer (find-buffer-visiting file))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            (should (equal (buffer-substring-no-properties (point-min) (+ (point-min) 2))
                           (unibyte-string #xff #xfe))))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "Existing café\nAdded café" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file file))))

(ert-deftest mcpkit-org-test-json-dispatch-properties ()
  "Insert nested JSON properties through the MCP request dispatcher."
  (with-temp-buffer
    (org-mode)
    (insert "* Tasks\n* Other\n")
    (let ((payload (json-serialize
                    (list :jsonrpc "2.0" :id 1 :method "tools/call"
                          :params (list :name "org_append_entry"
                                        :arguments (list :file_or_buffer (buffer-name)
                                                         :target_heading "Tasks"
                                                         :headline_title "Child"
                                                         :properties (list :OWNER "Sam")
                                                         :content "Work")))))
          status response)
      (mcpkit--handle-request-payload
       payload (list (cons mcpkit-org-service 'error))
       (lambda (s r &rest _) (setq status s response r)))
      (should (eq status 'success))
      (should-not (plist-get response :error))
      (goto-char (point-min))
      (should (search-forward "** Child" nil t))
      (should (equal (org-entry-get nil "OWNER") "Sam"))
      (should (search-forward "Work\n* Other" nil t)))))

(provide 'test-mcpkit-org)
;;; test-mcpkit-org.el ends here
