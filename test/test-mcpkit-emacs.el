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
        (should (map-elt tools "emacs_server_status"))
        (should (map-elt tools "emacs_get_buffer"))
        (should (map-elt tools "eval"))
        (should-not (map-elt tools "emacs_eval"))
        (should (map-elt tools "mcpkit_stats"))
        (dolist (gone '("mcp_buffer_list" "mcp_buffer_kill" "mcp_buffer_save"
                        "mcp_buffer_revert" "mcp_get_messages"))
          (should-not (map-elt tools gone)))
        (should (map-elt tools "emacs_list_buffers"))
        (should (map-elt tools "emacs_kill_buffer"))
        (should (map-elt tools "emacs_save_buffer"))
        (should (map-elt tools "emacs_revert_buffer"))
        (should (map-elt tools "emacs_get_messages"))))))

;; mcpkit-tool-aliases is customizable; only the defaults are checked here.
(ert-deftest test-mcpkit-emacs/legacy-aliases-resolve ()
  "Old eval and mcp_* duplicate names resolve to the current tools."
  (let ((svcs (list (cons mcpkit-emacs-service 'namespace)))
        (mcpkit-tool-aliases (default-value 'mcpkit-tool-aliases)))
    (dolist (pair '(("emacs_eval" . "emacs__eval")
                    ("emacs__emacs_eval" . "emacs__eval")
                    ("mcp_buffer_list" . "emacs__emacs_list_buffers")
                    ("mcp_buffer_kill" . "emacs__emacs_kill_buffer")
                    ("mcp_buffer_save" . "emacs__emacs_save_buffer")
                    ("mcp_buffer_revert" . "emacs__emacs_revert_buffer")
                    ("mcp_get_messages" . "emacs__emacs_get_messages")))
      (should (equal (mcpkit-resolve-tool-name (car pair) svcs) (cdr pair))))))

(ert-deftest test-mcpkit-emacs/eval-description ()
  "The eval tool steers agents toward specific tools."
  (let ((tool (gethash "eval" (mcpkit-service-tools mcpkit-emacs-service))))
    (should (string-prefix-p "Last resort: evaluates arbitrary Emacs Lisp. Prefer a specific tool"
                             (mcpkit-tool-description tool)))))

(ert-deftest test-mcpkit-emacs/mcpkit-stats-tool ()
  "The mcpkit_stats tool returns structured rows."
  (let ((mcpkit-stats--table (make-hash-table :test #'equal))
        (mcpkit--active-services (list (cons mcpkit-emacs-service 'namespace))))
    (mcpkit-stats-record-call "emacs__eval" 'success 2.0 nil '(:expression "(sprite-create 1)"))
    (mcpkit-stats-record-call "emacs__emacs_open" 'success 2.0)
    (let* ((tool (gethash "mcpkit_stats" (mcpkit-service-tools mcpkit-emacs-service)))
           (res (funcall (mcpkit-tool-handler tool) nil (lambda (_e r) r)))
           (rows (append (plist-get res :rows) nil)))
      (should (= (plist-get res :count) 2))
      (should (= (length rows) 2))
      (let* ((res1 (funcall (mcpkit-tool-handler tool) '(:tool "emacs__eval" :limit 5)
                            (lambda (_e r) r)))
             (row (aref (plist-get res1 :rows) 0)))
        (should (equal (plist-get row :tool) "emacs__eval"))
        (should (equal (plist-get (aref (plist-get row :top_digests) 0) :digest)
                       "sprite-create")))
      (let ((res2 (funcall (mcpkit-tool-handler tool) '(:limit 1) (lambda (_e r) r))))
        (should (= (length (plist-get res2 :rows)) 1))))))

(ert-deftest test-mcpkit-emacs/stats-tool-persistence-fields ()
  "mcpkit_stats reports persistence, usage pins and per-row score and pin."
  (let ((mcpkit-stats--table (make-hash-table :test #'equal))
        (mcpkit-stats-file nil)
        (mcpkit-stats--last-saved nil)
        (mcpkit--usage-pins nil)
        (mcpkit--catalog-snapshot nil)
        (mcpkit-search-pinned-usage-min-calls 2)
        (mcpkit--active-services (list (cons mcpkit-emacs-service 'namespace))))
    (dotimes (_ 3) (mcpkit-stats-record-call "emacs__emacs_open" 'success 1.0))
    (let* ((tool (gethash "mcpkit_stats" (mcpkit-service-tools mcpkit-emacs-service)))
           (res (funcall (mcpkit-tool-handler tool) nil (lambda (_e r) r)))
           (row (aref (plist-get res :rows) 0)))
      (should-not (plist-get res :persisted))
      (should-not (plist-get res :last_saved))
      (should (equal (append (plist-get res :usage_pins) nil) '("emacs__emacs_open")))
      (should (> (plist-get row :score) 2.9))
      (should (equal (plist-get row :pinned) "usage"))
      (should (stringp (mcpkit--serialize-response res))))))

(ert-deftest test-mcpkit-emacs/eval-and-buffer ()
  "Test evaluation and buffer inspection."
  (let ((mcpkit-registry nil))
    (let ((svc (mcpkit-emacs-register)))
      ;; Eval tool
      (let* ((eval-tool (gethash "eval" (mcpkit-service-tools svc)))
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

(ert-deftest test-mcpkit-emacs/buffer-lifecycle-tools ()
  "Test emacs_list_buffers, emacs_save_buffer, emacs_revert_buffer, and emacs_kill_buffer."
  (let* ((file (make-temp-file "mcpkit-buf-test-"))
         (svc (mcpkit-emacs-register))
         (tools (mcpkit-service-tools svc))
         (list-tool (map-elt tools "emacs_list_buffers"))
         (save-tool (map-elt tools "emacs_save_buffer"))
         (revert-tool (map-elt tools "emacs_revert_buffer"))
         (kill-tool (map-elt tools "emacs_kill_buffer")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "initial text"))
          (let ((buf (find-file-noselect file)))
            ;; Test list_buffers
            (let (res)
              (funcall (mcpkit-tool-handler list-tool) nil (lambda (_status r) (setq res r)))
              (should (vectorp res))
              (should (seq-some (lambda (b) (equal (plist-get b :name) (buffer-name buf))) res)))

            ;; Modify buffer and save_buffer
            (with-current-buffer buf
              (goto-char (point-max))
              (insert " modified"))
            (let (res)
              (funcall (mcpkit-tool-handler save-tool) (list :buffer (buffer-name buf))
                       (lambda (_status r) (setq res r)))
              (should (equal (plist-get res :status) "ok")))
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                           "initial text modified"))

            ;; Modify disk file and revert_buffer
            (with-temp-file file (insert "disk changes"))
            (let (res)
              (funcall (mcpkit-tool-handler revert-tool) (list :buffer (buffer-name buf))
                       (lambda (_status r) (setq res r)))
              (should (equal (plist-get res :status) "ok")))
            (should (equal (with-current-buffer buf (buffer-string)) "disk changes"))

            ;; kill_buffer
            (let (res)
              (funcall (mcpkit-tool-handler kill-tool) (list :buffer (buffer-name buf))
                       (lambda (_status r) (setq res r)))
              (should (equal (plist-get res :status) "ok")))
            (should-not (get-buffer (file-name-nondirectory file)))))
      (delete-file file))))

(ert-deftest test-mcpkit-emacs/get-messages-tool ()
  "Test emacs_get_messages fetching trailing lines."
  (let* ((svc (mcpkit-emacs-register))
         (tool (map-elt (mcpkit-service-tools svc) "emacs_get_messages"))
         res)
    (message "mcpkit-test-marker-12345")
    (funcall (mcpkit-tool-handler tool) (list :tail_lines 10) (lambda (_status r) (setq res r)))
    (should (equal (plist-get res :buffer) "*Messages*"))
    (should (plist-get res :exists))
    (should (string-search "mcpkit-test-marker-12345" (plist-get res :text)))))

(provide 'test-mcpkit-emacs)
;;; test-mcpkit-emacs.el ends here
