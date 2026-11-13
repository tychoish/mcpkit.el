;;; mcpkit-foundation-test.el --- Dependency isolation checks -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'seq)

(defconst mcpkit-foundation-test-root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(ert-deftest mcpkit-foundation-core-without-agent-shell ()
  (when (featurep 'agent-shell)
    (ert-skip "agent-shell is already loaded"))
  (let ((load-path (cons mcpkit-foundation-test-root
                         (seq-remove (lambda (path) (string-match-p "agent-shell" path)) load-path)))
        (original-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest args)
                 (when (eq feature 'agent-shell) (error "agent-shell unavailable"))
                 (apply original-require feature args))))
      (should-not (locate-library "agent-shell"))
      (require 'mcpkit)
      (require 'mcpkit-services)
      (when (mcpkit-active-port)
        (ert-skip "MCPKit already has an active listener"))
      (let ((mcpkit-service-modules nil)
            (mcpkit-registry nil)
            (mcpkit-services-port 0))
        (unwind-protect
            (progn
              (mcpkit-define-service 'foundation-test :port 0)
              (mcpkit-init-services-sync)
              (should (integerp (mcpkit-active-port)))
              (should-not (featurep 'agent-shell)))
          (mcpkit-stop-all-services))))))
