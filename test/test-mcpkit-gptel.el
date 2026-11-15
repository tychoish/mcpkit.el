;;; test-mcpkit-gptel.el --- Tests for mcpkit-gptel -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'mcpkit)
(when (locate-library "gptel")
  (require 'gptel))
(require 'mcpkit-gptel)

;;; JSON-schema -> gptel :args conversion

(ert-deftest test-mcpkit-gptel/schema-to-args-required-vs-optional ()
  "Required properties get no :optional, missing ones get :optional t."
  (let* ((schema '(:type "object"
                   :properties (:foo (:type "string" :description "a string")
                                :bar (:type "integer" :description "an int"))
                   :required ["foo"]))
         (args (mcpkit-gptel--schema-to-args schema))
         (foo (cl-find "foo" args :key (lambda (a) (plist-get a :name)) :test #'equal))
         (bar (cl-find "bar" args :key (lambda (a) (plist-get a :name)) :test #'equal)))
    (should (= (length args) 2))
    (should (eq (plist-get foo :type) 'string))
    (should (null (plist-get foo :optional)))
    (should (eq (plist-get bar :type) 'integer))
    (should (eq (plist-get bar :optional) t))))

(ert-deftest test-mcpkit-gptel/schema-to-args-types ()
  "Covers string/integer/boolean/array JSON-schema type conversion."
  (let* ((schema '(:type "object"
                   :properties (:a (:type "string" :description "s")
                                :b (:type "integer" :description "i")
                                :c (:type "boolean" :description "b")
                                :d (:type "array" :description "a"
                                    :items (:type "string")))
                   :required ["a" "b" "c" "d"]))
         (args (mcpkit-gptel--schema-to-args schema)))
    (should (eq (plist-get (cl-find "a" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'string))
    (should (eq (plist-get (cl-find "b" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'integer))
    (should (eq (plist-get (cl-find "c" args :key (lambda (x) (plist-get x :name)) :test #'equal) :type) 'boolean))
    (let ((d (cl-find "d" args :key (lambda (x) (plist-get x :name)) :test #'equal)))
      (should (eq (plist-get d :type) 'array))
      (should (eq (plist-get (plist-get d :items) :type) 'string)))))

(ert-deftest test-mcpkit-gptel/schema-to-args-no-required ()
  "When :required is absent, all properties are optional."
  (let* ((schema '(:type "object"
                   :properties (:only (:type "string" :description "s"))))
         (args (mcpkit-gptel--schema-to-args schema)))
    (should (= (length args) 1))
    (should (eq (plist-get (car args) :optional) t))))

;;; Round-trip registration and invocation

(ert-deftest test-mcpkit-gptel/round-trip-success ()
  "Register a synchronous mcpkit tool and invoke through the gptel wrapper."
  (skip-unless (featurep 'gptel))
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'gtest :description "gptel test service")
    (mcpkit-register-tool 'gt_add 'gtest
      :description "Add two integers."
      :input-schema '(:type "object"
                      :properties (:x (:type "integer" :description "first addend")
                                   :y (:type "integer" :description "second addend"))
                      :required ["x" "y"])
      (+ (plist-get args :x) (plist-get args :y)))
    (let* ((created (mcpkit-gptel-register-service-tools 'gtest))
           (gtool (car created))
           (result nil))
      (should (= (length created) 1))
      (should (equal (gptel-tool-name gtool) "gt_add"))
      (should (equal (gptel-tool-category gtool) "gtest"))
      (should (gptel-tool-async gtool))
      (let ((args-spec (gptel-tool-args gtool)))
        (should (= (length args-spec) 2)))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               3 4)
      (should (equal result 7)))))

(ert-deftest test-mcpkit-gptel/round-trip-error ()
  "A handler that reports an error via DONE surfaces a clear error string."
  (skip-unless (featurep 'gptel))
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'gtest-err :description "gptel error test service")
    (mcpkit-register-tool 'gt_fail 'gtest-err
      :description "Always fails."
      :input-schema '(:type "object"
                      :properties (:msg (:type "string" :description "input"))
                      :required ["msg"])
      :async t
      (funcall done "deliberate failure" nil))
    (let* ((created (mcpkit-gptel-register-service-tools 'gtest-err))
           (gtool (car created))
           (result nil))
      (funcall (gptel-tool-function gtool)
               (lambda (r) (setq result r))
               "irrelevant")
      (should (stringp result))
      (should (string-match-p "deliberate failure" result))
      (should (string-match-p "\\`Error" result)))))

;;; Multi-service registration and session setup

(ert-deftest test-mcpkit-gptel/register-all-services ()
  "Register multiple services across mcpkit and confirm all are bridged to gptel."
  (skip-unless (featurep 'gptel))
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'svc1 :description "service one")
    (mcpkit-register-tool 'tool1 'svc1 :description "tool 1" (identity 1))
    (mcpkit-define-service 'svc2 :description "service two")
    (mcpkit-register-tool 'tool2 'svc2 :description "tool 2" (identity 2))
    (let ((registered (mcpkit-gptel-register-all-services)))
      (should (= (length registered) 2))
      (should (assoc 'svc1 registered))
      (should (assoc 'svc2 registered))
      (should (assoc "svc1" gptel--known-tools))
      (should (assoc "svc2" gptel--known-tools)))))

(ert-deftest test-mcpkit-gptel/get-tools-filtering ()
  "Filtering by service name or retrieving all tools."
  (skip-unless (featurep 'gptel))
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil))
    (mcpkit-define-service 'alpha :description "alpha")
    (mcpkit-register-tool 'a_tool 'alpha :description "a" (identity 1))
    (mcpkit-define-service 'beta :description "beta")
    (mcpkit-register-tool 'b_tool 'beta :description "b" (identity 2))
    (mcpkit-gptel-register-all-services)
    (let ((alpha-tools (mcpkit-gptel-get-tools '(alpha)))
          (all-tools (mcpkit-gptel-get-tools 'all)))
      (should (= (length alpha-tools) 1))
      (should (equal (gptel-tool-name (car alpha-tools)) "a_tool"))
      (should (= (length all-tools) 2)))))

(ert-deftest test-mcpkit-gptel/setup-session-buffer ()
  "Setting up a buffer configures buffer-local tools and use-tools flag."
  (skip-unless (featurep 'gptel))
  (let ((mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-tools nil)
        (mcpkit-gptel-default-services '(my_service)))
    (mcpkit-define-service 'my_service :description "my service")
    (mcpkit-register-tool 'my_tool 'my_service :description "t" (identity 42))
    (with-temp-buffer
      (mcpkit-gptel-setup-session (current-buffer))
      (should gptel-use-tools)
      (should (= (length gptel-tools) 1))
      (should (equal (gptel-tool-name (car gptel-tools)) "my_tool"))
      ;; Test disable
      (mcpkit-gptel-disable)
      (should (= (length gptel-tools) 0))
      ;; Test re-enable
      (mcpkit-gptel-enable)
      (should (= (length gptel-tools) 1)))))

(ert-deftest test-mcpkit-gptel/mode-hook ()
  "Enabling and disabling mcpkit-gptel-mode affects gptel-mode-hook."
  (skip-unless (featurep 'gptel))
  (mcpkit-gptel-mode -1)
  (should-not (member #'mcpkit-gptel-setup-session gptel-mode-hook))
  (mcpkit-gptel-mode 1)
  (should (member #'mcpkit-gptel-setup-session gptel-mode-hook))
  (mcpkit-gptel-mode -1)
  (should-not (member #'mcpkit-gptel-setup-session gptel-mode-hook)))

(ert-deftest test-mcpkit-gptel/aliases ()
  "Verify backward compatibility aliases for gptel-mcp."
  (should (fboundp 'gptel-mcp-mode))
  (should (fboundp 'gptel-mcp-setup-session))
  (should (fboundp 'gptel-mcp-register-all-services))
  (should (fboundp 'gptel-mcp-register-service-tools))
  (should (fboundp 'gptel-mcp-get-tools)))

(provide 'test-mcpkit-gptel)
;;; test-mcpkit-gptel.el ends here
