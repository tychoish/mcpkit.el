;;; test-mcpkit-services.el --- Tests for mcpkit-services -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'mcpkit-services)

(defmacro mcpkit-services-test--with-bound-port (var port &rest body)
  "Run BODY with local PORT bound; VAR names the listening process."
  (declare (indent 2))
  `(let ((,var (make-network-process :name "ert-bound-probe" :server t
                                     :host "127.0.0.1" :service ,port
                                     :noquery t :family 'ipv4)))
     (unwind-protect (progn ,@body)
       (delete-process ,var))))

(ert-deftest mcpkit-services-test-port-probing ()
  "A bound port is unavailable and the next probe skips past it."
  (let ((base (mcpkit-find-available-port 45000 50)))
    (mcpkit-services-test--with-bound-port _proc base
      (should-not (mcpkit-port-available-p base))
      (should (> (mcpkit-find-available-port base 10) base)))))

(ert-deftest mcpkit-services-test-instance-port-is-stable-and-in-range ()
  "The same name yields the same port inside the configured range."
  (let ((mcpkit-instance-name-function (lambda () "ert-stable")))
    (let ((p1 (mcpkit-instance-port))
          (p2 (mcpkit-instance-port)))
      (should (= p1 p2))
      (should (<= mcpkit-instance-port-base p1
                  (+ mcpkit-instance-port-base mcpkit-instance-port-range 50))))))

(ert-deftest mcpkit-services-test-instance-port-concurrent-instances ()
  "A second instance with a bound preferred port gets a later port."
  (let* ((mcpkit-instance-name-function (lambda () "solo"))
         (p1 (mcpkit-instance-port)))
    (mcpkit-services-test--with-bound-port _proc p1
      (should (> (mcpkit-instance-port) p1)))))

(ert-deftest mcpkit-services-test-init-skips-when-running ()
  "Init does nothing when a server is already active."
  (let ((started nil))
    (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () 1234))
              ((symbol-function 'mcpkit-start-all-services)
               (lambda (&rest _) (setq started t))))
      (mcpkit-init-services-sync)
      (should-not started))))

(ert-deftest mcpkit-services-test-init-sync-uses-configured-port ()
  "Sync init loads modules and starts on an integer `mcpkit-services-port'."
  (let ((mcpkit-service-modules nil)
        (mcpkit-services-port 45678)
        (port nil))
    (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () nil))
              ((symbol-function 'mcpkit-start-all-services)
               (lambda (p &rest _) (setq port p))))
      (mcpkit-init-services-sync)
      (should (= port 45678)))))

;; Async path

(ert-deftest mcpkit-services-test-init-async-loads-then-starts ()
  "Interactive init loads modules asynchronously, then starts services."
  (let ((noninteractive nil)
        (loaded nil)
        (started nil))
    (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () nil))
              ((symbol-function 'mcpkit-load-services-async)
               (lambda (&rest args)
                 (setq loaded t)
                 (funcall (plist-get args :on-done))))
              ((symbol-function 'mcpkit-start-services-on-instance-port)
               (lambda () (setq started t))))
      (mcpkit-init-services)
      (should loaded)
      (should started))))

(ert-deftest mcpkit-services-test-init-takes-no-arguments ()
  "Both init functions are callable by a runner that passes no arguments."
  (should (= 0 (car (func-arity 'mcpkit-init-services))))
  (should (= 0 (car (func-arity 'mcpkit-init-services-sync)))))

(ert-deftest mcpkit-services-test-missing-module-is-logged-not-fatal ()
  "A module that cannot load is reported and skipped."
  (let ((msgs nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (mcpkit--require-module 'mcpkit-no-such-module))
    (should (string-match-p "skipping mcpkit-no-such-module" (car msgs)))))

;;; test-mcpkit-services.el ends here
