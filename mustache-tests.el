(require 'mustache)
(require 'ert)

(ert-deftest mustache-test-simple-string ()
  (should (equal "foo" (mustache-render "foo" nil))))

(defun mustache-run-tests ()
 (interactive)
 (ert-run-tests-interactively "mustache-test-"))

