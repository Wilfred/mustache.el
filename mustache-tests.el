(require 'mustache)
(require 'ert)
(require 'ht)

(ert-deftest mustache-test-simple-string ()
  (should (equal "foo" (mustache-render "foo" nil))))

(ert-deftest mustache-test-variable ()
  (let ((context (ht-create)))
    (ht-set context "blah" "bar")
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{blah}}" context)))))

(defun mustache-run-tests ()
 (interactive)
 (ert-run-tests-interactively "mustache-test-"))

