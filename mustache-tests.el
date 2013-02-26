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

(ert-deftest mustache-test-variable-escaped ()
  (let ((context (ht-create)))
    (ht-set context "blah" "<bar> &baz ' \"")
    (should
     (equal
      "&lt;bar&gt; &amp;baz &#39; &quot;"
      (mustache-render "{{blah}}" context)))))

(ert-deftest mustache-test-conditional-true ()
    (let ((context (ht-create)))
      (ht-set context "yes" 't)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{#yes}}bar{{/yes}}" context)))))

(ert-deftest mustache-test-conditional-false ()
    (let ((context (ht-create)))
      (ht-set context "no" nil)
      (should
       (equal
        "foo "
        (mustache-render "foo {{#no}}bar{{/no}}" context)))))

(defun mustache-run-tests ()
 (interactive)
 (ert-run-tests-interactively "mustache-test-"))

(provide 'mustache-tests)
;;; mustache-tests.el ends here
