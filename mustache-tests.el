(require 'mustache)
(require 'ert)
(require 'ht)

(ert-deftest mustache-test-simple-string ()
  (should (equal "foo" (mustache-render "foo" nil))))

(ert-deftest mustache-test-variable ()
  (let ((context (ht ("blah" "bar"))))
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{blah}}" context)))))

(ert-deftest mustache-test-variable-number ()
  (let ((context (ht ("blah" 2))))
    (should
     (equal
      "foo 2"
      (mustache-render "foo {{blah}}" context)))))

(ert-deftest mustache-test-unescaped-variable-number ()
  (let ((context (ht ("blah" 2))))
    (should
     (equal
      "foo 2"
      (mustache-render "foo {{{blah}}}" context)))))

(ert-deftest mustache-test-variable-whitespace ()
  (let ((context (ht ("blah" "bar"))))
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{ blah }}" context)))))

(ert-deftest mustache-test-variable-missing ()
  (let ((context (ht-create)))
    (should
     (equal
      "foo "
      (mustache-render "foo {{blah}}" context)))))

(ert-deftest mustache-test-keyword-variable ()
  (let ((mustache-key-type 'keyword)
        (context (ht (:blah "bar"))))
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{blah}}" context)))))

(ert-deftest mustache-test-section-inner-whitespace ()
  (should
   (equal
    "="
    (mustache-render "{{# boolean }}={{/ boolean }}" (ht ("boolean" t))))))

(ert-deftest mustache-test-standalone-lines ()
  (should
   (equal "| This Is
|
| A Line
"
    (mustache-render "| This Is
{{#boolean}}
|
{{/boolean}}
| A Line
" (ht ("boolean" t))))))

(ert-deftest mustache-test-standalone-lines-inverted ()
  (should
   (equal "| This Is
|
| A Line
"
    (mustache-render "| This Is
{{^boolean}}
|
{{/boolean}}
| A Line
" (ht ("boolean" nil))))))

(ert-deftest mustache-test-standalone-lines-leading-whitespace ()
  (should
   (equal "| This Is
|
| A Line
"
    (mustache-render "| This Is
  {{#boolean}}
|
  {{/boolean}}
| A Line
" (ht ("boolean" t))))))

(ert-deftest mustache-test-standalone-lines-comments ()
  (should
   (equal "| This Is
|
| A Line
"
    (mustache-render "| This Is
|
  {{!comment}}
| A Line
" (ht)))))

(ert-deftest mustache-test-extra-section-close ()
  (should-error
   (mustache-render "{{/blah}}" (ht-create))))

(ert-deftest mustache-test-extra-section-open ()
  (should-error
   (mustache-render "{{#blah}}" (ht-create))))

(ert-deftest mustache-test-mismatched-section-close ()
  (should-error
   (mustache-render "{{#blah}}{{/foo}}" (ht-create))))

(ert-deftest mustache-test-variable-escaped ()
  (let ((context (ht ("blah" "<bar> &baz ' \""))))
    (should
     (equal
      "&lt;bar&gt; &amp;baz &#39; &quot;"
      (mustache-render "{{blah}}" context)))))

(ert-deftest mustache-test-section-hash ()
  (let ((context (ht ("user"  (ht ("name" "bob"))))))
    (should
     (equal
      "bob"
      (mustache-render "{{#user}}{{name}}{{/user}}" context)))))

(ert-deftest mustache-test-section-hash-nested ()
  (let ((context (ht
                  ("foo" "bar")
                  ("user"  (ht ("name" "bob"))))))
    (should
     (equal
      "barbob"
      (mustache-render "{{#user}}{{foo}}{{name}}{{/user}}" context)))))

(ert-deftest mustache-test-section-list ()
  (let ((context (ht ("users" (list (ht ("name" "bob"))
                                    (ht ("name" "chris")))))))
    (should
     (equal
      "bobchris"
      (mustache-render "{{#users}}{{name}}{{/users}}" context)))))

(ert-deftest mustache-test-section-with-vector ()
  "Vectors should behave the same as a list in a context."
  (let ((context (ht ("users" `[,(ht ("name" "bob"))
                                ,(ht ("name" "chris"))]))))
    (should
     (equal
      "bobchris"
      (mustache-render "{{#users}}{{name}}{{/users}}" context)))))

(ert-deftest mustache-test-unescaped ()
  (let ((context (ht ("blah" "<bar>"))))
    (should
     (equal
      "<bar>"
      (mustache-render "{{& blah}}" context)))))

(ert-deftest mustache-test-triple-mustache ()
  (let ((context (ht ("blah" "<bar>"))))
    (should
     (equal
      "<bar>"
      (mustache-render "{{{blah}}}" context)))))

(ert-deftest mustache-test-conditional-true ()
  (let ((context (ht ("yes" 't))))
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{#yes}}bar{{/yes}}" context)))))

(ert-deftest mustache-test-conditional-false ()
  (let ((context (ht ("no" nil))))
    (should
     (equal
      "foo "
      (mustache-render "foo {{#no}}bar{{/no}}" context)))))

(ert-deftest mustache-test-conditional-keyword ()
  (let ((mustache-key-type 'keyword)
        (context (ht (:no nil))))
    (should
     (equal
      "foo "
      (mustache-render "foo {{#no}}bar{{/no}}" context)))))

(ert-deftest mustache-test-inverted ()
  (let ((context (ht ("no" nil))))
    (should
     (equal
      "foo bar"
      (mustache-render "foo {{^no}}bar{{/no}}" context)))))

(ert-deftest mustache-test-comment ()
  (let ((context (ht-create)))
    (should
     (equal
      ""
      (mustache-render "{{! whatever}}" context)))))

(ert-deftest mustache-test-after-comment ()
  "Ensure we render tags after comments.
Regression test for https://github.com/Wilfred/mustache.el/issues/4."
  (let ((context  (ht ("b" "hello"))))
    (should
     (equal
      "hello"
      (mustache-render
 "{{!<script src=\"somescript.js\" async=\"async\"}}{{b}}" context)))))

(ert-deftest mustache-test-partial ()
  (let ((mustache-partial-paths (list default-directory)))
    (should
     (equal
      "hello world"
      (mustache-render "{{> partial }}" (ht))))))

(ert-deftest mustache-test-partial-path ()
  "Ensure we get the correct partial path, regardless of `default-directory'.
Regression test for https://github.com/Wilfred/mustache.el/pull/3"
  (let ((mustache-partial-paths (list default-directory))
        (default-directory "/"))
    (should
     (equal
      "hello world"
      (mustache-render "{{> partial }}" (ht))))))

(ert-deftest mustache-test-partial-rendered ()
  "Test that we render the contents of the partial as a mustache template."
  (let ((mustache-partial-paths (list default-directory)))
    (should
     (equal
      "hello world"
      (mustache-render
       "{{> partial-with-variables }}"
       (ht ("thing" "world")))))))

(ert-deftest mustache-test-change-delimeter ()
  (should
   (equal
    "bar"
    (mustache-render "{{=<< >>=}}<< foo >>" (ht ("foo" "bar"))))))

(ert-deftest mustache-test-lambda ()
  (should
   (equal
    "<b>Willy is awesome.</b>"
    (mustache-render
     "{{#wrapped}}{{name}} is awesome.{{/wrapped}}"
     (ht ("name" "Willy")
         ("wrapped"
          (lambda (template context)
            (concat "<b>" (mustache-render template context) "</b>"))))))))

(defun mustache-run-tests ()
  (interactive)
  (ert-run-tests-interactively "mustache-test-"))

(provide 'mustache-tests)
;;; mustache-tests.el ends here
