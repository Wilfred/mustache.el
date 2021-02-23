;;; mustache-test.el --- Mustache test module

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/mustache.el

;; This file is part of mustache.el.

;; mustache.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mustache.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mustache.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Mustache test module.

;;; Code:

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

(ert-deftest mustache-malformed-tag ()
  "Don't crash if we have a malformed tag.
Arguably we could error, but mustache generally errs on error
tolerance, e.g. allowing missing variables."
  (mustache-render "}} foo {{" (ht)))

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

(ert-deftest mustache-test-triple-mustache-custom-delimiter ()
  (let ((context (ht ("name" "wilfred"))))
    (should
     (equal
      "{{{name}}}"
      (mustache-render "{{=<< >>=}}{{{name}}}" context)))))

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
