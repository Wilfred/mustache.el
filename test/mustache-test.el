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

(ert-deftest mustache-test-simple-string ()
  (should (equal "foo" (mustache-render "foo" nil))))

(ert-deftest mustache-test-variable ()
  (let ((contexts (list #s(hash-table test equal data ("blah" "bar"))
                        '(("blah" . "bar")))))
    (dolist (context contexts)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{blah}}" context))))))

(ert-deftest mustache-malformed-tag ()
  "Don't crash if we have a malformed tag.
Arguably we could error, but mustache generally errs on error
tolerance, e.g. allowing missing variables."
  (mustache-render "}} foo {{"  #s(hash-table)))

(ert-deftest mustache-test-variable-number ()
  (let ((contexts (list #s(hash-table test equal data ("blah" 2))
                        '(("blah" . 2)))))
    (dolist (context contexts)
      (should
       (equal
        "foo 2"
        (mustache-render "foo {{blah}}" context))))))

(ert-deftest mustache-test-unescaped-variable-number ()
  (let ((contexts (list #s(hash-table test equal data ("blah" 2))
                        '(("blah" . 2)))))
    (dolist (context contexts)
      (should
       (equal
        "foo 2"
        (mustache-render "foo {{{blah}}}" context))))))

(ert-deftest mustache-test-variable-whitespace ()
  (let ((contexts (list #s(hash-table test equal data ("blah" "bar"))
                        '(("blah" . "bar")))))
    (dolist (context contexts)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{ blah }}" context))))))

(ert-deftest mustache-test-variable-missing ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should
       (equal
        "foo "
        (mustache-render "foo {{blah}}" context))))))

(ert-deftest mustache-test-keyword-variable ()
  (let ((mustache-key-type 'keyword)
        (contexts (list #s(hash-table test equal data (:blah "bar"))
                        '(:blah "bar")
                        '((:blah . "bar")))))
    (dolist (context contexts)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{blah}}" context))))))

(ert-deftest mustache-test-section-inner-whitespace ()
  (let ((contexts (list #s(hash-table test equal data ("boolean" t))
                        '(("boolean" . t)))))
    (dolist (context contexts)
      (should
       (equal
        "="
        (mustache-render "{{# boolean }}={{/ boolean }}" context))))))

(ert-deftest mustache-test-standalone-lines ()
  (let ((contexts (list #s(hash-table test equal data ("boolean" t))
                        '(("boolean" . t)))))
    (dolist (context contexts)
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
" context))))))

(ert-deftest mustache-test-standalone-lines-inverted ()
  (let ((contexts (list #s(hash-table test equal data ("boolean" nil))
                        '(("boolean" . nil)))))
    (dolist (context contexts)
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
" context))))))

(ert-deftest mustache-test-standalone-lines-leading-whitespace ()
  (let ((contexts (list #s(hash-table test equal data ("boolean" t))
                        '(("boolean" . t)))))
    (dolist (context contexts)
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
" context))))))

(ert-deftest mustache-test-standalone-lines-comments ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should
       (equal "| This Is
|
| A Line
"
              (mustache-render "| This Is
|
  {{!comment}}
| A Line
" context))))))

(ert-deftest mustache-test-extra-section-close ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should-error
       (mustache-render "{{/blah}}" context)))))

(ert-deftest mustache-test-extra-section-open ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should-error
       (mustache-render "{{#blah}}" context)))))

(ert-deftest mustache-test-mismatched-section-close ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should-error
       (mustache-render "{{#blah}}{{/foo}}" context)))))

(ert-deftest mustache-test-variable-escaped ()
  (let ((contexts (list (ht ("blah" "<bar> &baz ' \""))
                        #s(hash-table test equal data ("blah" "<bar> &baz ' \""))
                        '(("blah" . "<bar> &baz ' \"")))))
    (dolist (context contexts)
      (should
       (equal
        "&lt;bar&gt; &amp;baz &#39; &quot;"
        (mustache-render "{{blah}}" context))))))

(ert-deftest mustache-test-section-hash ()
  (let ((contexts (list #s(hash-table test equal
                                      data ("user"
                                            #s(hash-table test equal
                                                          data ("name" "bob"))))
                        '(("user" ("name" . "bob"))))))
    (dolist (context contexts)
      (should
       (equal
        "bob"
        (mustache-render "{{#user}}{{name}}{{/user}}" context))))))

(ert-deftest mustache-test-section-hash-nested ()
  (let ((contexts (list #s(hash-table test equal
                                      data ("foo" "bar"
                                            "user" #s(hash-table test equal
                                                                 data ("name" "bob"))))
                        '(("foo" . "bar")
                          ("user" ("name" . "bob"))))))
    (dolist (context contexts)
      (should
       (equal
        "barbob"
        (mustache-render "{{#user}}{{foo}}{{name}}{{/user}}" context))))))

(ert-deftest mustache-test-section-list ()
  (let ((contexts (list (let ((hash #s(hash-table test equal)))
                          (puthash "users"
                                   (list #s(hash-table test equal
                                                       data ("name" "bob"))
                                         #s(hash-table test equal
                                                       data ("name" "chris")))
                                   hash)
                          hash)
                        '(("users" . ((("name" . "bob"))
                                      (("name" . "chris"))))))))
    (dolist (context contexts)
      (should
       (equal
        "bobchris"
        (mustache-render "{{#users}}{{name}}{{/users}}" context))))))

(ert-deftest mustache-test-section-with-vector ()
  "Vectors should behave the same as a list in a context."
  (let ((contexts (list (let ((hash #s(hash-table test equal)))
                          (puthash "users"
                                   (vector #s(hash-table test equal
                                                         data ("name" "bob"))
                                           #s(hash-table test equal
                                                         data ("name" "chris")))
                                   hash)
                          hash)
                        `(("users" . [(("name" . "bob"))
                                      (("name" . "chris"))])))))
    (dolist (context contexts)
      (should
       (equal
        "bobchris"
        (mustache-render "{{#users}}{{name}}{{/users}}" context))))))

(ert-deftest mustache-test-unescaped ()
  (let ((contexts (list #s(hash-table test equal data ("blah" "<bar>"))
                        '(("blah" . "<bar>")))))
    (dolist (context contexts)
      (should
       (equal
        "<bar>"
        (mustache-render "{{& blah}}" context))))))

(ert-deftest mustache-test-triple-mustache ()
  (let ((contexts (list #s(hash-table test equal data ("blah" "<bar>"))
                        '(("blah" . "<bar>")))))
    (dolist (context contexts)
      (should
       (equal
        "<bar>"
        (mustache-render "{{{blah}}}" context))))))

(ert-deftest mustache-test-triple-mustache-custom-delimiter ()
  (let ((contexts (list #s(hash-table test equal data ("name" "wilfred"))
                        '(("name" . "wilfred")))))
    (dolist (context contexts)
      (should
       (equal
        "{{{name}}}"
        (mustache-render "{{=<< >>=}}{{{name}}}" context))))))

(ert-deftest mustache-test-conditional-true ()
  (let ((contexts (list #s(hash-table test equal data ("yes" t))
                        '(("yes" . t)))))
    (dolist (context contexts)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{#yes}}bar{{/yes}}" context))))))

(ert-deftest mustache-test-conditional-false ()
  (let ((contexts (list #s(hash-table test equal data ("no" nil))
                        '(("no" . nil)))))
    (dolist (context contexts)
      (should
       (equal
        "foo "
        (mustache-render "foo {{#no}}bar{{/no}}" context))))))

(ert-deftest mustache-test-conditional-keyword ()
  (let ((mustache-key-type 'keyword)
        (contexts (list #s(hash-table test equal data (:no nil))
                        '(:no nil)
                        '((:no . nil)))))
    (dolist (context contexts)
      (should
       (equal
        "foo "
        (mustache-render "foo {{#no}}bar{{/no}}" context))))))

(ert-deftest mustache-test-inverted ()
  (let ((contexts (list #s(hash-table test equal data ("no" nil))
                        '(("no" . nil)))))
    (dolist (context contexts)
      (should
       (equal
        "foo bar"
        (mustache-render "foo {{^no}}bar{{/no}}" context))))))

(ert-deftest mustache-test-comment ()
  (let ((contexts (list #s(hash-table)
                        '())))
    (dolist (context contexts)
      (should
       (equal
        ""
        (mustache-render "{{! whatever}}" context))))))

(ert-deftest mustache-test-after-comment ()
  "Ensure we render tags after comments.
Regression test for https://github.com/Wilfred/mustache.el/issues/4."
  (let ((contexts (list #s(hash-table test equal data ("b" "hello"))
                        '(("b" . "hello")))))
    (dolist (context contexts)
      (should
       (equal
        "hello"
        (mustache-render
         "{{!<script src=\"somescript.js\" async=\"async\"}}{{b}}" context))))))

(ert-deftest mustache-test-partial ()
  (let ((contexts (list #s(hash-table)
                        '()))
        (mustache-partial-paths (list default-directory)))
    (dolist (context contexts)
      (should
       (equal
        "hello world"
        (mustache-render "{{> partial }}" context))))))

(ert-deftest mustache-test-partial-path ()
  "Ensure we get the correct partial path, regardless of `default-directory'.
Regression test for https://github.com/Wilfred/mustache.el/pull/3"
  (let ((contexts (list #s(hash-table)
                        '()))
        (mustache-partial-paths (list default-directory))
        (default-directory "/"))
    (dolist (context contexts)
      (should
       (equal
        "hello world"
        (mustache-render "{{> partial }}" context))))))

(ert-deftest mustache-test-partial-rendered ()
  "Test that we render the contents of the partial as a mustache template."
  (let ((contexts (list #s(hash-table test equal data ("thing" "world"))
                        '(("thing" . "world"))))
        (mustache-partial-paths (list default-directory)))
    (dolist (context contexts)
      (should
       (equal
        "hello world"
        (mustache-render
         "{{> partial-with-variables }}"
         context))))))

(ert-deftest mustache-test-change-delimeter ()
  (let ((contexts (list '(("foo" . "bar")))))
    (dolist (context contexts)
      (should
       (equal
        "bar"
        (mustache-render "{{=<< >>=}}<< foo >>" context))))))

(ert-deftest mustache-test-lambda ()
  (let* ((wrapped (lambda (template context)
                    (concat "<b>" (mustache-render template context) "</b>")))
         (contexts (list (let ((hash #s(hash-table test equal data ("name" "Willy"))))
                           (puthash "wrapped" wrapped hash)
                           hash)
                         `(("name" . "Willy")
                           ("wrapped" . ,wrapped)))))
    (dolist (context contexts)
      (should
       (equal
        "<b>Willy is awesome.</b>"
        (mustache-render
         "{{#wrapped}}{{name}} is awesome.{{/wrapped}}" context))))))

(defun mustache-run-tests ()
  (interactive)
  (ert-run-tests-interactively "mustache-test-"))

(provide 'mustache-tests)
;;; mustache-tests.el ends here
