# mustache.el
#### *a mustache templating library in Emacs Lisp*

[![Build Status](https://travis-ci.org/Wilfred/mustache.el.png?branch=master)](https://travis-ci.org/Wilfred/mustache.el)

Targeting [v.1.0.2](https://github.com/mustache/spec/tree/v1.0.2) of Mustache.

## Example usage

    (require 'mustache)
    (require 'ht) ;; hash table library

    (let ((context (ht ("name" "J. Random user"))))
      ;; evaluates to: "Hello J. Random user!"
      (mustache-render "Hello {{name}}!" context))

### (Optional) Without ht

You're not forced to use `ht`, it's just an easier way of creating
hash tables. You can use Emacs' reader syntax for hash tables instead:
      
    (require 'mustache)

    (let ((context
           #s(hash-table test equal data ("name" "J. Random user"))))
      ;; evaluates to: "Hello J. Random user!"
      (mustache-render "Hello {{name}}!" context))

Note that hash tables default to using `eql` as the key comparison
function. You must set it to `equal` since mustache.el uses hash
tables with string keys.

### Keywords in context

You can use keywords in contexts, which allows you to skip setting the
key comparison function.

    (require 'mustache)

    (let ((mustache-key-type 'keyword)
          (context
           #s(hash-table data (:name "J. Random user"))))
      ;; evaluates to: "Hello J. Random user!"
      (mustache-render "Hello {{name}}!" context))

## Implemented mustache features

Basic variable interpolation:

    (mustache-render
     "Coded with {{language}}!"
     (ht ("language" "elisp"))) ;; "Coded with elisp!"
     
Blocks with booleans:

    (mustache-render
     "{{#is-sunny}}Looks nice today.{{/is-sunny}}"
     (ht ("is-sunny" t))) ;; "Looks nice today."

    (mustache-render
     "{{#is-sunny}}Looks nice today.{{/is-sunny}}"
     (ht ("is-sunny" nil))) ;; ""
     
Blocks with hash tables:

    (mustache-render
     "{{#user}}{{name}}{{/user}}"
     (ht ("user"
          (ht ("name" "Wilfred"))))) ;; "Wilfred"
     
Blocks with lists:

    (mustache-render
     "{{#some-list}}{{item}}{{/some-list}}"
     (ht ("some-list"
          (list
           (ht ("item" "a"))
           (ht ("item" "b"))
           (ht ("item" "c")))))) ;; "abc"

Inverted blocks:

    (mustache-render
     "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
     (ht ("is-sunny" nil))) ;; "Take an umbrella!"

    (mustache-render
     "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
     (ht ("is-sunny" t))) ;; ""

Mustache variables are escaped:

    (mustache-render
     "{{info}}"
     (ht ("info" "<p>We use mustache</p>"))) ;; "&lt;p&gt;We use mustache&lt;/p&gt;"

Unless explicitly marked as safe:

    (mustache-render
     "{{{info}}}"
     (ht ("info" "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"

    (mustache-render
     "{{& info }}"
     (ht ("info" "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"

Comments:

    (mustache-render
     "hello{{! world }}"
     (ht)) ;; "hello"

Partials:

    ;; assuming ~/projects/mustache.el/test.mustache exists
    ;; and contains "hello {{user}}"
    (let ((mustache-partial-paths (list "~/projects/mustache.el")))
      (mustache-render
       "{{> test}}"
       (ht ("user" "wilfred")))) ;; "hello wilfred"

Changing delimeters:

    (mustache-render
     "{{=<% %>=}}<% style %>"
     (ht ("style" "ERB style!"))) ;; "ERB style!"

Lambdas:

    (mustache-render
     "{{#wrapped}}{{language}} is great.{{/wrapped}}"
     (ht ("language" "elisp")
         ("wrapped"
          (lambda (template context)
            (concat "<b>" (mustache-render template context) "</b>")))))
    ;; "<b>elisp is great.</b>"

Error checking on invalid sections:

    (mustache-render
     "{{#outer}}{{#inner}}mismatched!{{/outer}}{{/inner}}"
     (ht)) ;; error "Mismatched brackets: You closed a section with inner, but it wasn't open"

## Todo:

* Errors on unclosed tags
* Optional error on missing variables from the context
* Whitespace (in)sensitivity for windows newlines
* Run full specification test suite

## Developing mustache.el

mustache.el is broken up into several files, using `load` to import
them. If you have a checked-out copy of mustache.el, you will need to
do:

    (add-to-list 'load-path "~/projects/mustache.el/")
      
### Running tests

Within Emacs:

    M-x mustache-run-tests

Or from a command line (you need Cask installed):

    $ make test

### Roadmap

v1.0 -- Pass the full mustache v1.0.2 specification tests (excluding
optional parts).

## Other templating projects

* The `format` function (quick and dirty!)
* [esxml](https://github.com/tali713/esxml)
* [elnode](https://github.com/nicferrier/elnode) (docs [here](https://github.com/nicferrier/elnode#sending-files))
* `s-format` from [s.el](https://github.com/magnars/s.el)
* [xmlgen](https://github.com/philjackson/xmlgen)

## Changelog

* v0.23 Added the variable `mustache-key-type` which allows contexts
  with keyword for keys.
* v0.22 Fixed rendering of contexts with numeric values and
  `{{&escaped blocks}}`
* v0.21 Fixed rendering of contexts where the hash table contained
  number values.
* v0.20 improved whitespace insensitivity for `{{^blocks}}` and
  `{{!comments}}`
* v0.19 Fixed a bug where comment tags containing = were treated as
  changing the delimeters.
* v0.18 Improved whitespace (in)sensitivity
* v0.17 Fixed a bug where partial templates weren't found when
  `default-directory` was set.
* v0.16 Implemented lambdas.
* v0.15 Fixed partials not being recursively rendered, and added error
  checking for `mustache-partial-path` not being a list
* v0.14 Implemented changing tag delimeters e.g. `{{=<% %>=}}`
* v0.13 Improved whitespace sensitivity for `{{ variable }}`
  interpolation
* v0.12 Implemented partials `{{> foo}}`
* v0.11 Fixed nested contexts allowing access to the parent context
* v0.10 Whitespace sensitive now matches the mustache spec in more cases
* v0.9 Fixed rendering of `{{#blocks}}` when the context value is a
  hash table.
* v0.8 Removed dependency on `with-namespace` in favour of internal
  functions/variables being of the form `mst--foo`.
* v0.7 templates with malformed sections now throw an error. For
  example: `{{#foo}}{{/bar}}` and `{{#foo}}`.
* v0.6 `{{#blocks}}` now optionally support using vectors instead of
  lists
* v0.5 added rendering of `{{{blocks}}}`
* v0.4 internal functions are now named mustache--foo instead of
  mustache/foo due to use of the `with-namespace` macro
* v0.3 internal refactoring
* v0.2 added rendering `{{#blocks}}` when the context contains a list
  of hash tables
* v0.1 basic implementation of a subset of mustache
