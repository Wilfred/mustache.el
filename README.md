# mustache.el
#### *a mustache templating library in Emacs Lisp*

Targeting [v.1.0.2](https://github.com/mustache/spec/tree/v1.0.2) of Mustache.

## Example usage

``` emacs-lisp
(require 'mustache)
(require 'ht) ;; hash table library

(let ((context (ht ("name" "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

### (Optional) Without ht

You're not forced to use `ht`, it's just an easier way of creating
hash tables. You can use Emacs' reader syntax for hash tables instead:
      
``` emacs-lisp
(require 'mustache)

(let ((context
       #s(hash-table test equal data ("name" "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

Note that hash tables default to using `eql` as the key comparison
function. You must set it to `equal` since mustache.el uses hash
tables with string keys.

### Keywords in context

You can use keywords in contexts, which allows you to skip setting the
key comparison function.

``` emacs-lisp
(require 'mustache)

(let ((mustache-key-type 'keyword)
      (context
       #s(hash-table data (:name "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

## Implemented mustache features

Basic variable interpolation:

``` emacs-lisp
(mustache-render
 "Coded with {{language}}!"
 (ht ("language" "elisp"))) ;; "Coded with elisp!"
```
     
Blocks with booleans:

``` emacs-lisp
(mustache-render
 "{{#is-sunny}}Looks nice today.{{/is-sunny}}"
 (ht ("is-sunny" t))) ;; "Looks nice today."

(mustache-render
 "{{#is-sunny}}Looks nice today.{{/is-sunny}}"
 (ht ("is-sunny" nil))) ;; ""
```
     
Blocks with hash tables:

``` emacs-lisp
(mustache-render
 "{{#user}}{{name}}{{/user}}"
 (ht ("user"
      (ht ("name" "Wilfred"))))) ;; "Wilfred"
```
     
Blocks with lists:

``` emacs-lisp
(mustache-render
 "{{#some-list}}{{item}}{{/some-list}}"
 (ht ("some-list"
      (list
       (ht ("item" "a"))
       (ht ("item" "b"))
       (ht ("item" "c")))))) ;; "abc"
```

Inverted blocks:

``` emacs-lisp
(mustache-render
 "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
 (ht ("is-sunny" nil))) ;; "Take an umbrella!"

(mustache-render
 "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
 (ht ("is-sunny" t))) ;; ""
```

Mustache variables are escaped:

``` emacs-lisp
(mustache-render
 "{{info}}"
 (ht ("info" "<p>We use mustache</p>"))) ;; "&lt;p&gt;We use mustache&lt;/p&gt;"
```

Unless explicitly marked as safe:

``` emacs-lisp
(mustache-render
 "{{{info}}}"
 (ht ("info" "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"

(mustache-render
 "{{& info }}"
 (ht ("info" "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"
```

Comments:

``` emacs-lisp
(mustache-render
 "hello{{! world }}"
 (ht)) ;; "hello"
```

Partials:

``` emacs-lisp
;; assuming ~/projects/mustache.el/test.mustache exists
;; and contains "hello {{user}}"
(let ((mustache-partial-paths (list "~/projects/mustache.el")))
  (mustache-render
   "{{> test}}"
   (ht ("user" "wilfred")))) ;; "hello wilfred"
```

Changing delimeters:

``` emacs-lisp
(mustache-render
 "{{=<% %>=}}<% style %>"
 (ht ("style" "ERB style!"))) ;; "ERB style!"
```

Lambdas:

``` emacs-lisp
(mustache-render
 "{{#wrapped}}{{language}} is great.{{/wrapped}}"
 (ht ("language" "elisp")
     ("wrapped"
      (lambda (template context)
        (concat "<b>" (mustache-render template context) "</b>")))))
;; "<b>elisp is great.</b>"
```

Error checking on invalid sections:

``` emacs-lisp
(mustache-render
 "{{#outer}}{{#inner}}mismatched!{{/outer}}{{/inner}}"
 (ht)) ;; error "Mismatched brackets: You closed a section with inner, but it wasn't open"
```

## Todo:

* Errors on unclosed tags
* Optional error on missing variables from the context
* Whitespace (in)sensitivity for windows newlines
* Run full specification test suite

### Running tests

Within Emacs:

    M-x mustache-run-tests

Or from a command line (you need Cask installed):

    $ cask
    $ cask exec ert-runner

### Roadmap

v1.0 -- Pass the full mustache v1.0.2 specification tests (excluding
optional parts).

## Other templating projects

* The `format` function (quick and dirty!)
* [esxml](https://github.com/tali713/esxml)
* [elnode](https://github.com/nicferrier/elnode) (docs [here](https://github.com/nicferrier/elnode#sending-files))
* `s-format` from [s.el](https://github.com/magnars/s.el)
* [xmlgen](https://github.com/philjackson/xmlgen)
