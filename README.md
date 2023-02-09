# mustache.el [![Coverage Status](https://coveralls.io/repos/github/Wilfred/mustache.el/badge.svg)](https://coveralls.io/github/Wilfred/mustache.el)

#### _a mustache templating library in Emacs Lisp_

Targeting [v.1.0.2](https://github.com/mustache/spec/tree/v1.0.2) of Mustache.

## Example usage

```emacs-lisp
(require 'mustache)

(let ((context '(("name" . "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

`context` can be anything that Emacs's [map manipulation
functions](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/map.el)
accept: alists (as in the example above), hash tables, and (if using keyword
arguments — see below) plists.

Example with hash tables:

```emacs-lisp
(let ((context
       #s(hash-table test equal data ("name" "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

### Keywords in context

You can use keywords in alist and plist contexts:

```emacs-lisp
(require 'mustache)

;; Using an alist
(let ((mustache-key-type 'keyword)
      (context '((:name . "J. Random user"))))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))

;; Using a plist
(let ((mustache-key-type 'keyword)
      (context '(:name "J. Random user")))
  ;; evaluates to: "Hello J. Random user!"
  (mustache-render "Hello {{name}}!" context))
```

## Implemented mustache features

Basic variable interpolation:

```emacs-lisp
(mustache-render
 "Coded with {{language}}!"
 '(("language" . "elisp"))) ;; "Coded with elisp!"
```

Blocks with booleans:

```emacs-lisp
(mustache-render
 "{{#is-sunny}}Looks nice today.{{/is-sunny}}"
 '(("is-sunny" . t))) ;; "Looks nice today."
```

Blocks with maps:

```emacs-lisp
;; Using alists
(mustache-render
 "{{#user}}{{name}}{{/user}}"
 '(("user" ("name" . "Wilfred")))) ;; "Wilfred"

 ;; Using plists
(let ((mustache-key-type 'keyword))
  (mustache-render
    "{{#user}}{{name}}{{/user}}"
    '(:user (:name "Wilfred")))) ;; "Wilfred"
```

Blocks with lists:

```emacs-lisp
;; Using alists
(mustache-render
 "{{#some-list}}{{item}}{{/some-list}}"
 '(("some-list" . ((("item" . "a"))
                   (("item" . "b"))
                   (("item" . "c")))))) ;; "abc"

;; Using plists
(let ((mustache-key-type 'keyword))
  (mustache-render
   "{{#some-list}}{{item}}{{/some-list}}"
   '(:some-list ((:item "a")
                 (:item "b")
                 (:item "c"))))) ;; "abc"
```

Inverted blocks:

```emacs-lisp
(mustache-render
 "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
 '(("is-sunny" . nil))) ;; "Take an umbrella!"

(mustache-render
 "{{^is-sunny}}Take an umbrella!{{/is-sunny}}"
 '(("is-sunny" . t))) ;; ""
```

Mustache variables are escaped:

```emacs-lisp
(mustache-render
 "{{info}}"
 '(("info" . "<p>We use mustache</p>"))) ;; "&lt;p&gt;We use mustache&lt;/p&gt;"
```

Unless explicitly marked as safe:

```emacs-lisp
(mustache-render
 "{{{info}}}"
 '(("info" . "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"

(mustache-render
 "{{& info }}"
 '(("info" . "<p>We use mustache</p>"))) ;; "<p>We use mustache</p>"
```

Comments:

```emacs-lisp
(mustache-render
 "hello{{! world }}"
 '()) ;; "hello"
```

Partials:

```emacs-lisp
;; assuming ~/projects/mustache.el/test.mustache exists
;; and contains "hello {{user}}"
(let ((mustache-partial-paths (list "~/projects/mustache.el")))
  (mustache-render
   "{{> test}}"
   '(("user" . "wilfred")))) ;; "hello wilfred"
```

Changing delimeters:

```emacs-lisp
(mustache-render
 "{{=<% %>=}}<% style %>"
 '(("style" . "ERB style!"))) ;; "ERB style!"
```

Lambdas:

```emacs-lisp
(mustache-render
 "{{#wrapped}}{{language}} is great.{{/wrapped}}"
 `(("language" . "elisp")
   ("wrapped" .
    ,(lambda (template context)
      (concat "<b>" (mustache-render template context) "</b>")))))
;; "<b>elisp is great.</b>"
```

Error checking on invalid sections:

```emacs-lisp
(mustache-render
 "{{#outer}}{{#inner}}mismatched!{{/outer}}{{/inner}}"
 '()) ;; error "Mismatched brackets: You closed a section with inner, but it wasn't open"
```

## Todo:

- Errors on unclosed tags
- Optional error on missing variables from the context
- Whitespace (in)sensitivity for windows newlines
- Run full specification test suite

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

- The `format` function (quick and dirty!)
- [esxml](https://github.com/tali713/esxml)
- [elnode](https://github.com/nicferrier/elnode) (docs [here](https://github.com/nicferrier/elnode#sending-files))
- `s-format` from [s.el](https://github.com/magnars/s.el)
- [xmlgen](https://github.com/philjackson/xmlgen)
