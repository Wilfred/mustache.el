# mustache.el -- a mustache templating library in Emacs Lisp

Targeting [v.1.0.2](https://github.com/mustache/spec/tree/v1.0.2) of Mustache.

## Example usage

    (require 'ht) ;; hash table library

    (let ((context (ht ("name" "J. Random user")))
      ;; evaluates to: "Hello J. Random user!"
      (mustache-render "Hello {{name}}!" context))
      
## Implemented mustache features

* `{{variables}}`
* `{{#blocks}}`
* `{{^blocks}}`
* `{{& escapedvariables}}`
* `{{{escapedvariables}}}`
* `{{!comments}}`
* Errors on invalid templates (forgetting to close open sections etc)

## Todo:

* `{{> partials}}`
* `{{=different delimeters=}}`
* Functions in the context
* Errors on unclosed blocks
* Optional error on missing variables from the context
      
## Running tests

    M-x mustache-run-tests

## Other templating projects

* The `format` function (quick and dirty!)
* [esxml](https://github.com/tali713/esxml)
* [elnode](https://github.com/nicferrier/elnode) (docs [here](https://github.com/nicferrier/elnode#sending-files))

## Changelog

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
