# mustache.el -- a mustache templating library in Emacs Lisp

GPLv2 licensed.

## Example usage

    (require 'ht) ;; hash table library

    (let ((context (ht-create)))
      (ht-set context "name" "J. Random user")
      ;; evaluates to: "Hello J. Random user!"
      (mustache-render "Hello {{name}}!" context))
      
## Running tests

M-x mustache-run-tests

## Other templating projects

* The `format` function (quick and dirty!)
* [esxml](https://github.com/tali713/esxml)
* [elnode](https://github.com/nicferrier/elnode) (docs [here](https://github.com/nicferrier/elnode#sending-files))

## Changelog

* v0.2 added rendering `{{#blocks}}` when the context contains a list
  of hash tables
* v0.1 basic implementation of a subset of mustache
