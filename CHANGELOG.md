* v0.24 No changes yet.
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
