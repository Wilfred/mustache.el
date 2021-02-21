;;; mustache.el --- Mustache templating library in emacs lisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.24
;; Keywords: convenience mustache template
;; Package-Requires: ((ht "0.9") (s "1.3.0") (dash "1.2.0"))
;; URL: https://github.com/Wilfred/mustache.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See documentation at https://github.com/Wilfred/mustache.el

;; Note on terminology: We treat mustache templates as a sequence of
;; strings (plain text), and tags (anything wrapped in delimeters:
;; {{foo}}). A section is a special tag that requires closing
;; (e.g. {{#foo}}{{/foo}}).

;; We treat mustache templates as if they conform to a rough grammar:

;; TEMPLATE = plaintext | TAG | SECTION | TEMPLATE TEMPLATE
;; SECTION = OPEN-TAG TEMPLATE CLOSE-TAG
;; TAG = "{{" text "}}"

;; Public functions are of the form `mustache-FOO`, private
;; functions/variables are of the form `mst--FOO`.

;;; Code:

(require 's)
(require 'ht)
(require 'dash)
(require 'cl-lib)

;; todo: add flag to set tolerance of missing variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (mst--render template context))

(defvar mustache-partial-paths nil
  "A list of paths to be searched for mustache partial templates (files ending .mustache).")

(defvar mustache-key-type 'string
  "What type of key we expect in contexts.
Can take the value 'string or 'keyword.

For 'string we expect contexts of the form:
#s(hash-table data (\"name\" \"J. Random user\"))

for 'keyword we expect contexts of the form:
#s(hash-table data (:name \"J. Random user\"))")

;;; Lexing 

(defun mst--lex (template)
  "Iterate through TEMPLATE, splitting {{ tags }} and bare strings.
We return a list of lists: ((:text \"foo\") (:tag \"variable-name\"))"
  ;; convert {{{foo}}} to {{& foo}}
  (setq template (replace-regexp-in-string "{{{\\(.*?\\)}}}" "{{& \\1}}" template))

  (let ((open-delimeter "{{")
        (close-delimeter "}}")
        (lexemes nil))
    (while (not (s-equals? template ""))
      (let* ((open-index (s-index-of open-delimeter template))
             (close-index (s-index-of close-delimeter template)))
        ;; todo: check open-index < close-index
        ;; todo: error if we have an open and no close
        (if (and open-index close-index)
            ;; we have a tag
            (let ((between-delimeters
                   (substring template (+ open-index (length open-delimeter)) close-index))
                  (continue-from-index (+ close-index (length close-delimeter))))
              ;; save the string before the tag
              (when (> open-index 0)
                (push (list :text (substring template 0 open-index)) lexemes))

              ;; if this is a tag that changes delimeters e.g. {{=<< >>=}}
              ;; then set the new open/close delimeter string
              (if (s-matches-p "^=.+ .+=$" between-delimeters)
                  (let* (;; strip leading/trailing =
                         (delimeter-spec (substring between-delimeters 1 -1))
                         (spec-parts (s-split " " delimeter-spec)))
                    (setq open-delimeter (cl-first spec-parts))
                    (setq close-delimeter (cl-second spec-parts)))

                ;; otherwise it's a normal tag, so save it
                (push (list :tag (s-trim between-delimeters)) lexemes))

              ;; iterate on the remaining template
              (setq template
                    (substring template continue-from-index)))
          ;; else only plain text left
          (progn
            (push (list :text template) lexemes)
            (setq template "")))))
    (nreverse lexemes)))

(defun mst--clean-whitespace (lexemes)
  "Given a list of LEXEMES, remove whitespace around sections and
comments if they're on their own on a line.  Modifies the original
list."
  ;; iterate over all lexemes:
  (cl-loop for i from 0 to (- (length lexemes) 3)
           collect
           ;; find sections that have plain text before and after
           (let ((first (elt lexemes i))
                 (second (elt lexemes (+ i 1)))
                 (third (elt lexemes (+ i 2))))
             (when (and (mst--text-p first)
                        (or
                         (mst--section-tag-p second)
                         (mst--comment-tag-p second))
                        (mst--text-p third)
                        ;; check the section is on its own line
                        (string-match-p "\n *$" (mst--tag-text first))
                        (string-match-p "^\n" (mst--tag-text third)))
               ;; then we cleanup whitespace
               (setf (elt lexemes i) (mst--no-trailing-newline first)))))
  lexemes)

(defalias 'mst--tag-text 'cl-second
  "Returns the text context of a tag.")

(defun mst--no-trailing-newline (lexeme)
  "Replace \"\n\" or \"\n   \" at the end of a plain text LEXEME."
  (list
   :text
   (replace-regexp-in-string "\n *$" "" (mst--tag-text lexeme))))

(defun mst--tag-p (lexeme)
  "Is LEXEME a tag?"
  (equal (car lexeme) :tag))

(defun mst--section-tag-p (lexeme)
  "Is LEXEME a section tag?"
  (or
   (mst--open-section-tag-p lexeme)
   (mst--inverted-section-tag-p lexeme)
   (mst--close-section-tag-p lexeme)))

(defun mst--open-section-tag-p (lexeme)
  "Is LEXEME an open section tag?
See also `mst--inverted-section-tag-p'."
  (and (mst--tag-p lexeme)
       (s-starts-with-p "#" (cl-second lexeme))))

(defun mst--inverted-section-tag-p (lexeme)
  "Is LEXEME an inverted section tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "^" (cl-second lexeme))))

(defun mst--close-section-tag-p (lexeme)
  "Is LEXEME a close section tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "/" (cl-second lexeme))))

(defun mst--comment-tag-p (lexeme)
  "Is LEXEME a comment tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p "!" (cl-second lexeme))))

(defun mst--unescaped-tag-p (lexeme)
  "Is LEXEME an unescaped variable tag?
Note that the lexer converts {{{foo}}} to {{& foo}}."
  (and (mst--tag-p lexeme)
       (s-starts-with-p "&" (cl-second lexeme))))

(defun mst--partial-tag-p (lexeme)
  "Is LEXEME a partial tag?"
  (and (mst--tag-p lexeme)
       (s-starts-with-p ">" (cl-second lexeme))))

(defun mst--section-p (lexeme)
  "Is LEXEME a nested section?"
  (not (atom (car lexeme))))

(defun mst--text-p (lexeme)
  "Is LEXEME plain text?"
  (equal (car lexeme) :text))

;; fixme: assumes the delimeters haven't changed
;; fixme: mst--lex doens't preserve whitespace
(defun mst--unlex (lexemes)
  "Given a lexed (and optionally parsed) list of LEXEMES,
return the original input string."
  (if lexemes
      (let ((lexeme (cl-first lexemes))
            (rest (cdr lexemes)))
        (cond
         ;; recurse on this section, then the rest
         ((mst--section-p lexeme)
          (concat (mst--unlex lexeme) (mst--unlex rest)))
         ((mst--tag-p lexeme)
          ;; restore the delimeters, then unlex the rest
          (let ((tag-name (cl-second lexeme)))
            (concat "{{" tag-name "}}" (mst--unlex rest))))
         ;; otherwise, it's just raw text
         (t
          (let ((text (cl-second lexeme)))
            (concat text (mst--unlex rest))))))
    ""))

;;; Parsing 

(defvar mst--remaining-lexemes nil
  "Since `mst--parse-inner' recursively calls itself, we need a shared value to mutate.")

(defun mst--parse (lexemes)
  "Given a list LEXEMES, return a list of lexemes nested according to #tags or ^tags."
  (setq mst--remaining-lexemes lexemes)
  (mst--parse-inner))

(defun mst--parse-inner (&optional section-name)
  "Parse `mst--remaining-lexemes', and return a list of lexemes nested according to #tags or ^tags."
  (let (parsed-lexemes
        lexeme)
    (cl-loop while mst--remaining-lexemes do
             (setq lexeme (pop mst--remaining-lexemes))
             (cond
              ((mst--open-section-p lexeme)
               ;; recurse on this nested section
               (push (cons lexeme (mst--parse-inner (mst--section-name lexeme))) parsed-lexemes))
              ((mst--close-section-p lexeme)
               ;; this is the last tag in this section
               (unless (equal section-name (mst--section-name lexeme))
                 (error "Mismatched brackets: You closed a section with %s, but it wasn't open" section-name))
               (push lexeme parsed-lexemes)
               (cl-return))
              (t
               ;; this is just a tag in the current section
               (push lexeme parsed-lexemes))))

    ;; ensure we aren't inside an unclosed section
    (when (and section-name (not (mst--close-section-p lexeme)))
      (error "Unclosed section: You haven't closed %s" section-name))

    (nreverse parsed-lexemes)))

(defun mst--open-section-p (lexeme)
  "Is LEXEME a #tag or ^tag ?"
  (cl-destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (or
          (s-starts-with-p "#" value)
          (s-starts-with-p "^" value)))))

(defun mst--close-section-p (lexeme)
  "Is LEXEME a /tag ?"
  (cl-destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (s-starts-with-p "/" value))))

(defun mst--section-name (lexeme)
  "Get the name of the section from LEXEME, a two part list returned by `mst--lex'.
The leading character (the #, ^ or /) is stripped."
  (s-chop-prefixes '("#" "^" "/") (cadr lexeme)))

;;; Rendering 

(defvar mustache-key-type)
(defvar mustache-partial-paths)

(defun mst--render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (-> template
      mst--lex
      mst--clean-whitespace
      mst--parse
      (mst--render-section-list context)))

(defun mst--mapconcat (function sequence)
  "Apply FUNCTION to every element in SEQUENCE, and concat the results as strings."
  (mapconcat function sequence ""))

(defmacro mst--amapconcat (form sequence)
  "Anaphoric version of `mst--mapconcat'."
  `(mst--mapconcat (lambda (it) ,form) ,sequence))

;; todo: set flag to set tolerance of missing templates
(defun mst--get-partial (name)
  "Get the first partial whose file name is NAME.mustache, or nil otherwise.
Partials are searched for in `mustache-partial-paths'."
  (cl-block nil
    (unless (listp mustache-partial-paths)
      (error "`mustache-partial-paths' must be a list of paths"))
    (let ((partial-name (format "%s.mustache" name)))
      (dolist (path mustache-partial-paths)
        (-when-let*
            ((partials (directory-files path nil "\\.mustache$"))
             (matching-partial (--first
                                (string-match-p (regexp-quote partial-name) it)
                                partials)))
          (cl-return
           (with-temp-buffer
             (insert-file-contents-literally (expand-file-name matching-partial path))
             (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun mst--render-section-list (sections context)
  "Render a parsed list SECTIONS in CONTEXT."
  (mst--amapconcat (mst--render-section it context) sections))

(defun mst--context-get (context variable-name &optional default)
  "Lookup VARIABLE-NAME in CONTEXT, returning DEFAULT if not present."
  (when (eq mustache-key-type 'keyword)
    (setq variable-name (intern (concat ":" variable-name))))
  (ht-get context variable-name default))

(defun mst--render-tag (parsed-tag context)
  "Given PARSED-TAG, render it in hash table CONTEXT."
  (let ((inner-text (mst--tag-text parsed-tag)))
    (cond
     ((mst--comment-tag-p parsed-tag)
      "")
     ((mst--unescaped-tag-p parsed-tag)
      (let ((variable-value (mst--context-get context (s-trim (substring inner-text 1)) "")))
        (when (numberp variable-value)
          (setq variable-value (number-to-string variable-value)))
        variable-value))
     ((mst--partial-tag-p parsed-tag)
      (let ((partial (mst--get-partial (s-trim (substring inner-text 1)))))
        (if partial
            (mst--render partial context)
          "")))
     (t ;; normal variable
      (let ((variable-value (mst--context-get context inner-text "")))
        (when (numberp variable-value)
          (setq variable-value (number-to-string variable-value)))
        (mst--escape-html variable-value))))))

(defun mst--context-add (table from-table)
  "Return a copy of TABLE where all the key-value pairs in FROM-TABLE have been set."
  (let ((new-table (ht-copy table)))
    (ht-update new-table from-table)
    new-table))

(defun mst--listp (object)
  "Return t if OBJECT is a list.
Unlike `listp', does not return t if OBJECT is a function."
  (and (not (functionp object)) (listp object)))

(defun mst--section-name (section-tag)
  "Get the name of this SECCTION-TAG.
E.g. from {{#foo}} to \"foo\"."
  (-> section-tag ;; e.g (:tag "#foo")
      mst--tag-text ;; to "#foo"
      (substring 1) ;; to "foo"
      s-trim))

(defun mst--render-section (parsed-lexeme context)
  "Given PARSED-LEXEME -- a lexed tag, plain text, or a nested list,
render it in CONTEXT."
  (cond ((mst--section-p parsed-lexeme)
         ;; nested section
         (let* ((section-tag (cl-first parsed-lexeme))
                (section-name (mst--section-name section-tag))
                (context-value (mst--context-get context section-name))
                ;; strip section open and close
                (section-contents (-slice parsed-lexeme 1 -1)))
           (cond
            ((mst--open-section-tag-p section-tag)
             (cond
              ;; if the context is a list of hash tables, render repeatedly
              ((or (mst--listp context-value) (vectorp context-value))
               (mst--amapconcat (mst--render-section-list section-contents (mst--context-add context it)) context-value))
              ;; if the context is a hash table, render in that context
              ((hash-table-p context-value)
               (mst--render-section-list section-contents (mst--context-add context context-value)))
              ;; if the context is a function, call it
              ((functionp context-value)
               (funcall context-value (mst--unlex section-contents) context))
              ;; if it's a truthy value, render in the current context
              (context-value
               (mst--render-section-list section-contents context))
              ;; otherwise, don't render anything
              (t "")))
            ;; render ^tags
            ((mst--inverted-section-tag-p section-tag)
             (if context-value
                 ""
               (mst--render-section-list section-contents context))))))
        ((mst--tag-p parsed-lexeme)
         (mst--render-tag parsed-lexeme context))
        ;; plain text
        (t
         (cl-second parsed-lexeme))))

(defun mst--escape-html (string)
  "Escape HTML in STRING."
  (->> string
       (s-replace "&" "&amp;")
       (s-replace "<" "&lt;")
       (s-replace ">" "&gt;")
       (s-replace "'" "&#39;")
       (s-replace "\"" "&quot;")))

(provide 'mustache)
;;; mustache.el ends here
