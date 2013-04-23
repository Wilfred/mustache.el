;;; mustache.el -- a mustache templating library in emacs lisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.8
;; Keywords: mustache, template
;; Package-Requires: ((ht "0.8") (s "1.3.0") (dash "1.1.0"))

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

;; TEMPLATE = plaintext | TAG | SECTION | TEMPLATE
;; SECTION = OPEN-TAG TEMPLATE CLOSE-TAG
;; TAG = "{{" text "}}"

;; Public functions are of the form `mustache-FOO`, private
;; functions/variables are of the form `mst--FOO`.

(require 'ht)
(require 's)
(require 'dash)

(eval-when-compile '(require 'cl)) ;; destructuring-bind, loop, return

;; todo: add flag to set tolerance of missing variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let* ((lexemes (mst--lex template))
         (parsed-lexemes (mst--parse lexemes)))
    (mst--render-section-list parsed-lexemes context)))

(defvar mustache-partial-paths nil
  "A list of paths to be searched for mustache partial templates (files ending .mustache).")

;; todo: set flag to set tolerance of missing templates
(defun mst--get-partial (name)
  "Get the first partial whose file name is NAME.mustache, or \"\" otherwise.
Partials are searched for in `mustache-partial-paths'."
  (let ((partial-name (format "%s.mustache" name)))
    (dolist (path mustache-partial-paths)
      (let* ((partials (directory-files path nil "\\.mustache$"))
             (matching-partial (--first
                                (string-match-p (regexp-quote partial-name) it)
                                partials))))
      (when matching-partial
        (return (concat (file-name-as-directory path) matching-partial))))))

(defun mst--render-section-list (sections context)
  "Render a parsed list SECTIONS in CONTEXT."
  (mst--amapconcat (mst--render-section it context) sections))

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
              
              ;; save this tag
              (push (list :tag between-delimeters) lexemes)
              
              ;; iterate on the remaining template
              (setq template
                    (substring template continue-from-index)))
          ;; else only plain text left
          (progn
            (push (list :text template) lexemes)
            (setq template "")))))
    (nreverse lexemes)))

(defun mst--open-section-p (lexeme)
  "Is LEXEME a #tag or ^tag ?"
  (destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (or
          (s-starts-with-p "#" value)
          (s-starts-with-p "^" value)))))

(defun mst--close-section-p (lexeme)
  "Is LEXEME a /tag ?"
  (destructuring-bind (type value) lexeme
    (and (equal type :tag)
         (s-starts-with-p "/" value))))

(defun mst--section-name (lexeme)
  "Get the name of the section from LEXEME, a two part list returned by `mst--lex'.
The leading character (the #, ^ or /) is stripped."
  (s-chop-prefixes '("#" "^" "/") (cadr lexeme)))

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
    (loop while mst--remaining-lexemes do
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
            (return))
           (t
            ;; this is just a tag in the current section
            (push lexeme parsed-lexemes))))

    ;; ensure we aren't inside an unclosed section
    (when (and section-name (not (mst--close-section-p lexeme)))
      (error "Unclosed section: You haven't closed %s" section-name))

    (nreverse parsed-lexemes)))

(defun mst--render-tag (parsed-tag context)
  "Given PARSED-TAG, render it in hash table CONTEXT."
  (destructuring-bind (type value) parsed-tag
    (cond ((s-starts-with-p "!" value) ;; comment
           "")
          ((s-starts-with-p "&" value) ;; unescaped variable
           (or (ht-get context (s-trim (substring value 1))) ""))
          (t ;; normal variable
           (mst--escape-html (or (ht-get context value) ""))))))

(defun mst--tag-p (lexeme)
  "Is LEXEME a tag?"
  (equal (car lexeme) :tag))

(defun mst--section-p (lexeme)
  "Is LEXEME a nested section?"
  (not (atom (car lexeme))))

(defun mst--mapconcat (function sequence)
  "Apply FUNCTION to every element in SEQUENCE, and concat the results as strings."
  (mapconcat function sequence ""))

(defmacro mst--amapconcat (form sequence)
  "Anaphoric version of `mst--mapconcat'."
  `(mst--mapconcat (lambda (it) ,form) ,sequence))

(defun mst--render-section (parsed-lexeme context)
  "Given PARSED-LEXEME -- a lexed tag, plain text, or a nested list,
render it in CONTEXT."
  (cond ((mst--section-p parsed-lexeme)
         ;; nested section
         (let* (;; section-spec of the form "#foo"
                (section-spec (second (first parsed-lexeme)))
                (section-name (substring section-spec 1))
                (context-value (ht-get context section-name))
                ;; strip section open and close
                (section-contents (-slice parsed-lexeme 1 -1)))
           (cond
            ;; render #tags
            ((s-starts-with-p "#" section-spec)
             (if (or (consp context-value) (vectorp context-value))
                 ;; if the context is a list of hash tables, render repeatedly
                 (mst--amapconcat (mst--render-section-list section-contents it) context-value)
               ;; otherwise, if it's a truthy value, render in the current context
               (if context-value
                   (mst--render-section-list section-contents context)
                 "")))
            ;; render ^tags
            ((s-starts-with-p "^" section-spec)
             (if context-value
                 ""
               (mst--render-section-list section-contents context))))))
        ((mst--tag-p parsed-lexeme)
         (mst--render-tag parsed-lexeme context))
        ;; plain text
        (t
         (second parsed-lexeme))))

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
