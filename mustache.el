;;; mustache.el -- a mustache templating library in emacs lisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.6
;; Keywords: mustache, template
;; Package-Requires: ((ht "0.8") (s "1.3.0") (dash "1.1.0") (with-namespace "1.1"))

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
;; strings (plain text), and blocks (anything wrapped in delimeters:
;; {{foo}}). A section is a special block that requires closing
;; (e.g. {{#foo}}{{/foo}}).

;; We treat mustache templates as if they conform to a rough grammar:

;; TEMPLATE = plaintext | BLOCK | SECTION | TEMPLATE
;; SECTION = OPEN-BLOCK TEMPLATE CLOSE-BLOCK
;; BLOCK = "{{" text "}}"

(require 'ht)
(require 's)
(require 'dash)
(require 'with-namespace)

(eval-when-compile '(require 'cl)) ;; destructuring-bind, loop, return

(with-namespace "mustache"
  ;; todo: add flag to set tolerance of missing variables
  (defun render (template context)
    "Render a mustache TEMPLATE with hash table CONTEXT."
    (let* ((lexemes (-lex template))
           (parsed-lexemes (-parse lexemes)))
      (-render-section-list parsed-lexemes context)))

  (defvar partial-paths nil
    "A list of paths to be searched for mustache partial templates (files ending .mustache).")

  ;; todo: set flag to set tolerance of missing templates
  (defun -get-partial (name)
    "Get the first partial whose file name is NAME.mustache, or \"\" otherwise.
Partials are searched for in `mustache-partial-paths'."
    (let ((partial-name (format "%s.mustache" name)))
      (dolist (path partial-paths)
        (let* ((partials (directory-files path nil "\\.mustache$"))
               (matching-partial (--first
                                  (string-match-p (regexp-quote partial-name) it)
                                  partials))))
        (when matching-partial
          (return (concat (file-name-as-directory path) matching-partial))))))

  (defun -render-section-list (sections context)
    "Render a parsed list SECTIONS in CONTEXT."
    (-amapconcat (-render-section it context) sections))

  (defun -lex (template)
    "Iterate through TEMPLATE, splitting {{ blocks }} and bare strings.
We return a list of lists: ((:text \"foo\") (:block \"variable-name\"))"
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
              ;; we have a block
              (let ((between-delimeters
                     (substring template (+ open-index (length open-delimeter)) close-index))
                    (continue-from-index (+ close-index (length close-delimeter))))
                ;; save the string before the block
                (when (> open-index 0)
                  (!cons (list :text (substring template 0 open-index)) lexemes))
                
                ;; save this block
                (!cons (list :block between-delimeters) lexemes)
                
                ;; iterate on the remaining template
                (setq template
                      (substring template continue-from-index)))
            ;; else only plain text left
            (progn
              (!cons (list :text template) lexemes)
              (setq template "")))))
      (nreverse lexemes)))

  (defun -open-section-p (lexeme)
    "Is LEXEME a #block or ^block ?"
    (destructuring-bind (type value) lexeme
      (and (equal type :block)
           (or
            (s-starts-with-p "#" value)
            (s-starts-with-p "^" value)))))

  (defun -close-section-p (lexeme)
    "Is LEXEME a /block ?"
    (destructuring-bind (type value) lexeme
      (and (equal type :block)
           (s-starts-with-p "/" value))))

  (defun -section-name (lexeme)
    "Get the name of the section from LEXEME, a two part list returned by `mustache--lex'."
    (cadr lexeme))

  (defvar -remaining-lexemes nil
    "Since `mustache--parse' recursively calls itself, we need a shared value to mutate.")

  ;; todo: error on unclosed blocks
  ;; todo: check for mismatched section open/close
  (defun -parse (lexemes)
    "Given a list LEXEMES, return a list of lexemes nested according to #blocks or ^blocks."
    (setq -remaining-lexemes lexemes)
    (-parse-inner))

  (defun -parse-inner (&optional section-name)
    "Parse `mustache--remaining-lexemes', and return a list of lexemes nested according to #blocks or ^blocks."
    (let ((parsed-lexemes nil))
      (loop while -remaining-lexemes do
        (let ((lexeme (pop -remaining-lexemes)))
          (cond
           ((-open-section-p lexeme)
            ;; recurse on this nested section
            (!cons (cons lexeme (-parse-inner (-section-name lexeme))) parsed-lexemes))
           ((-close-section-p lexeme)
            ;; this is the last block in this section
            (unless section-name
              (error "Mismatched brackets: You closed a section with %s, but it wasn't open" section-name))
            (!cons lexeme parsed-lexemes)
            (return))
           (t
            ;; this is just a block in the current section          
            (!cons lexeme parsed-lexemes)))))
      
      (nreverse parsed-lexemes)))

  (defun -render-block (parsed-block context)
    "Given PARSED-BLOCK, render it in hash table CONTEXT."
    (destructuring-bind (type value) parsed-block
      (cond ((s-starts-with-p "!" value) ;; comment
             "")
            ((s-starts-with-p "&" value) ;; unescaped variable
             (or (ht-get context (s-trim (substring value 1))) ""))
            (t ;; normal variable
             (-escape-html (or (ht-get context value) ""))))))

  (defun -block-p (lexeme)
    "Is LEXEME a block?"
    (equal (car lexeme) :block))

  (defun -section-p (lexeme)
    "Is LEXEME a nested section?"
    (not (atom (car lexeme))))

  (defun -mapconcat (function sequence)
    "Apply FUNCTION to every element in SEQUENCE, and concat the results as strings."
    (mapconcat function sequence ""))

  (defmacro -amapconcat (form sequence)
    "Anaphoric version of `mustache--mapconcat'."
    `(-mapconcat (lambda (it) ,form) ,sequence))

  (defun -render-section (parsed-lexeme context)
    "Given PARSED-LEXEME -- a lexed block, plain text, or a nested list,
render it in CONTEXT."
    (cond ((-section-p parsed-lexeme)
           ;; nested section
           (let* (;; section-spec of the form "#foo"
                  (section-spec (second (first parsed-lexeme)))
                  (section-name (substring section-spec 1))
                  (context-value (ht-get context section-name))
                  ;; strip section open and close
                  (section-contents (-slice parsed-lexeme 1 -1)))
             (cond
              ;; render #blocks
              ((s-starts-with-p "#" section-spec)
               (if (or (consp context-value) (vectorp context-value))
                   ;; if the context is a list of hash tables, render repeatedly
                   (-amapconcat (-render-section-list section-contents it) context-value)
                 ;; otherwise, if it's a truthy value, render in the current context
                 (if context-value
                     (-render-section-list section-contents context)
                   "")))
              ;; render ^blocks
              ((s-starts-with-p "^" section-spec)
               (if context-value
                   ""
                 (-render-section-list section-contents context))))))
          ((-block-p parsed-lexeme)
           (-render-block parsed-lexeme context))
          ;; plain text
          (t
           (second parsed-lexeme))))

  (defun -escape-html (string)
    "Escape HTML in STRING."
    (->> string
      (s-replace "&" "&amp;")
      (s-replace "<" "&lt;")
      (s-replace ">" "&gt;")
      (s-replace "'" "&#39;")
      (s-replace "\"" "&quot;"))))

(provide 'mustache)
;;; mustache.el ends here
