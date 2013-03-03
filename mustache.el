;;; mustache.el -- a mustache templating library in emacs lisp

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.2
;; Keywords: mustache, template

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

(require 'ht)
(require 's)
(require 'dash)

(eval-when-compile '(require 'cl))

;; todo: add flag to set tolerance of missings variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let* ((lexemes (mustache/lex template))
         (parsed-lexemes (mustache/parse lexemes))
         (rendered ""))
    (mustache/render-section-list parsed-lexemes context)))

(defun mustache/render-section-list (sections context)
  "Render a parsed list SECTIONS in CONTEXT."
  (let ((rendered ""))
    (dolist (section sections rendered)
      (mustache/append! rendered (mustache/render-section section context)))))

(defun mustache/lex (template)
  "Iterate through TEMPLATE, splitting {{ blocks }} and bare strings.
We return a list of lists: ((:text \"foo\") (:block \"variable-name\"))"
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

(defun mustache/open-section-p (lexeme)
  "Is LEXEME a #block or ^block ?"
  (destructuring-bind (type value) lexeme
    (and (equal type :block)
         (or
          (s-starts-with-p "#" value)
          (s-starts-with-p "^" value)))))

(defun mustache/close-section-p (lexeme)
  "Is LEXEME a /block ?"
    (destructuring-bind (type value) lexeme
    (and (equal type :block)
         (s-starts-with-p "/" value))))

(defun mustache/parse (lexemes)
  "Given a list LEXEMES, return a list of lexemes nested according to #blocks or ^blocks."
  (destructuring-bind (last-index parsed-lexemes)
      (mustache/parse-from lexemes 0)
    parsed-lexemes))

;; todo: error on unclosed blocks
;; todo: check for mismatches section open/close
(defun mustache/parse-from (lexemes start-index)
  "Given a list LEXEMES and start position START-INDEX,
return a nested list (last-index, parsed-lexemes)"
  (let ((parsed-lexemes nil)
        (index start-index)
        (keep-parsing 't))
    (while (and (< index (length lexemes)) keep-parsing)
      (let ((lexeme (nth index lexemes)))
        (cond
         ((mustache/open-section-p lexeme)
          ;; recurse on this nested section
          (destructuring-bind (last-index nested-lexemes) (mustache/parse-from lexemes (1+ index))
            (setq index last-index)
            (setq parsed-lexemes (cons (cons lexeme nested-lexemes) parsed-lexemes))))
         ((mustache/close-section-p lexeme)
          ;; this is the last block in this section
          (setq parsed-lexemes (cons lexeme parsed-lexemes))
          (setq index (1+ index))
          (setq keep-parsing nil))
         (t
          ;; this is just a block in the current section          
          (setq parsed-lexemes (cons lexeme parsed-lexemes))
          (setq index (1+ index))))))
    
    (list index (nreverse parsed-lexemes))))

(defun mustache/render-block (parsed-block context)
  "Given PARSED-BLOCK, render it in hash table CONTEXT."
  (destructuring-bind (type value) parsed-block
    (if (s-starts-with-p "!" value)
        ""
      (mustache/escape-html (or (ht-get context value) "")))))

(defun mustache/block-p (lexeme)
  "Is LEXEME a block?"
  (equal (car lexeme) :block))

(defun mustache/section-p (lexeme)
  "Is LEXEME a nested section?"
  (not (atom (car lexeme))))

(defmacro mustache/append! (var string)
  "Destructive: sets VAR to the concatenation of VAR and STRING."
  `(setq ,var (concat ,var ,string)))

(defun mustache/render-section (parsed-lexeme context)
  "Given PARSED-LEXEME -- a lexed block, plain text, or a nested list,
render it in CONTEXT."
  (cond ((mustache/section-p parsed-lexeme)
         ;; nested section
         (let* ((rendered "")
                ;; section-spec of the form "#foo"
                (section-spec (second (first parsed-lexeme)))
                (section-name (substring section-spec 1))
                (context-value (ht-get context section-name))
                ;; strip section open and close
                (section-contents (-slice parsed-lexeme 1 -1)))
           (cond
            ;; render #blocks
            ((s-starts-with-p "#" section-spec)
                  (if (consp context-value)
                      ;; if the context is a list of hash tables, render repeatedly
                      (dolist (new-context context-value rendered)
                        (mustache/append! rendered (mustache/render-section-list section-contents new-context)))
                    ;; otherwise, if it's a truthy value, render in the current context
                    (if context-value
                        (mustache/append! rendered (mustache/render-section-list section-contents context)))))
            ;; render ^blocks
            ((s-starts-with-p "^" section-spec)
             (unless context-value
                (mustache/append! rendered (mustache/render-section-list section-contents context)))))))
        ((mustache/block-p parsed-lexeme)
         (mustache/render-block parsed-lexeme context))
        ;; plain text
        (t
         (second parsed-lexeme))))

(defun mustache/escape-html (string)
  "Escape HTML in STRING."
  (->> string
    (s-replace "&" "&amp;")
    (s-replace "<" "&lt;")
    (s-replace ">" "&gt;")
    (s-replace "'" "&#39;")
    (s-replace "\"" "&quot;")))

(provide 'mustache)
;;; mustache.el ends here
