(require 'ht)
(require 's)
(require 'dash)

;; checkme
(eval-when-compile '(require 'cl)) ;; destructuring-bind, loop, return, dolist

(load "mustache-lex.el")
(load "mustache-parse.el")

(defun mst--render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let* ((lexemes (mst--lex template))
         (parsed-lexemes (mst--parse lexemes)))
    (mst--render-section-list parsed-lexemes context)))

;; todo: set flag to set tolerance of missing templates
(defun mst--get-partial (name)
  "Get the first partial whose file name is NAME.mustache, or nil otherwise.
Partials are searched for in `mustache-partial-paths'."
  (unless (listp mustache-partial-paths)
    (error "`mustache-partial-paths' must be a list of paths."))
  (let ((partial-name (format "%s.mustache" name)))
    (dolist (path mustache-partial-paths)
      (-when-let*
          ((partials (directory-files path nil "\\.mustache$"))
           (matching-partial (--first
                              (string-match-p (regexp-quote partial-name) it)
                              partials)))
        (return
         (with-temp-buffer
           (insert-file-contents-literally matching-partial)
           (buffer-substring-no-properties (point-min) (point-max))))))))

(defun mst--render-section-list (sections context)
  "Render a parsed list SECTIONS in CONTEXT."
  (mst--amapconcat (mst--render-section it context) sections))

(defun mst--render-tag (parsed-tag context)
  "Given PARSED-TAG, render it in hash table CONTEXT."
  (destructuring-bind (type value) parsed-tag
    (cond ((s-starts-with-p "!" value) ;; comment
           "")
          ((s-starts-with-p "&" value) ;; unescaped variable
           (ht-get context (s-trim (substring value 1)) ""))
          ((s-starts-with-p ">" value)
           (let ((partial (mst--get-partial (s-trim (substring value 1)))))
             (if partial
                 (mst--render partial context)
               "")))
          (t ;; normal variable
           (mst--escape-html (ht-get context value ""))))))

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

(defun mst--context-add (table from-table)
  "Return a copy of TABLE where all the key-value pairs in FROM-TABLE have been set."
  (let ((new-table (ht-copy table)))
    (ht-update new-table from-table)
    new-table))

(defun mst--render-section (parsed-lexeme context)
  "Given PARSED-LEXEME -- a lexed tag, plain text, or a nested list,
render it in CONTEXT."
  (cond ((mst--section-p parsed-lexeme)
         ;; nested section
         (let* (;; section-spec of the form "#foo"
                (section-spec (second (first parsed-lexeme)))
                (section-name (s-trim (substring section-spec 1)))
                (context-value (ht-get context section-name))
                ;; strip section open and close
                (section-contents (-slice parsed-lexeme 1 -1)))
           (cond
            ;; render #tags
            ((s-starts-with-p "#" section-spec)
             (cond
              ;; if the context is a list of hash tables, render repeatedly
              ((or (consp context-value) (vectorp context-value))
               (mst--amapconcat (mst--render-section-list section-contents (mst--context-add context it)) context-value))
              ;; if the context is a hash table, render in that context
              ((hash-table-p context-value)
               (mst--render-section-list section-contents (mst--context-add context context-value)))
              ;; otherwise, if it's a truthy value, render in the current context
              (t (if context-value
                     (mst--render-section-list section-contents context)
                   ""))))
            ;; render ^tags
            ((s-starts-with-p "^" section-spec)
             (if context-value
                 ""
               (mst--render-section-list section-contents context))))))
        ((mst--tag-p parsed-lexeme)
         (mst--render-tag parsed-lexeme context))
        ;; plain text
        (t
         (s-chop-prefix
          "\n"
          (second parsed-lexeme)))))

(defun mst--escape-html (string)
  "Escape HTML in STRING."
  (->> string
    (s-replace "&" "&amp;")
    (s-replace "<" "&lt;")
    (s-replace ">" "&gt;")
    (s-replace "'" "&#39;")
    (s-replace "\"" "&quot;")))
