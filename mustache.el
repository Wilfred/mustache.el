(require 'ht)
(require 's)
(require 'dash)

(eval-when-compile '(require 'cl))

;; todo: add flag to set tolerance of missings variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let ((rendered ""))
    (dolist (lexeme (mustache-lex template) rendered)
      (destructuring-bind (type value) lexeme
        (if (eq type :text)
            (setq rendered (s-prepend rendered value))
          (setq rendered (s-prepend rendered (mustache-render-block value context))))))))

(defun mustache-lex (template)
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

(defun mustache-render-block (between-delimeters context)
  "Given BETWEEN-DELIMETERS text, render it in hash table CONTEXT."
  (ht-get context between-delimeters))

(provide 'mustache)
;;; mustache.el ends here
