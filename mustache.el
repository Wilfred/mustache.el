(require 'ht)
(require 's)

(eval-when-compile '(require 'cl))

;; todo: add flag to set tolerance of missings variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let ((open-delimeter "{{")
        (close-delimeter "}}")
        (rendered ""))
    (while (not (s-equals? template ""))
      (let ((open-index (s-index-of open-delimeter template)))
        (if open-index
            ;; something to render
            (progn
              ;; todo: get variable from context
              (setq rendered "template!")
              ;; todo: proper iterative through tempalte
              (setq template ""))
          ;; else only plain text left
          (progn
            (setq rendered (s-append rendered template))
            (setq template "")))))
    rendered))

(mustache-render "foo {{" (ht-create))

(provide 'mustache)
;;; mustache.el ends here
