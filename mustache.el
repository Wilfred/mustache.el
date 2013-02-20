(require 'ht)
(require 's)

;; todo: add flag to set tolerance of missings variables
(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (let ((open-delimeter "{{")
        (close-delimeter "}}")
        (rendered ""))
    (while (not (s-equals? template ""))
      (let* ((open-index (s-index-of open-delimeter template))
             (close-index (s-index-of close-delimeter template)))
        (if (and open-index close-index)
            ;; we have something to render
            (let ((between-delimeters
                   (substring template (+ open-index (length open-delimeter)) close-index))
                  (continue-from-index (+ close-index (length close-delimeter))))
              ;; append the string before the delimeter
              (setq rendered
                    (s-prepend rendered (substring template 0 open-index)))
              ;; render whatever we have between the delimeters
              (setq rendered
                    (s-prepend rendered (mustache-render-block between-delimeters context)))
              ;; iterate on the remaining template
              (setq template
                    (substring template continue-from-index)))
          ;; else only plain text left
          (progn
            (setq rendered (s-prepend rendered template))
            (setq template "")))))
    rendered))

(defun mustache-render-block (between-delimeters context)
  "Given BETWEEN-DELIMETERS text, render it in hash table CONTEXT."
  (ht-get context between-delimeters))

(provide 'mustache)
;;; mustache.el ends here
