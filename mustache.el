(require 'ht)
(require 's)

(defun mustache-render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  template)

(mustache-render "foo" (ht-create))

(provide 'mustache)
;;; mustache.el ends here
