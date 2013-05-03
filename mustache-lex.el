(require 's)

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
              (push (list :tag (s-trim between-delimeters)) lexemes)
              
              ;; iterate on the remaining template
              (setq template
                    (substring template continue-from-index)))
          ;; else only plain text left
          (progn
            (push (list :text template) lexemes)
            (setq template "")))))
    (nreverse lexemes)))

