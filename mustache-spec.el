(require 'json)
(require 'ht)

(defun read-json-path (path)
  "Open the JSON file at PATH, parse it, and return a hash table of its contents."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (json-read))))

(defun tests-from-path (path)
  "Read tests from a mustache specification file at PATH."
  (ht-get (read-json-path path) "tests"))

(defun mustache-test-spec ()
  (interactive)
  (ert-run-tests-interactively "mustache-spec-"))
