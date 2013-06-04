(require 'json)
(require 'ht)
(require 'dash)
(require 'mustache)
(eval-when-compile (require 'cl))

(defun read-json-path (path)
  "Open the JSON file at PATH, parse it, and return a hash table of its contents."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-false nil))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (json-read))))

(defun tests-from-path (path)
  "Read tests from a mustache specification file at PATH."
  (--map (list (ht-get it "data")
               (ht-get it "template")
               (ht-get it "expected")
               )
         (ht-get (read-json-path path) "tests")))

;; todo: proper ERT unit tests
(defun* run-tests-from-path (path &aux (failures 0))
  "Loop over the assertions, message when they fail"
  (loop for (context template expected)
        in (tests-from-path path)
        do (let ((actual (mustache-render template context)))
             (unless (equal actual expected)
               (message "Expected:\n%s\nbut got:\n%s" expected actual)
               (incf failures))))
  (message "%d failure(s)" failures))

(run-tests-from-path "spec/inverted.json")
(run-tests-from-path "spec/interpolation.json")

(defun mustache-test-spec ()
  (interactive)
  (ert-run-tests-interactively "mustache-spec-"))
