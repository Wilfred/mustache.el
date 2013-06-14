(require 'json)
(require 'ht)
(require 'dash)
(require 'mustache)
(require 'ert)
(require 's)
(eval-when-compile (require 'cl))

(eval-and-compile
  (defun mst--read-json-path (path)
    "Open the JSON file at PATH, parse it, and return a hash table of its contents."
    (let ((json-object-type 'hash-table)
          (json-array-type 'list)
          (json-false nil))
      (with-temp-buffer
        (insert-file-contents-literally path)
        (json-read))))

  (defun mst--ert-from-spec (spec-case)
    "Return a quoted ert test from a hash-table SPEC-CASE
describing expected behaviour"
    (let* ((spec-name (ht-get spec-case "name"))
           (test-name (intern (format "mustache-spec-%s" (s-replace " " "-" spec-name))))
           (description (ht-get spec-case "desc"))
           (template (ht-get spec-case "template"))
           (context (ht-get spec-case "data"))
           (expected (ht-get spec-case "expected")))
      `(ert-deftest ,test-name ()
         ,description
         (should
          (equal
           ,expected (mustache-render ,template ,context)))))))

(defmacro ert-tests-from-path (path)
  "Open the JSON file at PATH, and return a list of ert test cases."
  `(progn
     ,@(--map (mst--ert-from-spec it)
              (ht-get (mst--read-json-path path) "tests"))))

(ert-tests-from-path "spec/inverted.json")
(ert-tests-from-path "spec/interpolation.json")
(ert-tests-from-path "spec/section.json")
(ert-tests-from-path "spec/comments.json")

(defun mustache-test-spec ()
  (interactive)
  (ert-run-tests-interactively "mustache-spec-"))
