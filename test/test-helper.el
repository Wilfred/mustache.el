;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((mustache-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path mustache-dir))

(require 'undercover)
(undercover "*.el" (:exclude "*-tests.el"))

;;; test-helper.el ends here
