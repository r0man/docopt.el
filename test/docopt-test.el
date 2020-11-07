;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Keywords: docopt, tools, processes
;; Package-Requires: ((buttercup "1.21"))
;; Homepage: https://github.com/r0man/docopt.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'cl-print)
(require 'docopt)
(require 'docopt-testcase)
(require 'f)
;; (require 'test-helper)

(setf cl-print-readably t)

(defvar docopt-test-skip-list
  '((16 . (1))
    (28 . (2))
    (32 . (2))
    (41 . (0))
    (42 . (0 2 3))
    (43 . (0 2))
    (46 . (0))
    (47 . (0))
    (48 . (0 1))
    (67 . (0)))
  "The examples to skip by testcase.")

(defun docopt-test--skip-p (testcase example skip-list)
  "Return t if the EXAMPLE of TESTCASE is in SKIP-LIST, otherwise nil."
  (when-let ((examples (assoc (oref testcase index) skip-list)))
    (member (oref example index) examples)))

(defun docopt-test--example-message (example)
  "Return the test message for the EXAMPLE."
  (format "should parse example #%s: %s"
          (docopt-testcase-example-index example)
          (docopt-testcase-example-argv example)))

(defun docopt-test--testcase-message (testcase)
  "Return the test message for the TESTCASE."
  (with-slots (program) testcase
    (format "Parsing the Docopt program #%s:\n\n%s\n"
            (docopt-testcase-index testcase)
            (docopt-program-source program))))

(defun docopt-test-define-example (testcase example &optional skip-list)
  "Define a Buttercup test for the Docopt TESTCASE and EXAMPLE using SKIP-LIST."
  (if (docopt-test--skip-p testcase example skip-list)
      (xit (docopt-test--example-message example)
        (expect (docopt-testcase-example-actual example)
                :to-equal (docopt-testcase-example-expected example)))
    (it (docopt-test--example-message example)
      (expect (docopt-testcase-example-actual example)
              :to-equal (docopt-testcase-example-expected example)))))

(defun docopt-test-define-testcase (testcase &optional skip-list)
  "Define a Buttercup test suite for the Docopt TESTCASE using SKIP-LIST."
  (describe (docopt-test--testcase-message testcase)
    (seq-doseq (example (docopt-testcase-test testcase))
      (docopt-test-define-example testcase example skip-list))))

(seq-doseq (testcase (docopt-testcase-parse (f-read-text "test/testcases.docopt")))
  (docopt-test-define-testcase testcase docopt-test-skip-list))

;; (seq-doseq (testcase (docopt-testcase-parse (f-read-text "test/testcases.docopt")))
;;   (docopt-test-define-testcase testcase))

;;; docopt-test.el ends here
