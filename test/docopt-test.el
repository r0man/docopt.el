;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, tools, processes
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

(defun docopt-test-define-it (example)
  "Define a Buttercup test for the Docopt TESTCASE and EXAMPLE."
  (it (format "should parse: %s" (docopt-testcase-example-argv example))
    (expect (docopt-testcase-example-actual example)
            :to-equal (docopt-testcase-example-expected example))))

(defun docopt-test-define-describe (testcase)
  "Define a Buttercup test suite for the Docopt TESTCASE."
  (let ((program (docopt-testcase-program testcase)))
    (describe (format "Parsing the Docopt program:\n\n%s"
                      (docopt-program-source program))
      (seq-doseq (example (docopt-testcase-test testcase))
        (docopt-test-define-it example)))))

;;; docopt-test.el ends here

;; (setq my-program (docopt-parse "Usage: prog -v ..."))

(setq my-program (docopt-parse "usage: prog [-o <o>]...

options: -o <o>  [default: x y]"))

;; (docopt-eval my-program "prog -o this")
;; (docopt-eval my-program "prog 20 40")

;; (docopt-eval-ast my-program "prog -vv")

;; (docopt-eval-ast my-program "prog -a -r -myourass")

;; (setq my-program
;;       (docopt-parse "usage: prog (NAME | --foo NAME)

;; options: --foo
;; "))

;; (docopt-eval-ast my-program "prog -a")

;; (pp (car (last (docopt-parse-testcases (f-read-text "testcases.docopt")))))

(seq-doseq (testcase (docopt-parse-testcases (f-read-text "test/testcases.docopt")))
  (docopt-test-define-describe testcase))

;; (seq-doseq (testcase (seq-take (docopt-parse-testcases (f-read-text "test/testcases.docopt")) 17))
;;   (docopt-test-define-describe testcase))

;; (setq my-testcase (nth 5 (docopt-parse-testcases (f-read-text "test/testcases.docopt"))))
;; (docopt-test-define-describe my-testcase)
