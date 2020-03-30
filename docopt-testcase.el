;;; docopt-testcase.el --- The Docopt testcase parser -*- lexical-binding: t -*-

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

;; The Docopt testcase parser

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'json)
(require 's)

(defclass docopt-testcase-example ()
  ((argv
    :accessor docopt-testcase-example-argv
    :documentation "The argument vector of the testcase example."
    :initarg :argv
    :initform nil
    :type (or string null))
   (actual
    :accessor docopt-testcase-example-actual
    :documentation "The actual result of the testcase example."
    :initarg :actual
    :initform nil
    :type (or list string null))
   (expected
    :accessor docopt-testcase-example-expected
    :documentation "The expected result of the testcase example."
    :initarg :expected
    :initform nil
    :type (or list string null)))
  "A class representing a Docopt testcase example.")

(defclass docopt-testcase ()
  ((program
    :accessor docopt-testcase-program
    :documentation "The program of the testcase."
    :initarg :program
    :initform nil
    :type (or docopt-program null))
   (examples
    :accessor docopt-testcase-examples
    :documentation "The examples of the testcase."
    :initarg :examples
    :initform nil
    :type (or list null)))
  "A class representing a Docopt testcase.")

(defun docopt-testcase--strip-comments (s)
  "Strip the comments from S."
  (s-replace-regexp "#.*$" "" s))

(defun docopt-testcase--parse-expected (s)
  "Parse the Docopt expected testcase result from S."
  (let ((json-false nil))
    (let ((expected (json-read-from-string s)))
      (if (listp expected)
          (cl-sort expected #'string< :key #'car)
        expected))))

(defun docopt--testcase-parse-example (s)
  "Parse Docopt testcase example from the string S."
  (seq-let [argv expected] (s-split-up-to "\n" s 1)
    (when (and argv expected)
      (docopt-testcase-example
       :argv (s-trim argv)
       :expected (docopt-testcase--parse-expected expected)))))

(defun docopt--testcase-parse-examples (s)
  "Parse Docopt testcase examples from the string S."
  (seq-map #'docopt--testcase-parse-example
           (seq-remove #'s-blank-p (s-split "\\$" (s-trim s)))))

(defun docopt-testcase-parse (s)
  "Parse Docopt testcases from the string S."
  (let ((raw (docopt-testcase--strip-comments s)))
    (when (s-starts-with-p "\"\"\"" raw)
      (setq raw (substring s 3)))
    (seq-map (lambda (fixture)
               (seq-let [source examples] (s-split "\"\"\"" fixture)
                 (docopt-testcase
                  :examples (docopt--testcase-parse-examples examples)
                  :program (docopt-parse-program source))))
             (seq-remove #'s-blank-p (s-split "r\"\"\"" raw)))))

(defun docopt--testcase-test-example (program example)
  "Test the Docopt EXAMPLE of the PROGRAM."
  (let ((argv (docopt-testcase-example-argv example)))
    (condition-case exception
        (oset example :actual (docopt-eval program argv))
      (t (oset example :actual "user-error")))
    example))

(defun docopt-testcase-example-failed-p (example)
  "Return t if the Docopt testcase EXAMPLE failed."
  (not (equal (docopt-testcase-example-actual example)
              (docopt-testcase-example-expected example))))

(defun docopt-testcase-failed-examples (testcase)
  "Return the failed examples of the Docopt TESTCASE."
  (seq-filter #'docopt-testcase-example-failed-p (docopt-testcase-examples testcase)))

(defun docopt-testcase-test (testcase)
  "Test the Docopt examples of TESTCASE."
  (let ((program (docopt-testcase-program testcase)))
    (message "Testing program:\n%s" (docopt-program-source program))
    (seq-doseq (example (docopt-testcase-examples testcase))
      (docopt--testcase-test-example program example))
    (docopt-testcase-failed-examples testcase)))

(defun docopt-testcase--symbol (testcase example)
  "Return the test symbol for the EXAMPLE of the Docopt TESTCASE."
  (with-slots (program) testcase
    (thread-last (docopt-testcase-example-argv example)
      (concat (docopt-program-source program))
      (secure-hash 'md5)
      (concat "docopt-testcase-")
      (intern))))

(defun docopt-testcase--log-example (testcase example)
  "Log the Docopt TESTCASE for EXAMPLE."
  (message "Testing Docopt program:\n\n%s\n" (docopt-program-source (docopt-testcase-program testcase)))
  (message "Argument vector: %s" (docopt-testcase-example-argv example))
  (message "Expected:\n%s" (pp-to-string (docopt-testcase-example-expected example)))
  (message "Actual:\n%s\n\n" (pp-to-string (docopt-testcase-example-actual example))))

(defun docopt-testcase-define-example (testcase example)
  "Define a test for the EXAMPLE of the Docopt TESTCASE."
  (eval `(ert-deftest ,(docopt-testcase--symbol testcase example) ()
           (let ((testcase ,testcase) (example ,example))
             (with-slots (actual expexted) example
               (docopt--testcase-test-example program example)
               (when (docopt-testcase-example-failed-p example)
                 (docopt-testcase--log-example testcase example))
               ;; TODO: Why is this so slow on many failures?
               (should (equal (docopt-testcase-example-expected example)
                              (docopt-testcase-example-actual example))))))))


(defun docopt-testcase-define-testcase (testcase)
  "Define a test for each Docopt TESTCASE example."
  (seq-doseq (example (docopt-testcase-examples testcase))
    (docopt-testcase-define-example testcase example)))

(defun docopt-testcase-define-testcases (testcases)
  "Define tests for the Docopt TESTCASES."
  (seq-doseq (testcase testcases)
    (docopt-testcase-define-testcase testcase)))

(provide 'docopt-testcase)

;;; docopt-testcase.el ends here
