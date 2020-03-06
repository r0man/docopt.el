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

(require 'buttercup)
(require 'cl-lib)
(require 'docopt-parser)
(require 'docopt-util)
(require 'eieio)
(require 'json)
(require 'parsec)
(require 's)

(defclass docopt-testcase-example ()
  ((ast
    :accessor docopt-testcase-example-ast
    :documentation "The argument vector of the testcase example."
    :initarg :ast
    :initform nil)
   (argv
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
    :type (or list symbol null))
   (expected
    :accessor docopt-testcase-example-expected
    :documentation "The expected result of the testcase example."
    :initarg :expected
    :initform nil
    :type (or list symbol null)))
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

(defun docopt--parse-testcase-comment ()
  "Parse a Docopt testcase comment."
  (parsec-and (docopt--parse-spaces)
              (parsec-ch ?\#)
              (parsec-until-s (parsec-eol-or-eof))))

(defun docopt--parse-testcase-blank-line ()
  "Parse a Docopt testcase comment."
  (parsec-or (docopt--parse-testcase-comment)
             (parsec-try (docopt--parse-blank-line))))

(defun docopt--parse-testcase-blank-lines ()
  "Parse Docopt testcase blank lines."
  (parsec-many (docopt--parse-testcase-blank-line)))

(defun docopt--parse-testcase-usage-quotes ()
  "Parse the Docopt testcase quotes."
  (parsec-count 3 (parsec-ch ?\")))

(defun docopt--parse-testcase-usage-start ()
  "Parse the start of a Docopt testcase."
  (parsec-collect (parsec-ch ?r) (docopt--parse-testcase-usage-quotes)))

(defun docopt--parse-testcase-usage-end ()
  "Parse the start of a Docopt testcase."
  (docopt--parse-testcase-usage-quotes))

(defun docopt--parse-testcase-usage ()
  "Parse the Docopt testcase usage."
  (s-trim (parsec-and (docopt--parse-testcase-usage-start)
                      (parsec-return (parsec-until-s (docopt--parse-testcase-usage-end))
                        (docopt--parse-whitespaces)))))

(defun docopt--parse-testcase-program ()
  "Parse the Docopt testcase program."
  (let ((usage (docopt--parse-testcase-usage)))
    (let ((program (parsec-with-input usage (docopt--parse-program))))
      (if (docopt-program-p program)
          program
        (error "Can't parse Docopt program: %s" usage)))))

(defun docopt--parse-testcase-argv ()
  "Parse the Docopt testcase argument vector."
  (parsec-and (parsec-ch ?$)
              (docopt--parse-spaces1)
              (parsec-until-s (parsec-eol))))

(defun docopt--parse-testcase-expected-error ()
  "Parse the Docopt testcase expected result error."
  (parsec-return (parsec-str "\"user-error\"")
    (parsec-optional (parsec-try (docopt--parse-testcase-comment)))
    (parsec-eol-or-eof))
  'user-error)

(defun docopt--parse-testcase-expected-data ()
  "Parse the Docopt testcase expected result data."
  (let ((json-false nil))
    (cl-sort (json-read-from-string
              (concat "{" (parsec-and (docopt--parse-spaces)
                                      (parsec-ch ?\{)
                                      (parsec-until-s
                                       (parsec-try (parsec-and
                                                    (docopt--parse-spaces)
                                                    (parsec-ch ?\})
                                                    (docopt--parse-spaces)
                                                    (parsec-eol-or-eof))))) "}"))
             #'string< :key #'car)))

(defun docopt--parse-testcase-expected ()
  "Parse the Docopt testcase expected result."
  (parsec-or (docopt--parse-testcase-expected-error)
             (docopt--parse-testcase-expected-data)))

(defun docopt--parse-testcase-example ()
  "Parse a Docopt testcase example."
  (parsec-return (parsec-collect
                  (docopt--parse-testcase-argv)
                  (docopt--parse-testcase-expected))
    (docopt--parse-whitespaces)))

(defun docopt--parse-testcase-example ()
  "Parse a Docopt testcase example."
  (seq-let [argv expected]
      (parsec-return (parsec-collect
                      (docopt--parse-testcase-argv)
                      (docopt--parse-testcase-expected))
        (docopt--parse-whitespaces))
    (make-instance 'docopt-testcase-example :argv argv :expected expected)))

(defun docopt--parse-testcase-examples ()
  "Parse Docopt testcase examples."
  (parsec-many1 (docopt--parse-testcase-example)))

(defun docopt--parse-testcase ()
  "Parse a Docopt testcase."
  (seq-let [program examples]
      (parsec-collect
       (parsec-and (parsec-try (docopt--parse-testcase-blank-lines))
                   (docopt--parse-testcase-program))
       (docopt--parse-testcase-examples))
    (docopt-testcase :program program :examples examples)))

(defun docopt--parse-testcases ()
  "Parse Docopt testcases."
  (parsec-many (docopt--parse-testcase)))

(defun docopt-parse-testcases (s)
  "Parse Docopt testcases from the string S."
  (parsec-with-input s (docopt--parse-testcases)))

(defun docopt--testcase-test-example (program example)
  "Test the Docopt EXAMPLE of the PROGRAM."
  (let ((argv (docopt-testcase-example-argv example)))
    (condition-case nil
        (let* (
               (ast (docopt--parse-argv program argv))
               (expected (docopt-testcase-example-expected example)))
          (oset example :ast ast)
          (if (docopt--parsec-error-p ast)
              (oset example :actual 'user-error)
            (oset example :actual (docopt--argv-to-alist program ast))))
      (error (progn (message "Test \"%s\": ERROR" argv)
                    (oset example :actual 'fatal-error))))
    example))

(defun docopt-testcase-test (testcase)
  "Test the Docopt examples of TESTCASE."
  (let ((program (docopt-testcase-program testcase)))
    (message "Testing program:\n%s" (docopt-string program))
    (seq-map (lambda (example)
               (docopt--testcase-test-example program example))
             (docopt-testcase-examples testcase))))

(provide 'docopt-testcase)

;;; docopt-testcase.el ends here
