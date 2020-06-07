;;; docopt-testcase.el --- Docopt testcase -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
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
(require 'docopt-argv)
(require 'docopt-parser)
(require 'docopt-util)
(require 'eieio)
(require 'json)
(require 'parsec)
(require 's)
(require 'seq)

(defclass docopt-testcase-example ()
  ((actual
    :accessor docopt-testcase-example-actual
    :documentation "The actual result of the testcase example."
    :initarg :actual
    :initform nil
    :type (or list symbol null))
   (argv
    :accessor docopt-testcase-example-argv
    :documentation "The argument vector of the testcase example."
    :initarg :argv
    :initform nil
    :type (or string null))
   (ast
    :accessor docopt-testcase-example-ast
    :documentation "The argument vector of the testcase example."
    :initarg :ast
    :initform nil)
   (expected
    :accessor docopt-testcase-example-expected
    :documentation "The expected result of the testcase example."
    :initarg :expected
    :initform nil
    :type (or list symbol null))
   (index
    :accessor docopt-testcase-example-index
    :documentation "The index of the testcase example."
    :initarg :index
    :initform nil
    :type (or null number)))
  "A class representing a Docopt testcase example.")

(defclass docopt-testcase ()
  ((examples
    :accessor docopt-testcase-examples
    :documentation "The examples of the testcase."
    :initarg :examples
    :initform nil
    :type (or list null))
   (index
    :accessor docopt-testcase-index
    :documentation "The index of the testcase."
    :initarg :index
    :initform nil
    :type (or null number))
   (program
    :accessor docopt-testcase-program
    :documentation "The program of the testcase."
    :initarg :program
    :initform nil
    :type (or docopt-program null)))
  "A class representing a Docopt testcase.")

(defun docopt-testcase--parse-comment ()
  "Parse a Docopt testcase comment."
  (parsec-and (docopt-parser-spaces)
              (parsec-ch ?\#)
              (parsec-until-s (parsec-eol-or-eof))))

(defun docopt-testcase--parse-blank-line ()
  "Parse a Docopt testcase comment."
  (parsec-or (docopt-testcase--parse-comment)
             (parsec-try (parsec-and (docopt-parser-spaces) (parsec-eol)))))

(defun docopt-testcase--parse-blank-lines ()
  "Parse Docopt testcase blank lines."
  (parsec-many (docopt-testcase--parse-blank-line)))

(defun docopt-testcase--parse-usage-quotes ()
  "Parse the Docopt testcase quotes."
  (parsec-count 3 (parsec-ch ?\")))

(defun docopt-testcase--parse-usage-start ()
  "Parse the start of a Docopt testcase."
  (parsec-collect (parsec-ch ?r) (docopt-testcase--parse-usage-quotes)))

(defun docopt-testcase--parse-usage-end ()
  "Parse the start of a Docopt testcase."
  (docopt-testcase--parse-usage-quotes))

(defun docopt-testcase--parse-usage ()
  "Parse the Docopt testcase usage."
  (docopt-strip (parsec-and (docopt-testcase--parse-usage-start)
                            (parsec-return (parsec-until-s (docopt-testcase--parse-usage-end))
                              (docopt-parser-whitespaces)))))

(defun docopt-testcase--parse-program ()
  "Parse the Docopt testcase program."
  (docopt-parse (docopt-testcase--parse-usage)))

(defun docopt-testcase--parse-argv ()
  "Parse the Docopt testcase argument vector."
  (parsec-and (parsec-ch ?$)
              (docopt-parser-spaces1)
              (parsec-until-s (parsec-eol))))

(defun docopt-testcase--parse-expected-error ()
  "Parse the Docopt testcase expected result error."
  (parsec-return (parsec-str "\"user-error\"")
    (parsec-optional (parsec-try (docopt-testcase--parse-comment)))
    (parsec-eol-or-eof))
  'user-error)

(defun docopt-testcase--parse-expected-data ()
  "Parse the Docopt testcase expected result data."
  (let ((json-false nil))
    (cl-sort (json-read-from-string
              (concat "{" (parsec-and (docopt-parser-spaces)
                                      (parsec-ch ?\{)
                                      (parsec-until-s
                                       (parsec-try (parsec-and
                                                    (docopt-parser-spaces)
                                                    (parsec-ch ?\})
                                                    (docopt-parser-spaces)
                                                    (parsec-eol-or-eof))))) "}"))
             #'string< :key #'car)))

(defun docopt-testcase--parse-expected ()
  "Parse the Docopt testcase expected result."
  (parsec-or (docopt-testcase--parse-expected-error)
             (docopt-testcase--parse-expected-data)))

(defun docopt-testcase--parse-example ()
  "Parse a Docopt testcase example."
  (seq-let [argv expected]
      (parsec-return (parsec-collect
                      (docopt-testcase--parse-argv)
                      (docopt-testcase--parse-expected))
        (docopt-parser-whitespaces))
    (make-instance 'docopt-testcase-example :argv argv :expected expected)))

(defun docopt-testcase--parse-examples ()
  "Parse Docopt testcase examples."
  (thread-last (parsec-many1 (docopt-testcase--parse-example))
    (seq-map-indexed (lambda (example index)
                       (setf (oref example index) index)
                       example))))

(defun docopt-testcase--parse-testcase ()
  "Parse a Docopt testcase."
  (seq-let [program examples]
      (parsec-collect
       (parsec-and (parsec-try (docopt-testcase--parse-blank-lines))
                   (docopt-testcase--parse-program))
       (docopt-testcase--parse-examples))
    (docopt-testcase :program program :examples examples)))

(defun docopt-testcase--parse-testcases ()
  "Parse Docopt testcases."
  (parsec-many (docopt-testcase--parse-testcase)))

(defun docopt-testcase-parse (s)
  "Parse Docopt testcases from the string S."
  (thread-last (parsec-with-input s (docopt-testcase--parse-testcases))
    (seq-map-indexed (lambda (testcase index)
                       (setf (oref testcase index) index)
                       testcase))))

(defun docopt-testcase--test-example (program example)
  "Test the Docopt EXAMPLE of the PROGRAM."
  (let ((argv (docopt-testcase-example-argv example)))
    (condition-case nil
        (let ((ast (docopt-argv-parse program argv)))
          (setf (oref example :ast) ast)
          (if (docopt--parsec-error-p ast)
              (setf (oref example :actual) 'user-error)
            (setf (oref example :actual) (docopt--argv-to-alist program ast))))
      (error (setf (oref example :actual) 'user-error)))
    example))

(defun docopt-testcase-test (testcase)
  "Test the Docopt examples of TESTCASE."
  (let ((program (docopt-testcase-program testcase)))
    (message "Testing program:\n%s\n" (docopt-program-source program))
    (seq-map (lambda (example)
               (docopt-testcase--test-example program example))
             (docopt-testcase-examples testcase))))

(provide 'docopt-testcase)

;;; docopt-testcase.el ends here
