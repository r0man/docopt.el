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

;; (defun docopt--parse-testcase-expected-data ()
;;   "Parse the Docopt testcase expected result data."
;;   (let ((json-false nil))
;;     (cl-sort (json-read-from-string
;;               (concat "{" (parsec-and (docopt--parse-spaces)
;;                                       (parsec-ch ?\{)
;;                                       (parsec-until-s
;;                                        (parsec-try (parsec-and
;;                                                     (docopt--parse-spaces)
;;                                                     (parsec-ch ?\})
;;                                                     (docopt--parse-spaces)
;;                                                     (parsec-eol-or-eof))))) "}"))
;;              #'string< :key #'car)))

;; (defun docopt--parse-testcase-example ()
;;   "Parse a Docopt testcase example."
;;   )

;; (defun docopt--parse-testcase-examples ()
;;   "Parse Docopt testcase examples."
;;   )

;; (defun docopt--parse-testcase ()
;;   "Parse a Docopt testcase."
;;   (seq-let [program examples]
;;       (parsec-collect
;;        (parsec-and (parsec-try (docopt--parse-testcase-blank-lines))
;;                    (docopt--parse-testcase-program))
;;        (docopt--parse-testcase-examples))
;;     (docopt-testcase :program program :examples examples)))

(defun docopt-testcase--strip-comments (s)
  "Strip the comments from S."
  (s-replace-regexp "#.*$" "" s))

(defun docopt--testcase-parse (s)
  "Parse Docopt testcases from the string S."
  (let ((raw (docopt-testcase--strip-comments s)))
    (when (s-starts-with-p "\"\"\"" raw)
      (setq raw (substring s 3)))
    (seq-map (lambda (fixture)
               (seq-let [doc body] (s-split "\"\"\"" fixture)
                 (print doc)
                 (docopt-parse-program doc)))
             (seq-remove #'s-blank-p (s-split "r\"\"\"" raw)))))

(docopt--testcase-parse (f-read-text "test/testcases.docopt"))


(s-split-up-to "a" "abcdabc" 1)

(defun docopt--testcase-test-example (program example)
  "Test the Docopt EXAMPLE of the PROGRAM."
  (let ((argv (docopt-testcase-example-argv example)))
    (condition-case exception
        (let* ((ast (docopt--parse-argv program argv))
               (expected (docopt-testcase-example-expected example)))
          (oset example :ast ast)
          (if (docopt--parsec-error-p ast)
              (oset example :actual 'user-error)
            (oset example :actual (docopt--argv-to-alist program ast))))
      (error (progn (message "Test \"%s\": ERROR:%s " argv exception)
                    (oset example :actual exception))))
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
