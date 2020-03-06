;;; docopt-argv.el --- The Docopt argument vector parser -*- lexical-binding: t -*-

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

;; The Docopt argument vector parser

;;; Code:

(require 'cl-lib)
(require 'docopt-parser)
(require 'docopt-util)
(require 'parsec)

(defun docopt--parsec-error-p (result)
  "Return t if the car of RESULT is a 'parsec-error."
  (and (sequencep result) (equal 'parsec-error (car result))))

(defun docopt--parse-argv-identifier ()
  "Parse a Docopt command line argument identifier."
  (parsec-re "[^ ]+"))

(defun docopt--parse-argv-simple-list* (lst)
  "Parse the Docopt argument vector LST."
  (let ((num-elements (length lst)))
    (if (equal 1 num-elements)
        `(docopt-argv-parser ,(car lst)))
    `(parsec-collect
      ,@(seq-map-indexed
         (lambda (element index)
           (cond
            ((< index (- num-elements 1))
             `(parsec-return (docopt-argv-parser ,element)
                (parsec-optional
                 (parsec-try
                  (parsec-and
                   (parsec-peek
                    (parsec-and
                     (docopt--parse-spaces1)
                     (docopt-argv-parser ,(nth (+ index 1) lst))))
                   (docopt--parse-spaces1))))))
            ((< index num-elements)
             `(docopt-argv-parser ,element))))
         lst))))

(defun docopt--parse-argv-simple-list (lst)
  "Parse the Docopt argument vector LST."
  (let ((result (eval (docopt--parse-argv-simple-list* lst))))
    (if (= 1 (length lst))
        (car result) result)))

(defun docopt--parse-argv-option-argument (option)
  "Parse the argument of the OPTION command line argument."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (docopt--parse-long-option-separator)
                (docopt-argv-parser argument))
    option))

(cl-defgeneric docopt-argv-parser (object)
  "Return an argument vector parser for OBJECT.")

(cl-defmethod docopt-argv-parser ((argument docopt-argument))
  "Return an argument vector parser for ARGUMENT."
  (when-let ((value (docopt--parse-argv-identifier)))
    (oset argument :value value)
    argument))

(cl-defmethod docopt-argv-parser ((option docopt-option))
  "Return an argument vector parser for the long OPTION."
  (parsec-collect (parsec-str (concat (cond
                                       ((docopt-long-option-p option) "--")
                                       ((docopt-short-option-p option) "-"))
                                      (oref option object-name)))
                  (docopt--parse-argv-option-argument option))
  option)

(cl-defmethod docopt-argv-parser ((command docopt-command))
  "Return an argument vector parser for the COMMAND."
  (when (parsec-str (oref command object-name))
    command))

(cl-defmethod docopt-argv-parser ((either docopt-either))
  "Return an argument vector parser for the EITHER."
  (eval `(parsec-or ,@(seq-map (lambda (member) `(docopt-argv-parser (quote ,member)))
                               (docopt-either-members either)))))

(cl-defmethod docopt-argv-parser ((lst list))
  "Return an argument vector parser for the LST."
  (docopt--parse-argv-simple-list (docopt--flatten lst)))

(cl-defmethod docopt-argv-parser ((shortcut docopt-options-shortcut))
  "Return an argument vector parser for the options SHORTCUT."
  (eval `(parsec-sepby
          (parsec-or
           ,@(seq-map (lambda (option) `(docopt-argv-parser (quote ,option)))
                      (apply #'append (docopt-options-shortcut-options shortcut))))
          (docopt--parse-spaces1))))

(cl-defmethod docopt-argv-parser ((group docopt-optional-group))
  "Return an argument vector parser for the GROUP."
  (parsec-optional (docopt-argv-parser (docopt-group-members group))))

(cl-defmethod docopt-argv-parser ((program docopt-program))
  "Return an argument vector parser for the PROGRAM."
  (eval `(parsec-or ,@(seq-map (lambda (pattern) `(parsec-try (docopt-argv-parser (quote ,pattern))))
                               (docopt-program-usage program)))))

(cl-defmethod docopt-argv-parser ((group docopt-required-group))
  "Return an argument vector parser for the GROUP."
  (docopt-argv-parser (docopt-group-members group)))

(cl-defmethod docopt-argv-parser ((pattern docopt-usage-pattern))
  "Return an argument vector parser for the PATTERN."
  (let* ((expressions (docopt-usage-pattern-expressions pattern))
         (num-expressions (length expressions)))
    (seq-let [command exprs]
        (parsec-collect
         (parsec-return (docopt-argv-parser (docopt-usage-pattern-command pattern))
           (unless (zerop num-expressions)
             (docopt--parse-spaces1)))
         (unless (zerop num-expressions)
           (docopt-argv-parser expressions)))
      (cons command (if (listp exprs) exprs (list exprs))))))

;; alist symbol

(cl-defgeneric docopt--argv-symbol (object)
  "Return the symbol for the OBJECT in an alist.")

(cl-defmethod docopt--argv-symbol ((argument docopt-argument))
  "Return the symbol for the ARGUMENT in an alist."
  (intern (concat "<" (oref argument object-name) ">")))

(cl-defmethod docopt--argv-symbol ((command docopt-command))
  "Return the symbol for the COMMAND in an alist."
  (intern (oref command object-name)))

(cl-defmethod docopt--argv-symbol ((option docopt-long-option))
  "Return the symbol for the long OPTION in an alist."
  (intern (concat "--" (oref option object-name))))

(cl-defmethod docopt--argv-symbol ((option docopt-short-option))
  "Return the symbol for the short OPTION in an alist."
  (intern (concat "-" (oref option object-name))))

;; alist element

(cl-defgeneric docopt--argv-alist-element (object default)
  "Return the alist cons for the OBJECT and DEFAULT.")

(cl-defmethod docopt--argv-alist-element ((argument docopt-argument) default)
  "Return the alist cons for the ARGUMENT and DEFAULT."
  (cons (docopt--argv-symbol argument) (docopt-argument-value argument)))

(cl-defmethod docopt--argv-alist-element ((command docopt-command) default)
  "Return the alist cons for the COMMAND and DEFAULT."
  (cons (docopt--argv-symbol command) default))

(cl-defmethod docopt--argv-alist-element ((option docopt-option) default)
  "Return the alist cons for the OPTION and DEFAULT."
  (cons (docopt--argv-symbol option)
        (if-let (argument (docopt-option-argument option))
            (docopt-argument-value argument)
          default)))

(defun docopt-program-default-alist (program)
  "Return the default alist of the Docopt PROGRAM."
  (seq-map (lambda (element) (docopt--argv-alist-element element nil))
           (docopt-program-argv-normalize program)))

(defun docopt--argv-to-alist (program exprs)
  "Convert the Docopt EXPRS for PROGRAM to an alist."
  (let ((result (docopt-program-default-alist program)))
    (seq-doseq (expr (seq-map (lambda (element) (docopt--argv-alist-element element t))
                              (seq-remove #'null exprs)))
      (when-let ((element (assoc (car expr) result)))
        (setcdr element (cdr expr))))
    (cl-sort result #'string< :key #'car)))

(defun docopt--parse-argv (program s)
  "Parse the argument vector S of the Docopt PROGRAM."
  (cdr (parsec-with-input s (docopt-argv-parser program))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here
