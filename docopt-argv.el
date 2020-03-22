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

(require 'cl-generic)
(require 'cl-lib)
(require 'cl-seq)
(require 'dash)
(require 'docopt-parser)
(require 'docopt-util)
(require 'parsec)
(require 's)

(defun docopt--parse-argv-simple-list* (program lst)
  "Return the form to parse the LST of PROGRAM."
  (let ((num-elements (length lst)))
    (if (equal 1 num-elements)
        `(docopt-argv-parser ,program ,(car lst)))
    `(parsec-collect
      ,@(seq-map-indexed
         (lambda (element index)
           (cond
            ((< index (- num-elements 1))
             `(parsec-return (docopt-argv-parser ,program ,element)
                (parsec-optional
                 (parsec-try
                  (parsec-and
                   (parsec-peek
                    (parsec-and
                     (docopt--parse-spaces1)
                     (docopt-argv-parser ,program ,(nth (+ index 1) lst))))
                   (docopt--parse-spaces1))))))
            ((< index num-elements)
             `(docopt-argv-parser ,program ,element))))
         lst))))

(defun docopt--parse-argv-simple-list (program lst)
  "Parse the LST of PROGRAM."
  (docopt--flatten (eval (docopt--parse-argv-simple-list* program lst))))

(defun docopt-argv--parse-short-option-argument (program option)
  "Parse the short OPTION argument of PROGRAM."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (parsec-optional (docopt--parse-short-option-separator))
                (docopt-argv-parser program argument))))

(defun docopt-argv--parse-short-option-name (program option)
  "Parse the short OPTION name of PROGRAM."
  (let ((option (copy-sequence option)))
    (oset option :argument
          (parsec-and
           (parsec-str (eieio-object-name-string option))
           (docopt-argv--parse-short-option-argument program option)))
    option))

(defun docopt-argv--parse-long-option (program)
  "Parse the long options of PROGRAM."
  (let ((options (docopt-copy (docopt-program-long-options program))))
    (docopt-set-optional options nil)
    (list (eval `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser ,program ,long-option))
                                       options))))))

(defun docopt-argv--parse-short-options-stacked-arg-0 (program)
  "Parse the stacked short options of PROGRAM that don't have an argument."
  (parsec-many1 (eval `(parsec-or ,@(seq-map (lambda (option) `(docopt-argv--parse-short-option-name ,program ,option))
                                             (seq-remove #'docopt-option-argument (docopt-program-short-options program)))))))

(defun docopt-argv--parse-short-options-stacked-arg-1 (program)
  "Parse the stacked short options of PROGRAM that have an argument."
  (eval `(parsec-or ,@(seq-map (lambda (option) `(docopt-argv--parse-short-option-name ,program ,option))
                               (seq-filter #'docopt-option-argument (docopt-program-short-options program))))))

(defun docopt-argv--parse-short-options (program)
  "Parse the short options of PROGRAM."
  (thread-last (parsec-with-error-message
                   (format "Can't parse short options: %s" (cdr parsec-last-error-message))
                 (parsec-or
                  (parsec-try
                   (parsec-and
                    (parsec-str "-")
                    (parsec-collect
                     (docopt-argv--parse-short-options-stacked-arg-0 program)
                     (parsec-optional (docopt-argv--parse-short-options-stacked-arg-1 program)))))
                  (parsec-try
                   (parsec-and
                    (parsec-str "-")
                    (parsec-collect
                     (parsec-optional (docopt-argv--parse-short-options-stacked-arg-0 program))
                     (docopt-argv--parse-short-options-stacked-arg-1 program))))))
    (seq-remove #'null)
    (docopt--flatten)))

(defun docopt-argv--parse-options (program)
  "Parse the options of PROGRAM."
  (thread-last (parsec-with-error-message
                   (format "Can't parse options: %s" (cdr parsec-last-error-message))
                 (parsec-sepby
                  (parsec-or
                   (docopt-argv--parse-long-option program)
                   (docopt-argv--parse-short-options program))
                  (parsec-try (parsec-and (docopt--parse-whitespaces)
                                          (parsec-lookahead (parsec-str "-"))))))
    (apply #'append)
    (seq-remove #'null)))

(cl-defgeneric docopt-argv--parse-option-name (option)
  "Parse the OPTION name.")

(cl-defmethod docopt-argv--parse-option-name ((option docopt-long-option))
  "Parse the long OPTION name."
  (thread-last (cons (eieio-object-name-string option) (docopt-long-option-prefixes option))
    (seq-map (lambda (name) (concat "\\(?:--\\(" name "\\)\\)")))
    (s-join "\\|")
    (parsec-re)))

(cl-defmethod docopt-argv--parse-option-name ((option docopt-short-option))
  "Parse the short OPTION name."
  (parsec-str (concat "-" (eieio-object-name-string option))))

(cl-defgeneric docopt-argv--parse-option-separator (option)
  "Parse the OPTION separator.")

(cl-defmethod docopt-argv--parse-option-separator ((option docopt-long-option))
  "Parse the long OPTION separator."
  (docopt--parse-long-option-separator))

(cl-defmethod docopt-argv--parse-option-separator ((option docopt-short-option))
  "Parse the short OPTION separator."
  (parsec-optional (docopt--parse-short-option-separator)))

(cl-defgeneric docopt-argv-parser (program object)
  "Return an argument vector parser for PROGRAM and OBJECT.")

(cl-defmethod docopt-argv-parser (program (argument docopt-argument))
  "Return an argument vector parser for PROGRAM and ARGUMENT."
  (when-let ((value (parsec-and
                     (parsec-lookahead (parsec-none-of ?-))
                     (parsec-re "[^ ]+"))))
    (let ((argument (docopt-copy argument)))
      (oset argument :value value)
      argument)))

(cl-defmethod docopt-argv-parser (program (command docopt-command))
  "Return an argument vector parser for the PROGRAM and COMMAND."
  (when (parsec-str (oref command object-name))
    (docopt-copy command)))

(cl-defmethod docopt-argv-parser (program (either docopt-either))
  "Return an argument vector parser for PROGRAM and  EITHER."
  (let ((form `(parsec-with-error-message
                   (format "Can't parse either: %s" (cdr parsec-last-error-message))
                 (parsec-or ,@(seq-map (lambda (member) `(parsec-try (docopt-argv-parser (quote ,program) (quote ,member))))
                                       (docopt-either-members either))))))
    (if (docopt-optional-p either)
        (eval `(parsec-optional ,form))
      (eval form))))

(cl-defmethod docopt-argv-parser (program (lst list))
  "Return an argument vector parser for PROGRAM and LST."
  (docopt--parse-argv-simple-list program (docopt--flatten lst)))

(cl-defmethod docopt-argv-parser (program (option docopt-option))
  "Return an argument vector parser for PROGRAM and OPTION."
  (seq-let [name argument]
      (parsec-collect
       (docopt-argv--parse-option-name option)
       (when-let ((argument (docopt-option-argument option)))
         (docopt-argv--parse-option-separator option)
         (docopt-argv-parser program argument)))
    (let ((copy (docopt-copy option)))
      (when (docopt-option-argument option)
        (oset copy :argument argument))
      copy)))

(cl-defmethod docopt-argv-parser (program (repeated docopt-repeated))
  "Return an argument vector parser for PROGRAM and REPEATED."
  (parsec-sepby (docopt-argv-parser program (docopt-repeated-object repeated)) (docopt--parse-spaces1)))

(cl-defmethod docopt-argv-parser (program (shortcut docopt-options-shortcut))
  "Return an argument vector parser for PROGRAM and SHORTCUT."
  (docopt-argv--parse-options program))

(cl-defmethod docopt-argv-parser (program (group docopt-optional-group))
  "Return an argument vector parser for the PROGRAM and GROUP."
  (let ((shortcut (docopt-options-shortcut :options (docopt-program-options program)))
        (non-options (seq-remove #'docopt-option-child-p (docopt-group-members group))))
    (docopt-argv-parser program (docopt-interpose-around shortcut non-options))))

(cl-defmethod docopt-argv-parser :around (program (object docopt-optionable))
  "Optionally parse the OBJECT of PROGRAM."
  (if (docopt-optional-p object)
      (parsec-optional (cl-call-next-method program object))
    (cl-call-next-method program object)))

(cl-defmethod docopt-argv-parser (_ (program docopt-program))
  "Return an argument vector parser for the PROGRAM."
  (docopt--flatten (eval `(parsec-or ,@(seq-map (lambda (pattern) `(docopt-argv-parser ,program ,pattern))
                                                (docopt-program-usage program))))))

(cl-defmethod docopt-argv-parser (program (group docopt-required-group))
  "Return an argument vector parser for PROGRAM and GROUP."
  (docopt-argv-parser program (docopt-group-members group)))

(cl-defmethod docopt-argv-parser (program (standard-input docopt-standard-input))
  "Return an argument vector parser for PROGRAM and STANDARD-INPUT."
  (when (parsec-optional (parsec-str "-")) standard-input))

(cl-defmethod docopt-argv-parser (program (pattern docopt-usage-pattern))
  "Return an argument vector parser for PROGRAM and PATTERN."
  (let* ((expressions (docopt-usage-pattern-expressions pattern))
         (num-expressions (length expressions)))
    (seq-let [command exprs]
        (parsec-try
         (parsec-collect
          (docopt--parse-command-name)
          (parsec-return (if (zerop num-expressions)
                             (parsec-and (docopt--parse-spaces) nil)
                           (parsec-and
                            (docopt--parse-spaces)
                            (docopt-argv-parser program expressions)))
            (parsec-eof))))
      (cons (docopt-command command)
            (if (listp exprs) exprs (list exprs))))))

;; alist symbol

(cl-defgeneric docopt--argv-symbol (object)
  "Return the symbol for the OBJECT in an alist.")

(cl-defmethod docopt--argv-symbol ((argument docopt-argument))
  "Return the symbol for the ARGUMENT in an alist."
  (let ((name (eieio-object-name-string argument)))
    (if (s-uppercase? name) (intern name) (intern (concat "<" name ">")))))

(cl-defmethod docopt--argv-symbol ((command docopt-command))
  "Return the symbol for the COMMAND in an alist."
  (intern (oref command object-name)))

(cl-defmethod docopt--argv-symbol ((option docopt-long-option))
  "Return the symbol for the long OPTION in an alist."
  (intern (docopt-long-option-format (oref option object-name))))

(cl-defmethod docopt--argv-symbol ((option docopt-short-option))
  "Return the symbol for the short OPTION in an alist."
  (if (docopt-option-synonym option)
      (intern (docopt-long-option-format (oref option synonym)))
    (intern (docopt-short-option-format (oref option object-name)))))

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
            (or (docopt-argument-value argument)
                (docopt-argument-default argument))
          default)))

(cl-defmethod docopt--argv-alist-element ((standard-input docopt-standard-input) default)
  "Return the alist cons for the STANDARD-INPUT and DEFAULT."
  (cons '- default))

(defun docopt-program-default-alist (program)
  "Return the default alist of the Docopt PROGRAM."
  (seq-map (lambda (element) (docopt--argv-alist-element element nil))
           (docopt-program-argv-normalize program)))

(defun docopt--argv-to-alist (program exprs)
  "Convert the Docopt EXPRS for PROGRAM to an alist."
  (if (docopt--parsec-error-p exprs)
      exprs
    (let ((result (thread-last (seq-remove #'null exprs)
                    (seq-map (lambda (element) (docopt--argv-alist-element element t)))
                    (seq-group-by #'car)
                    (seq-map #'cdr)
                    (seq-map (lambda (group)
                               (cons (caar group)
                                     (let ((values (seq-map #'cdr group)))
                                       (cond
                                        ((= 1 (length values))
                                         (car values))
                                        ((cl-every (lambda (x) (equal t x)) values)
                                         (length values))
                                        (t (apply #'vector values)))))))
                    (seq-sort-by #'car #'string<))))
      (seq-doseq (element (docopt-program-default-alist program))
        (unless (assoc (car element) result)
          (setq result (cons element result))))
      (seq-sort-by #'car #'string< result))))

(defun docopt--parse-argv (program s)
  "Parse the argument vector S of the Docopt PROGRAM."
  (let* ((program (docopt-copy program))
         (result (parsec-with-input s (docopt-argv-parser program program))))
    (if (docopt--parsec-error-p result)
        result (cdr result))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here
