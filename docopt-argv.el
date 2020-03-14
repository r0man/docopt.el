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
(require 'seq)

(defun docopt-argv--parse-exprs-any-order (program exprs)
  "Parse the EXPRS of PROGRAM in any order."
  (eval `(parsec-or ,@(seq-map (lambda (exprs) `(parsec-try (docopt-argv-parser ,program (quote ,exprs))))
                               (-permutations exprs)))))

(defun docopt-argv--parse-simple-list* (program lst)
  "Return the form to parse the LST of PROGRAM."
  (let ((num-elements (length lst)))
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

(defun docopt-argv--parse-simple-list (program lst)
  "Parse the LST of PROGRAM."
  (-flatten (eval (docopt-argv--parse-simple-list* program lst))))

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
           (parsec-str (docopt-option-name option))
           (docopt-argv--parse-short-option-argument program option)))
    option))

(defun docopt-argv--parse-long-option (program options)
  "Parse the long OPTIONS of PROGRAM."
  (let ((options (clone (seq-filter #'docopt-long-option-p options))))
    (docopt-set-optional options nil)
    (list (eval `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser ,program ,long-option))
                                       options))))))

(defun docopt-argv--parse-short-options-stacked-arg-0 (program options)
  "Parse the stacked short OPTIONS of PROGRAM that don't have an argument."
  (parsec-many1 (eval `(parsec-or ,@(seq-map (lambda (option) `(docopt-argv--parse-short-option-name ,program ,option))
                                             (seq-remove #'docopt-option-argument (seq-filter #'docopt-short-option-p options)))))))

(defun docopt-argv--parse-short-options-stacked-arg-1 (program options)
  "Parse the stacked short OPTIONS of PROGRAM that have an argument."
  (eval `(parsec-or ,@(seq-map (lambda (option) `(docopt-argv--parse-short-option-name ,program ,option))
                               (seq-filter #'docopt-option-argument (seq-filter #'docopt-short-option-p options))))))

(defun docopt-argv--parse-short-options (program options)
  "Parse the short OPTIONS of PROGRAM."
  (thread-last (parsec-with-error-message
                   (format "Can't parse short options: %s" (cdr parsec-last-error-message))
                 (parsec-or
                  (parsec-try
                   (parsec-and
                    (parsec-str "-")
                    (parsec-collect
                     (docopt-argv--parse-short-options-stacked-arg-0 program options)
                     (parsec-optional (docopt-argv--parse-short-options-stacked-arg-1 program options)))))
                  (parsec-try
                   (parsec-and
                    (parsec-str "-")
                    (parsec-collect
                     (parsec-optional (docopt-argv--parse-short-options-stacked-arg-0 program options))
                     (docopt-argv--parse-short-options-stacked-arg-1 program options))))))
    (seq-remove #'null)
    (-flatten)))

(defun docopt-argv--parse-options (program &optional options)
  "Parse the OPTIONS of PROGRAM."
  (let ((options (or options (docopt-program-options program))))
    (thread-last (parsec-with-error-message
                     (format "Can't parse options: %s" (cdr parsec-last-error-message))
                   (parsec-sepby
                    (parsec-or
                     (docopt-argv--parse-long-option program options)
                     (docopt-argv--parse-short-options program options))
                    (parsec-try (parsec-and (docopt--parse-whitespaces)
                                            (parsec-lookahead (parsec-str "-"))))))
      (apply #'append)
      (seq-remove #'null))))

(cl-defgeneric docopt-argv--parse-option-name (option)
  "Parse the OPTION name.")

(cl-defmethod docopt-argv--parse-option-name ((option docopt-long-option))
  "Parse the long OPTION name."
  (thread-last (cons (docopt-option-name option) (docopt-long-option-prefixes option))
    (seq-map (lambda (name) (concat "\\(?:--\\(" name "\\)\\)")))
    (s-join "\\|")
    (parsec-re)))

(cl-defmethod docopt-argv--parse-option-name ((option docopt-short-option))
  "Parse the short OPTION name."
  (parsec-str (concat "-" (docopt-option-name option))))

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
    (let ((argument (clone argument)))
      (oset argument :value value)
      argument)))

(cl-defmethod docopt-argv-parser (program (argument docopt-argument))
  "Return an argument vector parser for PROGRAM and ARGUMENT."
  (when-let ((value (parsec-and
                     (parsec-lookahead (parsec-none-of ?-))
                     (parsec-re "[^ ]+"))))
    (let ((argument (clone argument)))
      (oset argument :value (if (vectorp (docopt-argument-value argument))
                                (vector value)
                              value))
      argument)))

(cl-defmethod docopt-argv-parser (program (command docopt-command))
  "Return an argument vector parser for the PROGRAM and COMMAND."
  (when (parsec-str (docopt-command-name command))
    (clone command)))

(cl-defmethod docopt-argv-parser (program (either docopt-either))
  "Return an argument vector parser for PROGRAM and  EITHER."
  (with-slots (members) either
    (let* ((results (seq-map-indexed
                     (lambda (exprs index)
                       (let ((result (eval `(parsec-start
                                             (parsec-lookahead
                                              (parsec-try
                                               (docopt-argv-parser
                                                (quote ,program) (quote ,exprs))))))))
                         (cons index (unless (docopt--parsec-error-p result) result))))
                     members))
           (member (nth (caar (seq-sort-by (lambda (result) (length (cdr result))) #'> results)) members)))
      (eval `(docopt-argv-parser (quote ,program) (quote ,member))))))

(cl-defmethod docopt-argv-parser (program (lst list))
  "Return an argument vector parser for PROGRAM and LST."
  (docopt-argv--parse-simple-list program (-flatten lst)))

(cl-defmethod docopt-argv-parser (program (option docopt-option))
  "Return an argument vector parser for PROGRAM and OPTION."
  (seq-let [name argument]
      (parsec-collect
       (docopt-argv--parse-option-name option)
       (when-let ((argument (docopt-option-argument option)))
         (docopt-argv--parse-option-separator option)
         (docopt-argv-parser program argument)))
    (let ((copy (clone option)))
      (when (docopt-option-argument option)
        (oset copy :argument argument))
      copy)))

(cl-defmethod docopt-argv-parser (program (repeated docopt-repeated))
  "Return an argument vector parser for PROGRAM and REPEATED."
  (parsec-sepby (docopt-argv-parser program (docopt-repeated-object repeated))
                (docopt--parse-spaces1)))

(cl-defmethod docopt-argv-parser (program (shortcut docopt-options-shortcut))
  "Return an argument vector parser for PROGRAM and SHORTCUT."
  (docopt-argv--parse-options program))

(cl-defmethod docopt-argv-parser (program (group docopt-optional-group))
  "Return an argument vector parser for the PROGRAM and GROUP."
  (with-slots (members) group
    (cond ((and (< 1 (length members))
                (cl-every #'docopt-option-child-p members))
           (docopt-argv--parse-options program members))
          (t (docopt-argv-parser program members)))))

(cl-defmethod docopt-argv-parser :around (program (object docopt-optionable))
  "Optionally parse the OBJECT of PROGRAM."
  (if (docopt-optional-p object)
      (parsec-optional (cl-call-next-method program object))
    (cl-call-next-method program object)))

(cl-defmethod docopt-argv-parser (_ (program docopt-program))
  "Return an argument vector parser for the PROGRAM."
  (-flatten (eval `(parsec-or ,@(seq-map (lambda (pattern) `(docopt-argv-parser ,program ,pattern))
                                         (docopt-program-usage program))))))

(cl-defmethod docopt-argv-parser (program (group docopt-required-group))
  "Return an argument vector parser for PROGRAM and GROUP."
  (with-slots (members) group
    (if (cl-every #'docopt-option-child-p members)
        (docopt-argv--parse-exprs-any-order program members)
      (docopt-argv-parser program members))))

(cl-defmethod docopt-argv-parser (program (standard-input docopt-standard-input))
  "Return an argument vector parser for PROGRAM and STANDARD-INPUT."
  (when (parsec-optional (parsec-str "-")) standard-input))

(cl-defmethod docopt-argv-parser (program (pattern docopt-usage-pattern))
  "Return an argument vector parser for PROGRAM and PATTERN."
  (let* ((expressions (docopt-usage-pattern-expressions pattern))
         (num-expressions (length expressions)))
    (seq-let [command exprs options]
        (parsec-try
         (parsec-collect
          (docopt--parse-command-name)
          (parsec-return
              (cond
               ((zerop num-expressions)
                (parsec-and (docopt--parse-spaces) nil))
               ((cl-every (lambda (expr)
                            (or (docopt-group-child-p expr)
                                (docopt-option-child-p expr)))
                          expressions)
                (parsec-and
                 (docopt--parse-spaces)
                 (docopt-argv--parse-exprs-any-order program expressions)))
               (t (parsec-and
                   (docopt--parse-spaces)
                   (docopt-argv-parser program expressions)))))
          (parsec-eof)))
      (cons (docopt-command :name command)
            (append (if (listp exprs) exprs (list exprs))
                    options)))))

;; alist symbol

(cl-defgeneric docopt-argv--symbol (object)
  "Return the symbol for the OBJECT in an alist.")

(cl-defmethod docopt-argv--symbol ((argument docopt-argument))
  "Return the symbol for the ARGUMENT in an alist."
  (let ((name (docopt-argument-name argument)))
    (if (s-uppercase? name) (intern name) (intern (concat "<" name ">")))))

(cl-defmethod docopt-argv--symbol ((command docopt-command))
  "Return the symbol for the COMMAND in an alist."
  (intern (docopt-command-name command)))

(cl-defmethod docopt-argv--symbol ((option docopt-long-option))
  "Return the symbol for the long OPTION in an alist."
  (intern (docopt-long-option-format (docopt-option-name option))))

(cl-defmethod docopt-argv--symbol ((standard-input docopt-standard-input))
  "Return the symbol for the STANDARD-INPUT in an alist."
  '-)

(cl-defmethod docopt-argv--symbol ((option docopt-short-option))
  "Return the symbol for the short OPTION in an alist."
  (if (docopt-option-synonym option)
      (intern (docopt-long-option-format (docopt-option-synonym option)))
    (intern (docopt-short-option-format (docopt-option-name option)))))

;; alist value

(cl-defgeneric docopt-argv--value (object default)
  "Return the value for the OBJECT in an alist or DEFAULT.")

(cl-defmethod docopt-argv--value ((object t) default)
  "Return the value for the OBJECT in an alist or DEFAULT."
  default)

(cl-defmethod docopt-argv--value ((argument docopt-argument) default)
  "Return the value for the ARGUMENT in an alist or DEFAULT."
  (docopt-argument-value argument))

(cl-defmethod docopt-argv--value ((option docopt-option) default)
  "Return the value for the OPTION in an alist or DEFAULT."
  (if-let (argument (docopt-option-argument option))
      (or (docopt-argument-value argument)
          (docopt-argument-default argument)
          default)
    default))

(defun docopt-argv--default-alist (program)
  "Return the default alist of the Docopt PROGRAM."
  (seq-map (lambda (element)
             (cons (docopt-argv--symbol element)
                   (if (and (docopt-repeatable-child-p element)
                            (docopt-repeat-p element))
                       (let ((value (docopt-argv--value element nil)))
                         (cond
                          ((null value) [])
                          ((vectorp value) value)
                          (t (vector value))))
                     (docopt-argv--value element nil))))
           (docopt-program-argv-normalize program)))

(defun docopt--argv-to-alist (program exprs)
  "Convert the Docopt EXPRS for PROGRAM to an alist."
  (let ((result (thread-last (cdr exprs)
                  (seq-group-by (lambda (expr)
                                  (cons (eieio-object-class-name expr)
                                        (docopt-name expr))))
                  (seq-map #'cdr)
                  (seq-map (lambda (group)
                             (let ((expr (car group))
                                   (values (seq-map (lambda (expr) (docopt-argv--value expr t)) group)))
                               (cons (docopt-argv--symbol expr)
                                     (cond
                                      ((and (docopt-repeatable-child-p expr)
                                            (docopt-repeat-p expr))
                                       (if (cl-every (lambda (value) (equal t value)) values)
                                           (length values)
                                         (apply #'vector values)))
                                      (t (car values))))))))))
    (seq-doseq (element (docopt-argv--default-alist program))
      (unless (assoc (car element) result)
        (setq result (cons element result))))
    (seq-sort-by #'car #'string< result)))

(defun docopt-argv-parse (program s)
  "Parse the argument vector S of the Docopt PROGRAM."
  (let* ((program (clone program))
         (result (parsec-with-input s (docopt-argv-parser program program))))
    (if (docopt--parsec-error-p result)
        (error "Can't parse Docopt argument vector: %s\n%s" s (cdr result))
      result)))

(provide 'docopt-argv)

;;; docopt-argv.el ends here
