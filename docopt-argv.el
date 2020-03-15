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
(require 'cl-seq)
(require 'dash)
(require 'docopt-parser)
(require 'docopt-util)
(require 'parsec)
(require 's)

(defun docopt--parse-argv-identifier ()
  "Parse a Docopt command line argument identifier."
  (parsec-re "[^ ]+"))

(defun docopt--parse-argv-long-option-argument (option)
  "Parse the argument of the OPTION command line argument."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (docopt--parse-long-option-separator)
                (docopt-argv-parser argument))))

(defun docopt--parse-argv-short-option-argument (option)
  "Parse the argument of the OPTION command line argument."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (parsec-optional (docopt--parse-short-option-separator))
                (docopt-argv-parser argument))))

(defun docopt-argv--parse-long-option (option)
  "Parse the Docopt long OPTION, without dashes."
  (seq-let [_ argument]
      (parsec-collect
       (eval `(parsec-or
               ,@(seq-map
                  (lambda (prefix) `(parsec-str ,prefix))
                  (cons (eieio-object-name-string option)
                        (docopt-long-option-prefixes option)))))
       (docopt--parse-argv-long-option-argument option))
    (let ((option (copy-sequence option)))
      (oset option :argument argument)
      option)))

(defun docopt-argv--parse-short-option (option)
  "Parse the Docopt short OPTION, without dash."
  (let ((option (copy-sequence option)))
    (oset option :argument
          (parsec-and
           (parsec-str (eieio-object-name-string option))
           (docopt--parse-argv-short-option-argument option)))
    option))

(defun docopt-argv--parse-long-option (option)
  "Parse the Docopt long OPTION, without dashes."
  (seq-let [_ argument]
      (parsec-try
       (parsec-collect
        (eval `(parsec-or
                ,@(seq-map
                   (lambda (prefix) `(parsec-str ,prefix))
                   (cons (eieio-object-name-string option)
                         (docopt-long-option-prefixes option)))))
        (docopt--parse-argv-long-option-argument option)))
    (let ((option (copy-sequence option)))
      (oset option :argument argument)
      option)))

(defun docopt-argv--parse-long-options (options)
  "Parse the long OPTIONS."
  (list (eval `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser ,long-option))
                                     (seq-filter #'docopt-long-option-p options))))))

(defun docopt-argv--parse-short-options-stacked-arg-0 (options)
  "Parse the stacked short OPTIONS without an argument."
  (parsec-many (eval `(parsec-or ,@(seq-map (lambda (option)
                                              `(docopt-argv--parse-short-option ,option))
                                            (seq-remove #'docopt-option-argument options))))))

(defun docopt-argv--parse-short-options-stacked-arg-1 (options)
  "Parse the stacked OPTIONS with an argument."
  (eval `(parsec-or ,@(seq-map (lambda (option)
                                 `(docopt-argv--parse-short-option ,option))
                               (seq-filter #'docopt-option-argument options)))))

(defun docopt-argv--parse-short-options (options)
  "Parse the short OPTIONS, possibly stacked."
  (let ((options-arg-0 (seq-remove #'docopt-option-argument options))
        (options-arg-1 (seq-filter #'docopt-option-argument options)))
    (thread-last (parsec-and
                  (parsec-str "-")
                  (parsec-or
                   (parsec-collect
                    (docopt-argv--parse-short-options-stacked-arg-0 options-arg-0)
                    (parsec-optional (docopt-argv--parse-short-options-stacked-arg-1 options-arg-1)))
                   (parsec-collect
                    (parsec-optional (docopt-argv--parse-short-options-stacked-arg-0 options-arg-0))
                    (docopt-argv--parse-short-options-stacked-arg-1 options-arg-1))))
      (seq-remove #'null)
      (docopt--flatten))))

(defun docopt-argv--parse-options (options)
  "Parse the argument vector for OPTIONS."
  (apply #'append (parsec-sepby
                   (parsec-or
                    (docopt-argv--parse-long-options
                     (seq-filter #'docopt-long-option-p options))
                    (docopt-argv--parse-short-options
                     (seq-filter #'docopt-short-option-p options)))
                   (docopt--parse-whitespaces))))

(cl-defgeneric docopt-argv-parser (object)
  "Return an argument vector parser for OBJECT.")

(cl-defmethod docopt-argv-parser ((argument docopt-argument))
  "Return an argument vector parser for ARGUMENT."
  (let ((argument (copy-sequence argument)))
    (when-let ((value (docopt--parse-argv-identifier)))
      (oset argument :value value))
    argument))

(cl-defmethod docopt-argv-parser ((option docopt-long-option))
  "Return an argument vector parser for the long OPTION."
  (parsec-try (parsec-and (parsec-str "--") (docopt-argv--parse-long-option option))))

(cl-defmethod docopt-argv-parser ((option docopt-short-option))
  "Return an argument vector parser for the short OPTION."
  (parsec-try (parsec-and (parsec-str "-") (docopt-argv--parse-short-option option))))

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

(defun docopt-argv--parse-argument ()
  "Parse an argument of a command line argument vector."
  (list (parsec-re "[^ ]+")))

(cl-defgeneric docopt-argv-match (program object arguments)
  "Match the OBJECT of PROGRAM against the ARGUMENTS.")

(cl-defmethod docopt-argv-match (program (argument docopt-argument) arguments)
  "Match the ARGUMENT of PROGRAM against the ARGUMENTS."
  (let ((argument (docopt-copy argument)))
    (oset argument :value (car arguments))
    (list (list argument) (cdr arguments))))

(cl-defmethod docopt-argv-match (program (command docopt-command) arguments)
  "Match the COMMAND of PROGRAM against the ARGUMENTS."
  (when (equal (eieio-object-name-string command) (car arguments))
    (list (list command) (cdr arguments))))

(cl-defmethod docopt-argv-match (program (either docopt-either) arguments)
  "Match the EITHER of PROGRAM against the ARGUMENTS."
  (thread-last (docopt-either-members either)
    (seq-map (lambda (member) (docopt-argv-match program member arguments)))
    (seq-remove (lambda (result) (null (car result))))
    (car)))

(cl-defmethod docopt-argv-match (program (group docopt-group) arguments)
  "Match the GROUP of PROGRAM against the ARGUMENTS."
  (docopt-argv-match program (docopt-group-members group) arguments))

(cl-defmethod docopt-argv-match (program (lst list) arguments)
  "Match the list LST of PROGRAM against the ARGUMENTS."
  (let ((results '()))
    (seq-doseq (element lst)
      (seq-let [match pending]
          (docopt-argv-match program element arguments)
        (when match
          (setq results (append match results)
                arguments pending))))
    (unless (cl-position nil results)
      (list (reverse results) arguments))))

(cl-defmethod docopt-argv-match (program (lst list) arguments)
  "Match the list LST of PROGRAM against the ARGUMENTS."
  (condition-case exception
      (seq-reduce (lambda (state element)
                    (seq-let [result arguments] state
                      (seq-let [match pending] (docopt-argv-match program element arguments)
                        (if match
                            (list (append result match) pending)
                          (error "match-error")))))
                  lst (list nil arguments))
    (error (list nil arguments))))

(cl-defmethod docopt-argv-match (program (option docopt-option) arguments)
  "Match the OPTION of PROGRAM against the ARGUMENTS."
  (when-let ((element (car arguments)))
    (when (and (docopt-option-child-p element)
               (string= (eieio-object-name-string element)
                        (eieio-object-name-string option)))
      (let ((copy (docopt-copy option)))
        (when-let ((argument (docopt-option-argument copy)))
          (oset argument :value (oref (oref element :argument) :value)))
        (list (list copy) (cdr arguments))))))

(cl-defmethod docopt-argv-match (program (pattern docopt-usage-pattern) arguments)
  "Match the PATTERN of PROGRAM against the ARGUMENTS."
  (seq-let [command command-arguments]
      (docopt-argv-match program (docopt-usage-pattern-command pattern) arguments)
    (seq-let [expressions expression-arguments]
        (docopt-argv-match program (docopt-usage-pattern-expressions pattern) command-arguments)
      (when expressions
        (list (append command expressions) expression-arguments)))))

(cl-defmethod docopt-argv-match (program (repeated docopt-repeated) arguments)
  "Match the REPEATED of PROGRAM against the ARGUMENTS."
  (let* ((object (docopt-repeated-object repeated))
         (match (docopt-argv-match program object arguments))
         (results nil))
    (while (and match (not (zerop (length arguments))))
      (setq results (cons (car match) results)
            arguments (cadr match)
            match (docopt-argv-match program object arguments)))
    (when (not (zerop (length results)))
      (list (reverse (apply #'append results)) arguments))))

(cl-defmethod docopt-argv-match (_ (program docopt-program) arguments)
  "Match the PROGRAM against the ARGUMENTS."
  (thread-last (docopt-program-usage program)
    (seq-map (lambda (pattern) (docopt-argv-match program pattern arguments)))
    (seq-remove #'null)
    (caar)))

(defun docopt-argv-parse (program argv)
  "Parse the Docopt command line argument vector ARGV with PROGRAM."
  (docopt-argv-match program program
                     (parsec-with-input argv
                       (apply #'append (parsec-sepby
                                        (parsec-or
                                         (docopt-argv--parse-long-options
                                          (docopt-program-options program))
                                         (docopt-argv--parse-short-options
                                          (docopt-program-options program))
                                         (docopt-argv--parse-argument))
                                        (docopt--parse-whitespaces))))))

(defun docopt-argv-eval (program argv)
  "Evaluate the Docopt command line argument vector ARGV with PROGRAM."
  (docopt--argv-to-alist program (cdr (docopt-argv-parse program argv))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here

;; (docopt-argv-parse docopt-naval-fate "--speed=20")
;; (docopt-argv-parse docopt-naval-fate "naval_fate --help")
;; (docopt-argv-parse docopt-naval-fate "naval_fate --version")
;; (docopt-argv-parse docopt-naval-fate "naval_fate -h")
;; (docopt-argv-parse docopt-naval-fate "naval_fate ship --help")
;; (docopt-argv-parse docopt-naval-fate "naval_fate ship SHIP-123 move 1 2 --speed=20")
;; (docopt-argv-parse docopt-naval-fate "naval_fate ship new SHIP-1 SHIP-2")
