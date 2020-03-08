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
  (docopt--flatten (eval (docopt--parse-argv-simple-list* lst))))

(defun docopt-argv--parse-option-list (lst)
  "Parse the list LST of Docopt options in any order."
  (let ((options (apply #'append (eval `(parsec-sepby
                                         (parsec-or ,@(seq-map (lambda (option) `(list (docopt-argv-parser ,option))) lst))
                                         (parsec-try (parsec-and (docopt--parse-spaces1)
                                                                 (parsec-lookahead (parsec-str "-")))))))))
    (let ((expected-options (seq-map #'eieio-object-name-string lst))
          (found-options (seq-map #'eieio-object-name-string options)))
      (when-let ((difference (cl-set-difference expected-options found-options)))
        (parsec-stop
         :message (format "Missing options: %s" difference)
         :expected expected-options
         :found expected-options)))
    options))

(defun docopt--parse-argv-stacked-list (lst)
  "Parse the Docopt argument vector LST."
  (parsec-or (docopt--flatten (docopt-argv-parser (docopt-argv--stack-short-options lst)))
             (docopt-argv--parse-option-list lst)))

(defun docopt-argv--stack-short-options (lst)
  "Parse the Docopt argument vector LST."
  (thread-last lst
    (-partition-by (lambda (element) (not (docopt-short-option-p element))))
    (seq-mapcat (lambda (group)
                  (thread-last (-partition-after-pred
                                (lambda (element)
                                  (and (docopt-short-option-p element)
                                       (docopt-option-argument element)))
                                group)
                    (seq-map (lambda (group)
                               (if (docopt-short-option-p (car group))
                                   (make-instance 'docopt-stacked-short-options :members group)
                                 group))))))
    (docopt--flatten)))

(defun docopt-argv-stack-start-regex (option)
  "Return the regular expression to parse a stacked short OPTION at the beginning."
  (concat "-" (eieio-object-name-string option)))

(defun docopt-argv-stack-element-regex (option)
  "Return the regular expression to parse a stacked short OPTION."
  (let ((name (eieio-object-name-string option)))
    (if (docopt-option-argument option)
        (concat "\\(\s*-\\)?" name "\\(=\\|[\s]+\\)?\\([^\s]+\\)")
      (concat "\\(\s*-\\)?" name))))

(defun docopt-argv-stack-regex (options)
  "Return the regular expression to parse the stacked short OPTIONS."
  (with-slots (members) options
    (concat (docopt-argv-stack-start-regex (car members))
            (thread-last (seq-drop members 1)
              (seq-map (lambda (option) (docopt-argv-stack-element-regex option)))
              (s-join "")))))

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

(defun docopt-argv--parse-stacked-short-option (shortcut)
  "Parse a stacked short option from the Docopt SHORTCUT options."
  (eval `(parsec-or ,@(thread-last (docopt-options-shortcut-options shortcut)
                        (seq-filter #'docopt-short-option-p)
                        (seq-map (lambda (option) `(parsec-try (docopt-argv--parse-short-option ,option))))))))

(defun docopt-argv--parse-stacked-options (shortcut)
  "Parse stacked short options from the Docopt SHORTCUT options."
  (parsec-and (parsec-str "-") (parsec-many1 (docopt-argv--parse-stacked-short-option shortcut))))

(cl-defgeneric docopt-argv-parser (object)
  "Return an argument vector parser for OBJECT.")

(cl-defmethod docopt-argv-parser ((argument docopt-argument))
  "Return an argument vector parser for ARGUMENT."
  (let ((argument (copy-sequence argument)))
    (when-let ((value (docopt--parse-argv-identifier)))
      (oset argument :value value))
    argument))

(cl-defmethod docopt-argv-parser ((command docopt-command))
  "Return an argument vector parser for the COMMAND."
  (when (parsec-str (oref command object-name))
    (copy-sequence command)))

(cl-defmethod docopt-argv-parser ((either docopt-either))
  "Return an argument vector parser for the EITHER."
  (eval `(parsec-or ,@(seq-map (lambda (member) `(docopt-argv-parser (quote ,member)))
                               (docopt-either-members either)))))

(cl-defmethod docopt-argv-parser ((lst list))
  "Return an argument vector parser for the LST."
  (cond ((seq-find #'docopt-short-option-p lst)
         (docopt--parse-argv-stacked-list (docopt--flatten lst)))
        (t (docopt--parse-argv-simple-list (docopt--flatten lst)))))

(cl-defmethod docopt-argv-parser ((option docopt-long-option))
  "Return an argument vector parser for the long OPTION."
  (parsec-try (parsec-and (parsec-str "--") (docopt-argv--parse-long-option option))))

(cl-defmethod docopt-argv-parser ((options docopt-stacked-short-options))
  "Return an argument vector parser for the stacked short OPTIONS."
  (with-slots (members) options
    (let ((group (+ 1 (length members)))
          (members (seq-map #'copy-sequence members)))
      (when-let ((value (eval `(parsec-query (parsec-re ,(docopt-argv-stack-regex options)) :group ,group))))
        (when-let ((argument (docopt-option-argument (car (last members)))))
          (oset argument :value value))))
    members))

(cl-defmethod docopt-argv-parser ((option docopt-short-option))
  "Return an argument vector parser for the short OPTION."
  (parsec-try (parsec-and (parsec-str "-") (docopt-argv--parse-short-option option))))

(cl-defmethod docopt-argv-parser ((repeated docopt-repeated))
  "Return an argument vector parser for the REPEATED."
  (parsec-sepby (docopt-argv-parser (docopt-repeated-object repeated)) (docopt--parse-spaces1)))

(cl-defmethod docopt-argv-parser ((shortcut docopt-options-shortcut))
  "Return an argument vector parser for the options SHORTCUT."
  (apply #'append (eval `(parsec-sepby
                          (parsec-or
                           (parsec-try (docopt-argv--parse-stacked-options (quote ,shortcut)))
                           ,@(seq-map (lambda (option) `(list (docopt-argv-parser (quote ,option))))
                                      (docopt-options-shortcut-options shortcut)))
                          (parsec-try (parsec-and (docopt--parse-spaces1)
                                                  (parsec-lookahead (parsec-str "-"))))))))

(cl-defmethod docopt-argv-parser ((group docopt-optional-group))
  "Return an argument vector parser for the GROUP."
  (parsec-optional (docopt-argv-parser (docopt-group-members group))))

;; (cl-defmethod docopt-argv-parser ((group docopt-optional-group))
;;   "Return an argument vector parser for the GROUP."
;;   (docopt--flatten
;;    (eval `(parsec-optional (parsec-sepby
;;                             (parsec-or ,@(seq-map (lambda (member)
;;                                                     `(docopt-argv-parser (quote ,member)))
;;                                                   (docopt-group-members group)))
;;                             (docopt--parse-spaces))))))

(cl-defmethod docopt-argv-parser ((program docopt-program))
  "Return an argument vector parser for the PROGRAM."
  (docopt--flatten (eval `(parsec-or ,@(seq-map (lambda (pattern) `(parsec-try (docopt-argv-parser (quote ,pattern))))
                                                (docopt-program-usage program))))))

(cl-defmethod docopt-argv-parser ((group docopt-required-group))
  "Return an argument vector parser for the GROUP."
  (docopt-argv-parser (docopt-group-members group)))

(cl-defmethod docopt-argv-parser ((standard-input docopt-standard-input))
  "Return an argument vector parser for the STANDARD-INPUT."
  (when (parsec-optional (parsec-str "-")) standard-input))

(cl-defmethod docopt-argv-parser ((pattern docopt-usage-pattern))
  "Return an argument vector parser for the PATTERN."
  (let* ((expressions (docopt-usage-pattern-expressions pattern))
         (num-expressions (length expressions)))
    (seq-let [command exprs]
        (parsec-collect
         (docopt--parse-command-name)
         (parsec-return (if (zerop num-expressions)
                            (parsec-and (docopt--parse-spaces) nil)
                          (parsec-and
                           (docopt--parse-spaces)
                           (docopt-argv-parser expressions)))
           (parsec-eof)))
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
  (let ((result (parsec-with-input s (docopt-argv-parser program))))
    (if (docopt--parsec-error-p result)
        result (cdr result))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here
