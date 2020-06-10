;;; docopt-analyzer.el --- Docopt analyzer -*- lexical-binding: t -*-

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

;; The Docopt anylyzer

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-either)
(require 'docopt-generic)
(require 'docopt-group)
(require 'docopt-option)
(require 'docopt-options-shortcut)
(require 'docopt-program)
(require 'docopt-usage-pattern)
(require 's)
(require 'seq)
(require 'subr-x)

(defun docopt-analyzer--set-repeated (program)
  "Set the :repeat slot of all repeated Docopt elements in PROGRAM to t."
  (let ((arguments (docopt-collect-arguments program)))
    (seq-doseq (repeated (seq-filter #'docopt-repeat-p arguments))
      (seq-doseq (argument arguments)
        (when (equal (docopt-argument-name repeated)
                     (docopt-argument-name argument))
          (docopt-set-repeat argument t))))
    (seq-doseq (usage-pattern (docopt-program-usage program))
      (docopt-usage-pattern-set-repeat usage-pattern))))

(defun docopt-analyzer--program-name (program)
  "Return the name of the PROGRAM from the first usage pattern."
  (when-let ((command (thread-last (docopt-program-usage program)
                        (seq-map #'docopt-usage-pattern-command)
                        (seq-remove #'null)
                        (car))))
    (docopt-command-name command)))

(defun docopt-analyzer--rewrite-options (program)
  "Rewrite the PROGRAM options and arguments."
  (let ((options (seq-filter #'docopt-option-argument (oref program options))))
    (docopt-walk program (lambda (element)
                           (when (listp element)
                             (seq-doseq (option options)
                               (when-let ((found (seq-find (lambda (item)
                                                             (and (equal (type-of option)
                                                                         (type-of item))
                                                                  (equal (docopt-option-name option)
                                                                         (docopt-option-name item))))
                                                           element)))
                                 (let ((index (-elem-index found element)))
                                   (when (equal (nth (+ index 1) element)
                                                (docopt-option-argument found))
                                     (setq element (-remove-at (+ index 1) element)))))))
                           element))))

(defun docopt-analyzer--remove-unknown-options (program)
  "Remove all options from PROGRAM that are not defined in the options section."
  (if (docopt-program-options program)
      (docopt-walk program
                   (lambda (element)
                     (cond
                      ((cl-typep element 'docopt-group)
                       (with-slots (members) element
                         (setq members (delete-dups
                                        (seq-filter (lambda (member)
                                                      (if (cl-typep member 'docopt-option)
                                                          (docopt-program-option program (docopt-option-name member))
                                                        t))
                                                    members)))
                         element))
                      (t element))))
    program))

(defun docopt-analyzer--deduplicate (program)
  "Deduplicate all Docopt elements in PROGRAM."
  (let ((objects (list)))
    (docopt-walk program
                 (lambda (element)
                   (if-let ((found (cl-find element objects :test #'docopt-equal)))
                       found
                     (progn
                       (setq objects (cons element objects))
                       element))))
    program))

(defun docopt-analyzer--assign-incompatible-commands (program)
  "Set the incompatible commands for the PROGRAM."
  (docopt-walk program
               (lambda (element)
                 (when  (docopt-either-all-type-p element 'docopt-command)
                   (let ((commands (apply #'append (docopt-either-members element))))
                     (seq-doseq (command commands)
                       (setf (oref command incompatible)
                             (seq-remove (lambda (current)
                                           (docopt-equal current command))
                                         commands)))))
                 element))
  program)

(defun docopt-analyzer--assign-incompatible-options (program)
  "Set the incompatible options for the PROGRAM."
  (docopt-walk program
               (lambda (element)
                 (when (docopt-either-all-type-p element 'docopt-option)
                   (let ((options (apply #'append (docopt-either-members element))))
                     (seq-doseq (option options)
                       (setf (oref option incompatible)
                             (seq-remove (lambda (current)
                                           (docopt-equal current option))
                                         options)))))
                 element))
  program)

(defun docopt-analyzer--assign-incompatible (program)
  "Assign the incompatible commands and options of PROGRAM."
  (docopt-analyzer--assign-incompatible-commands program)
  (docopt-analyzer--assign-incompatible-options program))

(defun docopt-analyzer--assign-argument-keys (program)
  "Assign the transient argument keys for the PROGRAM."
  (let ((docopt-abbrev-chars docopt-abbrev-upper-chars)
        (arguments (docopt-program-arguments program)))
    (thread-last (seq-map #'docopt-name arguments)
      (seq-map #'upcase)
      (docopt-abbrev-list 1)
      (docopt-assign-keys arguments))))

(defun docopt-analyzer--assign-command-keys (program)
  "Assign the transient command keys for the PROGRAM."
  (let ((docopt-abbrev-chars docopt-abbrev-lower-chars)
        (commands (docopt-program-commands program)))
    (thread-last (seq-map #'docopt-name commands)
      (docopt-abbrev-list 2)
      (docopt-assign-keys commands))))

(defun docopt-analyzer--assign-option-keys (program)
  "Assign the transient option keys for the PROGRAM."
  (let ((docopt-abbrev-chars docopt-abbrev-lower-chars)
        (options (seq-remove (lambda (option)
                               (and (cl-typep option 'docopt-short-option)
                                    (oref option synonym)))
                             (docopt-program-options program))))
    (thread-last (seq-map #'docopt-name options)
      (docopt-abbrev-list 1)
      (seq-map (lambda (key) (concat "-" key)))
      (docopt-assign-keys options))))

(defun docopt-analyzer--assign-keys (program)
  "Assign the transient keys for the PROGRAM."
  (docopt-analyzer--assign-argument-keys program)
  (docopt-analyzer--assign-command-keys program)
  (docopt-analyzer--assign-option-keys program)
  program)

(defun docopt-analyze-program (program)
  "Analyze the Docopt PROGRAM."
  (let ((program (docopt-analyzer--remove-unknown-options (docopt-analyzer--deduplicate program))))
    (docopt-analyzer--assign-incompatible program)
    (with-slots (arguments name options usage) program
      (docopt-set-shortcut-options program options)
      (setq name (docopt-analyzer--program-name program))
      (setq options (docopt-options-merge (docopt-remove-duplicates (docopt-collect-options usage)) options))
      (docopt-analyzer--set-repeated program)
      (docopt-analyzer--rewrite-options program)
      (seq-doseq (option options)
        (when (docopt-long-option-p option)
          (setf (oref option prefixes) (docopt-option-prefixes option options))))
      program)))

(provide 'docopt-analyzer)

;;; docopt-analyzer.el ends here
