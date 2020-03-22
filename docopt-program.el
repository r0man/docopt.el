;;; docopt-program.el --- The Docopt program class -*- lexical-binding: t -*-

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

;; The Docopt program class

;;; Code:

(require 'docopt-generic)
(require 'docopt-option)
(require 'eieio)
(require 'seq)

(defclass docopt-program ()
  ((arguments
    :accessor docopt-program-arguments
    :documentation "The arguments of the program."
    :initarg :arguments
    :initform nil
    :type (or list null))
   (examples
    :accessor docopt-program-examples
    :documentation "The examples of the program."
    :initarg :examples
    :initform nil
    :type (or list null))
   (footer
    :accessor docopt-program-footer
    :documentation "The footer of the program."
    :initarg :footer
    :initform nil
    :type (or string null))
   (header
    :accessor docopt-program-header
    :documentation "The header of the program."
    :initarg :header
    :initform nil
    :type (or string null))
   (options
    :accessor docopt-program-options
    :documentation "The options of the program."
    :initarg :options
    :initform nil
    :type (or list null))
   (source
    :accessor docopt-program-source
    :documentation "The source of the program."
    :initarg :source
    :initform nil
    :type (or string null))
   (usage
    :accessor docopt-program-usage
    :documentation "The usage information of the program."
    :initarg :usage
    :initform nil
    :type (or list null)))
  "A class representing a Docopt program.")

(cl-defmethod docopt-collect-arguments ((program docopt-program))
  "Collect the arguments from the Docopt PROGRAM."
  (seq-mapcat #'docopt-collect-arguments (docopt-program-usage program)))

(cl-defmethod docopt-collect-commands ((program docopt-program))
  "Collect the commands from the Docopt PROGRAM."
  (seq-mapcat #'docopt-collect-commands (docopt-program-usage program)))

(cl-defmethod docopt-collect-options ((program docopt-program))
  "Collect the options from the Docopt PROGRAM."
  (seq-concatenate
   'list
   (seq-mapcat #'docopt-collect-options (docopt-program-usage program))
   (docopt-program-options program)))

(cl-defmethod docopt-copy ((program docopt-program))
  "Return a copy of the usage PROGRAM."
  (let ((copy (copy-sequence program)))
    (with-slots (arguments examples footer header options source usage) copy
      (setq arguments (docopt-copy (docopt-program-arguments program)))
      (setq examples (docopt-copy (docopt-program-examples program)))
      (setq footer (docopt-copy (docopt-program-footer program)))
      (setq header (docopt-copy (docopt-program-header program)))
      (setq options (docopt-copy (docopt-program-options program)))
      (setq source (docopt-copy (docopt-program-source program)))
      (setq usage (docopt-copy (docopt-program-usage program)))
      copy)))

(cl-defmethod docopt-equal ((program docopt-program) other)
  "Return t if PROGRAM and OTHER are equal-ish."
  (with-slots (arguments usage options) program
    (and (docopt-program-p other)
         (equal arguments (oref other :arguments))
         (equal usage (oref other :usage))
         (equal options (oref other :options)))))

(defun docopt-program-long-options (program)
  "Return the long options of PROGRAM."
  (seq-filter #'docopt-long-option-p (docopt-program-options program)))

(defun docopt-program-short-options (program)
  "Return the short options of PROGRAM."
  (seq-filter #'docopt-short-option-p (docopt-program-options program)))

(defun docopt-program-option (program name)
  "Return the long or short option of PROGRAM by NAME."
  (seq-find (lambda (option) (equal name (oref option object-name)))
            (docopt-program-options program)))

(defun docopt-program-set-sections (program sections)
  "Set the sections of the PROGRAM to SECTIONS."
  (seq-doseq (section sections)
    (seq-let [slot value] section
      (eieio-oset program slot value))))

(defun docopt-program-argv-normalize (program)
  "Return a list of normalized Docopt argv elements for PROGRAM."
  (seq-concatenate 'list
                   (docopt-program-arguments program)
                   (docopt-collect-commands program)
                   (seq-remove (lambda (option)
                                 (and (docopt-short-option-p option)
                                      (docopt-option-synonym option)))
                               (docopt-program-options program))))

(cl-defmethod docopt-walk ((program docopt-program) f)
  "Walk the PROGRAM of an abstract syntax tree and apply F on it."
  (let ((program (copy-sequence program)))
    (with-slots (arguments header examples footer usage options) program
      (setq arguments (docopt-walk arguments f))
      (setq header (docopt-walk header f))
      (setq examples (docopt-walk examples f))
      (setq usage (docopt-walk usage f))
      (funcall f program))))

(defun docopt-program-remove-unknown-options (program)
  "Remove all options from PROGRAM that are not defined in the options section."
  (if (docopt-program-options program)
      (docopt-walk program
                   (lambda (element)
                     (cond
                      ((docopt-group-child-p element)
                       (with-slots (members) element
                         (setq members (delete-dups
                                        (seq-filter (lambda (member)
                                                      (if (docopt-option-child-p member)
                                                          (docopt-program-option program (eieio-object-name-string member))
                                                        t))
                                                    members)))
                         element))
                      (t element))))
    program))

;; (defun docopt-walk-options (program)
;;   (docopt-walk program (lambda (element)
;;                          (cond
;;                           ((docopt-option-child-p element)
;;                            element)

;;                           ((docopt-group-child-p element)
;;                            (docopt-group-members element))

;;                           ((docopt-either-p element)
;;                            (docopt--flatten (apply #'append (docopt-either-members element))))

;;                           ((docopt-repeated-p element)
;;                            (list (docopt-repeated-object element)))

;;                           ((docopt-usage-pattern-p element)
;;                            (apply #'append (docopt-usage-pattern-expressions element)))

;;                           ((docopt-program-p element)
;;                            (seq-mapcat #'docopt--flatten (docopt-program-usage element)))))))

(provide 'docopt-program)

;;; docopt-program.el ends here
