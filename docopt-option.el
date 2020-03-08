;;; docopt-option.el --- The Docopt option class -*- lexical-binding: t -*-

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

;; The Docopt option class

;;; Code:

(require 'docopt-argument)
(require 'docopt-generic)
(require 'eieio)
(require 'eieio-base)

(defclass docopt-option (eieio-named)
  ((argument
    :accessor docopt-option-argument
    :documentation "The argument of the option."
    :initarg :argument
    :initform nil
    :type (or docopt-argument null))
   (description
    :accessor docopt-option-description
    :documentation "The description of the option."
    :initarg :description
    :initform nil
    :type (or string null))
   (synonym
    :accessor docopt-option-synonym
    :documentation "The synonym of the option."
    :initarg :synonym
    :initform nil
    :type (or string null)))
  "A class representing a Docopt base option.")

(cl-defmethod docopt-walk ((option docopt-option) f)
  "Walk the OPTION of an abstract syntax tree and apply F on it."
  (let ((option (copy-sequence option)))
    (with-slots (argument description synonym) option
      (setq argument (docopt-walk argument f))
      (setq description (docopt-walk description f))
      (setq synonym (docopt-walk synonym f))
      (funcall f option))))

;;; Long Option

(defclass docopt-long-option (docopt-option)
  ((prefixes
    :accessor docopt-long-option-prefixes
    :documentation "The prefixes of the long option."
    :initarg :prefixes
    :initform nil
    :type (or list null)))
  "A class representing a Docopt long option.")

(cl-defmethod docopt-walk ((option docopt-long-option) f)
  "Walk the OPTION of an abstract syntax tree and apply F on it."
  (let ((option (copy-sequence option)))
    (with-slots (argument description synonym prefixes) option
      (setq argument (docopt-walk argument f))
      (setq description (docopt-walk description f))
      (setq synonym (docopt-walk synonym f))
      (setq prefixes (docopt-walk prefixes f))
      (funcall f option))))

(defun docopt-long-option-format (name)
  "Format the long option NAME."
  (concat "--" name))

;;; Short option

(defclass docopt-short-option (docopt-option) ()
  "A class representing a Docopt short option.")

(defun docopt-short-option-format (name)
  "Format the short option NAME."
  (concat "-" name))

;;; Stacked Short Options

(defclass docopt-stacked-short-options ()
  ((members
    :accessor docopt-stacked-short-options-members
    :documentation "The members of the stacked short options."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing stacked Docopt short options.")

(cl-defmethod docopt-collect-arguments ((_ docopt-option))
  "Collect the arguments from the Docopt OPTION." nil)

(cl-defmethod docopt-collect-commands ((option docopt-option))
  "Collect the commands from the Docopt OPTION." nil)

(cl-defmethod docopt-collect-options ((option docopt-option))
  "Collect the options from the Docopt OPTION." option)

(cl-defmethod docopt-collect-options ((lst list))
  "Collect the options from the list LST."
  (delete-dups (docopt--flatten (seq-map #'docopt-collect-options lst))))

(defun docopt-option-set-default (option default)
  "Set the default argument value of OPTION to DEFAULT."
  (when-let ((argument (docopt-option-argument option)))
    (oset argument :default default)))

(defun docopt-option-set-description-and-default (option description default)
  "Set the DESCRIPTION and DEFAULT of the OPTION."
  (when option
    (oset option :description description)
    (docopt-option-set-default option default)))

(defun docopt-option-set-synonym (option synonym)
  "Set the :synonym slot of OPTION to :object-name of SYNONYM."
  (when (and option synonym)
    (oset option :synonym (oref synonym :object-name))))

(defun docopt-option-link (long-option short-option description default)
  "Link LONG-OPTION and SHORT-OPTION using DESCRIPTION and DEFAULT."
  (when long-option
    (docopt-option-set-description-and-default long-option description default)
    (docopt-option-set-synonym long-option short-option))
  (when short-option
    (docopt-option-set-description-and-default short-option description default)
    (docopt-option-set-synonym short-option long-option))
  (when (and long-option short-option)
    (let ((long-opt-arg (oref long-option :argument))
          (short-opt-arg (oref short-option :argument)))
      (oset long-option :argument (or long-opt-arg short-opt-arg))
      (oset short-option :argument (or short-opt-arg long-opt-arg))))
  (list long-option short-option))

(defun docopt-option-prefixes (option skip-options)
  "Return the prefixes for OPTION computed from the SKIP-OPTIONS."
  (let ((skip-names (thread-last (seq-map #'eieio-object-name-string skip-options)
                      (delete (oref option :object-name))
                      (delete (oref option :synonym))))
        (option-name (eieio-object-name-string option)) )
    (thread-last (number-sequence 1 (- (length option-name) 1))
      (seq-map (lambda (length) (substring option-name 0 length)))
      (seq-remove (lambda (prefix)
                    (seq-some (lambda (skip-name)
                                (s-starts-with-p prefix skip-name))
                              skip-names)))
      (nreverse))))

(cl-defun docopt-make-options (&key description default long-name short-name argument argument-name)
  "Make a new Docopt option line instance.

Initialize the DESCRIPTION, DEFAULT, LONG-NAME, SHORT-NAME,
ARGUMENT and ARGUMENT-NAME slots of the instance."
  (let* ((argument (cond
                    ((and argument
                          (object-of-class-p argument 'docopt-argument)) argument)
                    (argument-name (docopt-argument :object-name argument-name))))
         (long-option (when long-name
                        (docopt-long-option
                         :object-name long-name
                         :argument argument
                         :description description)))
         (short-option (when short-name
                         (docopt-short-option
                          :object-name short-name
                          :argument argument
                          :description description))))
    (seq-remove #'null (docopt-option-link long-option short-option description default))))

(defun docopt-option-merge (option-1 option-2)
  "Merge OPTION-2 into OPTION-1."
  (cond
   ((and option-1 option-2)
    (with-slots (argument description synonym object-name) option-1
      ;; (setq argument (or argument (oref option-2 :argument)))
      (setq argument (docopt-argument-merge argument (oref option-2 :argument)))
      (setq description (or description (oref option-2 :description)))
      (setq object-name (or object-name (oref option-2 :object-name)))
      (setq synonym (or synonym (oref option-2 :synonym)))
      option-1))
   ((option-1 option-1))
   ((option-2 option-2))))

(defun docopt-options-merge (options-1 options-2)
  "Merge OPTIONS-2 into OPTIONS-1."
  (thread-last options-1
    (seq-reduce
     (lambda (options option-2)
       (if-let ((option-1 (seq-find (lambda (option-1)
                                      (string= (eieio-object-name-string option-1)
                                               (eieio-object-name-string option-2)))
                                    options)))
           (progn (docopt-option-merge option-1 option-2) options)
         (cons option-2 options)))
     options-2)
    (seq-sort-by #'eieio-object-name-string #'string<)))

(provide 'docopt-option)

;;; docopt-option.el ends here
