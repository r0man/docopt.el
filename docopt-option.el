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

(require 'dash)
(require 'docopt-argument)
(require 'docopt-generic)
(require 'docopt-optional)
(require 'docopt-repeated)
(require 'docopt-value)
(require 'eieio)
(require 'eieio-base)
(require 'seq)
(require 'subr-x)

(defclass docopt-option
  (docopt-optionable docopt-repeatable docopt-value-base)
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
   (name
    :accessor docopt-option-name
    :documentation "The name of the option."
    :initarg :name
    :initform nil
    :type (or string null))
   (incompatible
    :accessor docopt-option-incompatible
    :documentation "The list of incompatible options."
    :initarg :incompatible
    :initform nil
    :type (or list null))
   (synonym
    :accessor docopt-option-synonym
    :documentation "The synonym of the option."
    :initarg :synonym
    :initform nil
    :type (or string null)))
  "A class representing a Docopt base option.")

(cl-defmethod clone ((option docopt-option) &rest params)
  "Return a copy of the OPTION and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method option params)))
    (with-slots (argument description synonym) copy
      (setq argument (clone (docopt-option-argument option)))
      (setq description (clone (docopt-option-description option)))
      (setq synonym (clone (docopt-option-synonym option)))
      copy)))

(cl-defmethod docopt-equal ((option docopt-option) object)
  "Return t if OPTION and OBJECT are equal-ish."
  (and (eieio-object-p option)
       (eieio-object-p object)
       (equal (eieio-object-class option)
              (eieio-object-class object))
       (string= (docopt-option-name option)
                (docopt-option-name object))
       (docopt-equal (docopt-option-argument option)
                     (docopt-option-argument object))))

(cl-defmethod docopt-name ((option docopt-option))
  "Return the name of OPTION."
  (docopt-option-name option))

(cl-defmethod docopt-walk ((option docopt-option) f)
  "Walk the OPTION of an abstract syntax tree and apply F on it."
  (with-slots (argument description synonym) option
    (setq argument (docopt-walk argument f))
    (setq description (docopt-walk description f))
    (setq synonym (docopt-walk synonym f))
    (funcall f option)))

;;; Long Option

(defclass docopt-long-option (docopt-option)
  ((prefixes
    :accessor docopt-long-option-prefixes
    :documentation "The prefixes of the long option."
    :initarg :prefixes
    :initform nil
    :type (or list null)))
  "A class representing a Docopt long option.")

(cl-defmethod clone ((option docopt-long-option) &rest params)
  "Return a copy of the long OPTION and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method option params)))
    (with-slots (prefixes) copy
      (setq prefixes (clone (docopt-long-option-prefixes option)))
      copy)))

(cl-defmethod docopt-argument-list ((option docopt-long-option))
  "Return the shell argument list for the long OPTION."
  (with-slots (argument name value) option
    ;; TODO: Use value from argument
    (list (concat "--" name (when argument (concat " " value))))))

(defun docopt-format--option-argument (option)
  "Convert the Docopt OPTION argument to a formatted string."
  (with-slots (argument value) option
    (when argument (concat "=" (or value (docopt-format argument))))))

(defun docopt-format--option (prefix option)
  "Return the OPTION as a string with PREFIX."
  (let ((s (concat prefix (docopt-option-name option) (docopt-format--option-argument option))))
    (if (docopt-value option) (docopt-bold s) s)))

(cl-defmethod docopt-format ((option docopt-long-option))
  "Convert the Docopt long OPTION to a formatted string."
  (docopt-format--option "--" option))

(defun docopt-string--option-argument (option)
  "Convert the Docopt OPTION argument to a string."
  (when-let ((argument (docopt-option-argument option)))
    (concat "=" (docopt-string argument))))

(cl-defmethod docopt-string ((option docopt-long-option))
  "Convert the Docopt long OPTION to a string."
  (concat "--" (docopt-option-name option) (docopt-string--option-argument option)))

(cl-defmethod docopt-walk ((option docopt-long-option) f)
  "Walk the OPTION of an abstract syntax tree and apply F on it."
  (with-slots (argument description synonym prefixes) option
    (setq argument (docopt-walk argument f))
    (setq description (docopt-walk description f))
    (setq synonym (docopt-walk synonym f))
    (setq prefixes (docopt-walk prefixes f))
    (funcall f option)))

(defun docopt-long-option-format (name)
  "Format the long option NAME."
  (concat "--" name))

;;; Short option

(defclass docopt-short-option (docopt-option) ()
  "A class representing a Docopt short option.")

(defun docopt-short-option-format (name)
  "Format the short option NAME."
  (concat "-" name))

(cl-defmethod docopt-argument-list ((option docopt-short-option))
  "Return the shell argument list for the short OPTION."
  (with-slots (argument name value) option
    ;; TODO: Use value from argument
    (list (concat "-" name (when argument (concat " " value))))))

(cl-defmethod docopt-collect-arguments ((_ docopt-option))
  "Collect the arguments from the Docopt OPTION." nil)

(cl-defmethod docopt-collect-commands ((option docopt-option))
  "Collect the commands from the Docopt OPTION."
  (ignore option) nil)

(cl-defmethod docopt-collect-options ((option docopt-option))
  "Collect the options from the Docopt OPTION." option)

(cl-defmethod docopt-collect-options ((lst list))
  "Collect the options from the list LST."
  (-flatten (seq-map #'docopt-collect-options lst)))

(cl-defmethod docopt-format ((option docopt-short-option))
  "Convert the Docopt short OPTION to a formatted string."
  (docopt-format--option "-" option))

(cl-defmethod docopt-string ((option docopt-short-option))
  "Convert the Docopt short OPTION to a string."
  (concat "-" (docopt-option-name option) (docopt-string--option-argument option)))

(defun docopt-option-set-default (option default)
  "Set the default argument value of OPTION to DEFAULT."
  (when-let ((argument (docopt-option-argument option)))
    (setf (oref argument :default) default)))

(defun docopt-option-set-description-and-default (option description default)
  "Set the DESCRIPTION and DEFAULT of the OPTION."
  (when option
    (setf (oref option :description) description)
    (docopt-option-set-default option default)))

(defun docopt-option-set-synonym (option synonym)
  "Set the :synonym slot of OPTION to SYNONYM."
  (when (and option synonym)
    (setf (oref option :synonym) (docopt-option-name synonym))))

(defun docopt-option-link (long-option short-option description default)
  "Link LONG-OPTION and SHORT-OPTION using DESCRIPTION and DEFAULT."
  (when long-option
    (docopt-option-set-description-and-default long-option description default)
    (docopt-option-set-synonym long-option short-option))
  (when short-option
    (docopt-option-set-description-and-default short-option description default)
    (docopt-option-set-synonym short-option long-option))
  (when (and long-option short-option)
    (let ((long-opt-arg (docopt-option-argument long-option))
          (short-opt-arg (docopt-option-argument short-option)))
      (setf (oref long-option :argument) (or long-opt-arg short-opt-arg))
      (setf (oref short-option :argument) (or short-opt-arg long-opt-arg))))
  (list long-option short-option))

(defun docopt-option-prefixes (option skip-options)
  "Return the prefixes for OPTION computed from the SKIP-OPTIONS."
  (let ((skip-names (thread-last (seq-map #'docopt-option-name skip-options)
                      (delete (docopt-option-name option))
                      (delete (docopt-option-synonym option))))
        (option-name (docopt-option-name option)) )
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
                    (argument-name (docopt-argument :name argument-name))))
         (long-option (when long-name
                        (docopt-long-option
                         :name long-name
                         :argument argument
                         :description description)))
         (short-option (when short-name
                         (docopt-short-option
                          :name short-name
                          :argument argument
                          :description description))))
    (seq-remove #'null (docopt-option-link long-option short-option description default))))

(defun docopt-option-merge (option-1 option-2)
  "Merge OPTION-2 into OPTION-1."
  (cond
   ((and option-1 option-2)
    (with-slots (argument description synonym name) option-1
      (setq argument (docopt-argument-merge argument (docopt-option-argument option-2)))
      (setq description (or description (docopt-option-description option-2)))
      (setq name (or name (docopt-option-name option-2)))
      (setq synonym (or synonym (docopt-option-synonym option-2)))
      option-1))
   ((option-1 option-1))
   ((option-2 option-2))))

(defun docopt-options-merge (options-1 options-2)
  "Merge OPTIONS-2 into OPTIONS-1."
  (thread-last options-1
    (seq-reduce
     (lambda (options option-2)
       (if-let ((option-1 (seq-find (lambda (option-1)
                                      (string= (docopt-option-name option-1)
                                               (docopt-option-name option-2)))
                                    options)))
           (progn (docopt-option-merge option-1 option-2) options)
         (cons option-2 options)))
     options-2)
    (seq-sort-by #'docopt-option-name #'string<)))

(provide 'docopt-option)

;;; docopt-option.el ends here
