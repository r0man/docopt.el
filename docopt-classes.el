;;; docopt-classes.el --- The Docopt classes -*- lexical-binding: t -*-

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

;; The Docopt classes

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 's)
(require 'seq)
(require 'subr-x)

(defvar docopt-strict-long-options nil
  "Whether to parse long options in strict mode or not.
When t, only allow \"=\" as the long option separator, otherwise
\"=\" and \" \" are allowed.")

(defun docopt--flatten (list)
  "Flatten the LIST."
  (mapcan (lambda (x)
            (if (listp x)
                x (list x)))
          list))

;;; Repeatable

(defclass docopt-repeated ()
  ((object
    :initarg :object
    :initform nil
    :accessor docopt-repeated-object
    :documentation "The repeated object."))
  "A class representing a repeatable Docopt object.")

(defun docopt-make-repeated (object)
  "Make a new Docopt argument using OBJECT."
  (make-instance 'docopt-repeated :object object))

;; (parsec-with-input "ARG..." (docopt--parse-usage-expr))

;; Either

(defclass docopt-either ()
  ((members
    :initarg :members
    :initform nil
    :accessor docopt-either-members
    :documentation "The members of the either."))
  "A class representing a Docopt either.")

(defun docopt-make-either (&rest members)
  "Make a new Docopt argument using MEMBERS and OPTIONAL."
  (make-instance 'docopt-either :members members))

(defun docopt-either-concat (&rest eithers)
  "Return a new either made of the concatenation of the members of EITHERS."
  (apply #'docopt-make-either (seq-mapcat #'docopt-either-members eithers)))

;;; Argument

(defclass docopt-argument ()
  ((default
     :initarg :default
     :initform nil
     :accessor docopt-argument-default
     :documentation "The default of the argument.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-argument-name
    :documentation "The name of the argument.")
   (value
    :initarg :value
    :initform nil
    :accessor docopt-argument-value
    :documentation "The value of the argument."))
  "A class representing a Docopt argument.")

(defun docopt-make-argument (&rest args)
  "Make a new Docopt argument using ARGS."
  (apply 'make-instance 'docopt-argument args))

;;; Command

(defclass docopt-command ()
  ((arguments
    :initarg :arguments
    :initform nil
    :accessor docopt-command-arguments
    :documentation "The arguments of the command.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-command-name
    :documentation "The name of the command.")
   (long-options
    :initarg :long-options
    :initform nil
    :accessor docopt-command-long-options
    :documentation "The long-options of the command.")
   (short-options
    :initarg :short-options
    :initform nil
    :accessor docopt-command-short-options
    :documentation "The short-options of the command."))
  "A class representing a Docopt command.")

(defun docopt-make-command (&rest args)
  "Make a new Docopt command using ARGS."
  (apply 'make-instance 'docopt-command args))

;;; Base Option

(defclass docopt-option-base ()
  ((argument
    :initarg :argument
    :initform nil
    :accessor docopt-option-argument
    :documentation "The argument of the option.")
   (description
    :initarg :description
    :initform nil
    :accessor docopt-option-description
    :documentation "The description of the option.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-option-name
    :documentation "The long name of the option."))
  "A class representing a Docopt base option.")

;;; Long Option

(defclass docopt-long-option (docopt-option-base) ()
  "A class representing a Docopt long option.")

(defun docopt-make-long-option (&rest args)
  "Make a new Docopt long option using ARGS."
  (apply 'make-instance 'docopt-long-option args))

;;; Short option

(defclass docopt-short-option (docopt-option-base) ()
  "A class representing a Docopt short option.")

(defun docopt-make-short-option (&rest args)
  "Make a new Docopt short option using ARGS."
  (apply 'make-instance 'docopt-short-option args))

;; Option line

(defclass docopt-option-line ()
  ((description
    :initarg :description
    :initform nil
    :accessor docopt-option-line-description
    :documentation "The description of the option-line.")
   (long-option
    :initarg :long-option
    :initform nil
    :accessor docopt-option-line-long-option
    :documentation "The long name of the option line.")
   (short-option
    :initarg :short-option
    :initform nil
    :accessor docopt-option-line-short-option
    :documentation "The short name of the option line."))
  "A class representing a Docopt option line.")

(cl-defun docopt-make-option-line (&key description long-name short-name argument argument-name)
  "Make a new Docopt option line instance.
Initialize the DESCRIPTION, LONG-NAME, SHORT-NAME, ARGUMENT and ARGUMENT-NAME
slots of the instance."
  (let ((argument (cond
                   ((and argument
                         (object-of-class-p argument 'docopt-argument)) argument)
                   (argument-name (docopt-make-argument :name argument-name)))))
    (make-instance
     'docopt-option-line
     :description description
     :long-option (when long-name
                    (docopt-make-long-option
                     :argument argument
                     :description description
                     :name long-name))
     :short-option (when short-name
                     (docopt-make-short-option
                      :argument argument
                      :description description
                      :name short-name)))))

;; Program

(defclass docopt-program ()
  ((header
    :initarg :header
    :initform nil
    :accessor docopt-program-header
    :documentation "The header of the program.")
   (examples
    :initarg :examples
    :initform nil
    :accessor docopt-program-examples
    :documentation "The examples of the program.")
   (footer
    :initarg :footer
    :initform nil
    :accessor docopt-program-footer
    :documentation "The footer of the program.")
   (usage
    :initarg :usage
    :initform nil
    :accessor docopt-program-usage
    :documentation "The usage information of the program.")
   (options
    :initarg :options
    :initform nil
    :accessor docopt-program-options
    :documentation "The options of the program."))
  "A class representing a Docopt program.")

(defun docopt-make-program (&rest args)
  "Make a new Docopt program using ARGS."
  (apply 'make-instance 'docopt-program args))

(defun docopt-program-options-list (program)
  "Return a list of long/short option pairs of the Docopt PROGRAM."
  (seq-map (lambda (option-line)
             (list (docopt-option-line-long-option option-line)
                   (docopt-option-line-short-option option-line)))
           (docopt-program-options program)))

;; Group

(defclass docopt-group ()
  ((members
    :initarg :members
    :initform nil
    :accessor docopt-group-members
    :documentation "The argument of the option."))
  "A class representing a Docopt group.")

;;; Optional Group

(defclass docopt-optional-group (docopt-group) ()
  "A class representing a required Docopt group.")

(defun docopt-make-optional-group (&rest members)
  "Make a new optional Docopt group with MEMBERS."
  (make-instance 'docopt-optional-group :members members))

;;; Required Group

(defclass docopt-required-group (docopt-group) ()
  "A class representing a required Docopt group.")

(defun docopt-make-required-group (&rest members)
  "Make a new required Docopt group with MEMBERS."
  (make-instance 'docopt-required-group :members members))

;;; Usage Pattern

(defclass docopt-usage-pattern ()
  ((command
    :initarg :command
    :initform nil
    :accessor docopt-usage-pattern-command
    :documentation "The command of the usage pattern.")
   (expressions
    :initarg :expressions
    :initform nil
    :accessor docopt-usage-pattern-expressions
    :documentation "The expressions of the usage pattern."))
  "A class representing a Docopt usage pattern.")

(defun docopt-make-usage-pattern (command &rest expressions)
  "Make a new Docopt usage pattern with COMMAND and EXPRESSIONS."
  (make-instance 'docopt-usage-pattern :command command :expressions expressions))

;;; Standard Input

(defclass docopt-standard-input () ()
  "A class representing the Docopt standard input.")

(defun docopt-make-standard-input ()
  "Make a new Docopt standard input."
  (make-instance 'docopt-standard-input))

;; Options Shortcut

(defclass docopt-options-shortcut ()
  ((options
    :initarg :options
    :initform nil
    :accessor docopt-options-shortcut-options
    :documentation "The options of the options shortcut."))
  "A class representing a Docopt options shortcut.")

(defun docopt-make-options-shortcut (&rest options)
  "Make a new Docopt options shortcut using OPTIONS."
  (make-instance 'docopt-options-shortcut :options options))

(cl-defgeneric docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS.")

(cl-defmethod docopt-set-shortcut-options ((group docopt-group) options)
  "Set the options shortcut in GROUP to OPTIONS."
  (docopt-set-shortcut-options (docopt-group-members group) options))

(cl-defmethod docopt-set-shortcut-options ((either docopt-either) options)
  "Set the options shortcut in EITHER to OPTIONS."
  (docopt-set-shortcut-options (docopt-either-members either) options))

(cl-defmethod docopt-set-shortcut-options ((lst list) options)
  "Set the options shortcut of the elements in LST to OPTIONS."
  (seq-doseq (element lst) (docopt-set-shortcut-options element options)))

(cl-defmethod docopt-set-shortcut-options ((pattern docopt-usage-pattern) options)
  "Set the options shortcut in PATTERN to OPTIONS."
  (docopt-set-shortcut-options (docopt-usage-pattern-expressions pattern) options))

(cl-defmethod docopt-set-shortcut-options ((program docopt-program) options)
  "Set the options shortcut in PROGRAM to OPTIONS."
  (docopt-set-shortcut-options (docopt-program-usage program) options))

(cl-defmethod docopt-set-shortcut-options ((shortcut docopt-options-shortcut) options)
  "Set the options shortcut in SHORTCUT to OPTIONS."
  (oset shortcut :options options))

(cl-defmethod docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS.")

(provide 'docopt-classes)

;;; docopt-classes.el ends here
