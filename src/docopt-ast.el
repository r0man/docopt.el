;;; docopt-ast.el --- Docopt AST classes -*- lexical-binding: t -*-

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

;; The Docopt AST classes

;;; Code:

;;;; Argument

(require 'cl-lib)
(require 'dash)
(require 'docopt-generic)
(require 'docopt-util)
(require 'eieio)
(require 'eieio-base)
(require 's)

;;;; Key

(defclass docopt-key-base ()
  ((key
    :initarg :key
    :initform nil
    :accessor docopt-key
    :documentation "The key of the Docopt object."))
  "A base class for Docopt objects that have a key slot.")

;;;; Optional

(defclass docopt-optionable ()
  ((optional
    :initarg :optional
    :initform nil
    :accessor docopt-optional-p
    :documentation "Whether the object is optional or not."))
  "A class representing a optional Docopt object.")

(cl-defgeneric docopt-set-optional (object optional)
  "Set the :optional slot of OBJECT to OPTIONAL.")

(cl-defmethod docopt-set-optional ((lst list) optional)
  "Set the :optional slot of the elements of LST to OPTIONAL."
  (seq-doseq (element lst) (docopt-set-optional element optional)))

(cl-defmethod docopt-set-optional ((object docopt-optionable) optional)
  "Set the :optional slot of OBJECT to OPTIONAL."
  (setf (oref object :optional) optional))

(cl-defmethod docopt-set-optional ((object t) optional)
  "Set the :optional slot of OBJECT to OPTIONAL."
  (ignore object optional) nil)

;;;; Repeatable

(defclass docopt-repeatable ()
  ((repeat
    :initarg :repeat
    :initform nil
    :accessor docopt-repeat-p
    :documentation "Whether the object can be repeated or not."))
  "A class providing a :repeat slot for a object.")

(cl-defmethod docopt-set-repeat ((object docopt-repeatable) value)
  "Set the :repeat slot of OBJECT to VALUE."
  (setf (oref object :repeat) value)
  object)

;;;; Value

(defclass docopt-value-base ()
  ((value
    :initarg :value
    :initform nil
    :accessor docopt-value
    :documentation "The value of the Docopt object."))
  "A base class for Docopt objects that have a value slot.")

;;;; Argument

(defclass docopt-argument
  (docopt-key-base docopt-optionable docopt-repeatable docopt-value-base)
  ((default
     :accessor docopt-argument-default
     :documentation "The default of the argument."
     :initarg :default
     :initform nil
     :type (or string vector null))
   (name
    :accessor docopt-argument-name
    :documentation "The name of the argument."
    :initarg :name
    :initform nil
    :type (or string null)))
  "A class representing a argument.")

(cl-defmethod clone ((argument docopt-argument) &rest params)
  "Return a copy of the ARGUMENT and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method argument params)))
    (with-slots (default value name) copy
      (setq default (clone (docopt-argument-default argument)))
      (setq name (clone (docopt-argument-name argument)))
      (setq value (clone (docopt-value argument)))
      copy)))

(cl-defmethod docopt-shell-arguments ((argument docopt-argument))
  "Return the shell argument list for the ARGUMENT."
  (with-slots (value) argument
    (when value (list value))))

(cl-defmethod docopt-equal ((argument docopt-argument) object)
  "Return t if ARGUMENT and OBJECT are equal according to #'docopt-equal."
  (and (docopt-argument-p object)
       (string= (docopt-argument-name argument)
                (docopt-argument-name object))))

(cl-defmethod docopt-collect-arguments ((argument docopt-argument))
  "Collect the arguments from the ARGUMENT."
  (list argument))

(cl-defmethod docopt-collect-arguments ((lst list))
  "Collect the arguments from the list LST."
  (-flatten (seq-map #'docopt-collect-arguments lst)))

(cl-defmethod docopt-format ((argument docopt-argument))
  "Convert the ARGUMENT to a formatted string."
  (with-slots (name value) argument
    (concat "<" (cond
                 (value (docopt-bold value))
                 ((s-uppercase? name) name)
                 (t name)) ">")))

(cl-defmethod docopt-name ((argument docopt-argument))
  "Return the name of ARGUMENT."
  (docopt-argument-name argument))

(cl-defmethod docopt-string ((argument docopt-argument))
  "Convert the ARGUMENT to a string."
  (let ((name (docopt-argument-name argument)))
    (if (s-uppercase? name) name (concat "<" name ">"))))

(cl-defmethod docopt-walk ((argument docopt-argument) f)
  "Walk the ARGUMENT of an abstract syntax tree and apply F on it."
  (with-slots (default name value) argument
    (setq default (docopt-walk default f))
    (setq name (docopt-walk name f))
    (setq value (docopt-walk value f))
    (funcall f argument)))

(defun docopt-argument-merge (argument-1 argument-2)
  "Merge ARGUMENT-2 into ARGUMENT-1."
  (cond
   ((and argument-1 argument-2)
    (with-slots (default name value) argument-1
      (setq default (or default (docopt-argument-default argument-2)))
      (setq value (or value (docopt-value argument-2)))
      (setq name (or name (docopt-argument-name argument-2)))
      argument-1))
   (argument-1 argument-1)
   (argument-2 argument-2)))

;;;; Command

(defclass docopt-command
  (docopt-key-base docopt-optionable docopt-repeatable docopt-value-base)
  ((name
    :accessor docopt-command-name
    :documentation "The name of the command."
    :initarg :name
    :initform nil
    :type (or string null))
   (incompatible
    :accessor docopt-command-incompatible
    :documentation "The list of incompatible commands."
    :initarg :incompatible
    :initform nil
    :type (or list null)))
  "A class representing a command.")

(cl-defmethod clone ((command docopt-command) &rest params)
  "Return a copy of the COMMAND and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method command params)))
    (with-slots (name optional) copy
      (setq name (clone (docopt-command-name command)))
      copy)))

(cl-defmethod docopt-shell-arguments ((command docopt-command))
  "Return the shell argument list for the COMMAND."
  (with-slots (value) command
    (list (docopt-command-name command))))

(cl-defmethod docopt-collect-commands ((command docopt-command))
  "Collect the commands from the COMMAND." command)

(cl-defmethod docopt-format ((command docopt-command))
  "Convert the COMMAND to a formatted string."
  (let ((s (docopt-string command)))
    (if (docopt-value command) (docopt-bold s) s)))

(cl-defmethod docopt-name ((command docopt-command))
  "Return the name of COMMAND."
  (docopt-command-name command))

(cl-defmethod docopt-string ((command docopt-command))
  "Convert the COMMAND to a string."
  (docopt-command-name command))

(cl-defmethod docopt-walk ((command docopt-command) f)
  "Walk the COMMAND of an abstract syntax tree and apply F on it."
  (with-slots (name) command
    (setq name (docopt-walk name f))
    (funcall f command)))

;;;; Either

(defclass docopt-either (docopt-optionable docopt-value-base)
  ((members
    :accessor docopt-either-members
    :documentation "The members of the either."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a either.")

(cl-defmethod clone ((either docopt-either) &rest params)
  "Return a copy of EITHER and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method either params)))
    (with-slots (members) copy
      (setq members (clone (docopt-either-members either)))
      copy)))

(cl-defmethod docopt-shell-arguments ((either docopt-either))
  "Return the shell argument list for the EITHER."
  (thread-last (oref either members)
    (seq-map (lambda (members)
               (seq-mapcat #'docopt-shell-arguments
                           (seq-filter #'docopt-value members))))
    (seq-remove #'null)
    (car)))

(cl-defmethod docopt-format ((either docopt-either))
  "Convert the EITHER to a formatted string."
  (with-slots (members) either
    (s-join " | " (seq-map #'docopt-format members))))

(cl-defmethod docopt-set-repeat ((either docopt-either) value)
  "Set the :repeat slot of the EITHER members to VALUE."
  (docopt-set-repeat (docopt-either-members either) value)
  either)

(cl-defmethod docopt-string ((either docopt-either))
  "Convert the EITHER to a string."
  (with-slots (members) either
    (s-join " | " (seq-map #'docopt-string members))))

(defun docopt-make-either (&rest members)
  "Make a new argument using MEMBERS and OPTIONAL."
  (make-instance 'docopt-either :members members))

(defun docopt-either-concat (&rest eithers)
  "Return a new either made of the concatenation of the members of EITHERS."
  (apply #'docopt-make-either (seq-mapcat #'docopt-either-members eithers)))

(defun docopt-either-all-type-p (either type)
  "Return t if all members of EITHER have a length of 1 and are of TYPE."
  (and (cl-typep either 'docopt-either)
       (cl-every (lambda (members)
                   (and (= 1 (length members))
                        (cl-every (lambda (member)
                                    (cl-typep member type))
                                  members)))
                 (docopt-either-members either))))

(cl-defmethod docopt-collect-arguments ((either docopt-either))
  "Collect the arguments from the EITHER."
  (seq-mapcat #'docopt-collect-arguments (docopt-either-members either)))

(cl-defmethod docopt-collect-commands ((either docopt-either))
  "Collect the commands from the EITHER."
  (seq-mapcat #'docopt-collect-commands (docopt-either-members either)))

(cl-defmethod docopt-collect-options ((either docopt-either))
  "Collect the options from the EITHER."
  (seq-mapcat #'docopt-collect-options (docopt-either-members either)))

(cl-defmethod docopt-walk ((either docopt-either) f)
  "Walk the EITHER of an abstract syntax tree and apply F on it."
  (with-slots (members) either
    (setq members (docopt-walk members f))
    (funcall f either)))

;;;; Group

(defclass docopt-group ()
  ((members
    :accessor docopt-group-members
    :documentation "The argument of the option."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a group.")

(cl-defmethod clone ((group docopt-group) &rest params)
  "Return a copy of the GROUP and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method group params)))
    (setf (oref copy :members) (clone (docopt-group-members group)))
    copy))

(cl-defmethod docopt-shell-arguments ((group docopt-group))
  "Return the shell argument list for the GROUP."
  (with-slots (members) group
    (seq-mapcat #'docopt-shell-arguments members)))

(cl-defmethod docopt-set-repeat ((group docopt-group) value)
  "Set the :repeat slot of the GROUP members to VALUE."
  (docopt-set-repeat (docopt-group-members group) value)
  group)

(cl-defmethod docopt-walk ((group docopt-group) f)
  "Walk the GROUP of an abstract syntax tree and apply F on it."
  (with-slots (members) group
    (setq members (docopt-walk members f))
    (funcall f group)))

;;; Optional Group

(defclass docopt-optional-group (docopt-group docopt-optionable) ()
  "A class representing a required group.")

(defun docopt-make-optional-group (&rest members)
  "Make a new optional group with MEMBERS."
  (let ((group (docopt-optional-group :members members)))
    (docopt-set-optional group t)
    group))

(cl-defmethod docopt-set-optional ((group docopt-optional-group) optional)
  "Set the :optional slot of the GROUP members to OPTIONAL."
  (cl-call-next-method group optional)
  (docopt-set-optional (docopt-group-members group) optional))

(cl-defmethod docopt-format ((group docopt-optional-group))
  "Convert the usage GROUP to a formatted string."
  (with-slots (members) group
    (concat "[" (s-join " " (seq-map #'docopt-format members)) "]")))

(cl-defmethod docopt-string ((group docopt-optional-group))
  "Convert the usage GROUP to a string."
  (with-slots (members) group
    (concat "[" (s-join " " (seq-map #'docopt-string members)) "]")))

;;; Required Group

(defclass docopt-required-group (docopt-group) ()
  "A class representing a required group.")

(defun docopt-make-required-group (&rest members)
  "Make a new required group with MEMBERS."
  (make-instance 'docopt-required-group :members members))

(cl-defmethod docopt-collect-arguments ((group docopt-group))
  "Collect the arguments from the GROUP."
  (docopt-collect-arguments (docopt-group-members group)))

(cl-defmethod docopt-collect-commands ((group docopt-group))
  "Collect the commands from the GROUP."
  (docopt-collect-commands (docopt-group-members group)))

(cl-defmethod docopt-collect-options ((group docopt-group))
  "Collect the options from the GROUP."
  (docopt-collect-options (docopt-group-members group)))

(cl-defmethod docopt-format ((group docopt-required-group))
  "Convert the required GROUP to a formatted string."
  (with-slots (members) group
    (concat "(" (s-join " " (seq-map #'docopt-format members)) ")")))

(cl-defmethod docopt-string ((group docopt-required-group))
  "Convert the required GROUP to a string."
  (with-slots (members) group
    (concat "(" (s-join " " (seq-map #'docopt-string members)) ")")))

(defun docopt-assign-keys (objects keys)
  "Set the key slots of OBJECTS to the values in KEYS."
  (cl-mapcar (lambda (object key)
               (setf (oref object key) key)
               object)
             objects keys))

;;;; Option

(defclass docopt-option
  (docopt-key-base docopt-optionable docopt-repeatable docopt-value-base)
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
  "A class representing a base option.")

(cl-defmethod clone ((option docopt-option) &rest params)
  "Return a copy of the OPTION and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method option params)))
    (with-slots (argument description synonym) copy
      (setq argument (clone (docopt-option-argument option)))
      (setq description (clone (docopt-option-description option)))
      (setq synonym (clone (docopt-option-synonym option)))
      copy)))

(cl-defmethod docopt-equal ((option docopt-option) object)
  "Return t if OPTION and OBJECT are equal according to #'docopt-equal."
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
  "A class representing a long option.")

(cl-defmethod clone ((option docopt-long-option) &rest params)
  "Return a copy of the long OPTION and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method option params)))
    (with-slots (prefixes) copy
      (setq prefixes (clone (docopt-long-option-prefixes option)))
      copy)))

(cl-defmethod docopt-shell-arguments ((option docopt-long-option))
  "Return the shell argument list for the long OPTION."
  (with-slots (argument name value) option
    (if argument
        (when-let ((value (docopt-shell-arguments argument)))
          (cons (concat "--" name) value))
      (when value
        (list (concat "--" name))))))

(defun docopt-format--option-argument (option)
  "Convert the OPTION argument to a formatted string."
  (with-slots (argument value) option
    (when argument (concat "=" (or value (docopt-format argument))))))

(defun docopt-format--option (prefix option)
  "Return the OPTION as a string with PREFIX."
  (let ((s (concat prefix (docopt-option-name option) (docopt-format--option-argument option))))
    (if (docopt-value option) (docopt-bold s) s)))

(cl-defmethod docopt-format ((option docopt-long-option))
  "Convert the long OPTION to a formatted string."
  (docopt-format--option "--" option))

(defun docopt-string--option-argument (option)
  "Convert the OPTION argument to a string."
  (when-let ((argument (docopt-option-argument option)))
    (concat "=" (docopt-string argument))))

(cl-defmethod docopt-string ((option docopt-long-option))
  "Convert the long OPTION to a string."
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
  "A class representing a short option.")

(defun docopt-short-option-format (name)
  "Format the short option NAME."
  (concat "-" name))

(cl-defmethod docopt-shell-arguments ((option docopt-short-option))
  "Return the shell argument list for the short OPTION."
  (with-slots (argument name value) option
    (if argument
        (when-let ((value (docopt-shell-arguments argument)))
          (cons (concat "-" name) value))
      (when value
        (list (concat "-" name))))))

(cl-defmethod docopt-collect-options ((option docopt-option))
  "Collect the options from the OPTION." option)

(cl-defmethod docopt-collect-options ((lst list))
  "Collect the options from the list LST."
  (-flatten (seq-map #'docopt-collect-options lst)))

(cl-defmethod docopt-format ((option docopt-short-option))
  "Convert the short OPTION to a formatted string."
  (docopt-format--option "-" option))

(cl-defmethod docopt-string ((option docopt-short-option))
  "Convert the short OPTION to a string."
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
  "Make a new option line instance.

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
   (option-1 option-1)
   (option-2 option-2)))

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

(defun docopt-option-remove-synonyms (options)
  "Remove all short options from OPTIONS that have a long option synonym."
  (seq-remove (lambda (option-1)
                (and (docopt-short-option-p option-1)
                     (seq-find (lambda (option-2)
                                 (and (docopt-long-option-p option-2)
                                      (string= (oref option-1 name)
                                               (oref option-2 synonym))))
                               options)))
              options))

;;;; Options Shortcut

(defclass docopt-options-shortcut ()
  ((options
    :accessor docopt-options-shortcut-options
    :documentation "The options of the options shortcut."
    :initarg :options
    :initform nil
    :type (or list null)))
  "A class representing a options shortcut.")

(cl-defmethod docopt-shell-arguments ((shortcut docopt-options-shortcut))
  "Return the shell argument list for the options SHORTCUT."
  (with-slots (options) shortcut
    (thread-last options
      (docopt-option-remove-synonyms)
      (seq-mapcat #'docopt-shell-arguments)
      (seq-remove #'null))))

(cl-defmethod docopt-collect-arguments ((shortcut docopt-options-shortcut))
  "Collect the arguments from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-collect-commands ((shortcut docopt-options-shortcut))
  "Collect the commands from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-collect-options ((shortcut docopt-options-shortcut))
  "Collect the options from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-format ((shortcut docopt-options-shortcut))
  "Convert the options SHORTCUT to a formatted string."
  (with-slots (options) shortcut
    (thread-last options
      (docopt-option-remove-synonyms)
      (seq-filter (lambda (option)
                    (if-let ((argument (oref option argument)))
                        (not (null (docopt-value argument)))
                      (not (null (docopt-value option))))))
      (seq-map #'docopt-format)
      (seq-remove #'null)
      (s-join " "))))

(cl-defmethod docopt-string ((shortcut docopt-options-shortcut))
  "Convert the options SHORTCUT to a string."
  (ignore shortcut)
  "[options]")

(cl-defmethod docopt-walk ((shortcut docopt-options-shortcut) f)
  "Walk the SHORTCUT of an abstract syntax tree and apply F on it."
  (with-slots (options) shortcut
    (setq options (docopt-walk options f))
    (funcall f shortcut)))

(defun docopt-make-options-shortcut (&rest options)
  "Make a new options shortcut using OPTIONS."
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

(cl-defmethod docopt-set-shortcut-options ((shortcut docopt-options-shortcut) options)
  "Set the options shortcut in SHORTCUT to OPTIONS."
  (setf (oref shortcut :options) options))

(cl-defmethod docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS."
  (ignore object options))

;;;; Program

(defcustom docopt-string-options-width 20
  "The width of the options on a options line."
  :type 'number
  :group 'docopt)

(defclass docopt-program ()
  ((examples
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
   (name
    :accessor docopt-program-name
    :documentation "The name of the program."
    :initarg :name
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
  "A class representing a program.")

(cl-defmethod docopt-collect-arguments ((program docopt-program))
  "Collect the arguments from the PROGRAM."
  (seq-mapcat #'docopt-collect-arguments (docopt-program-usage program)))

(cl-defmethod docopt-collect-commands ((program docopt-program))
  "Collect the commands from the PROGRAM."
  (seq-mapcat #'docopt-collect-commands (docopt-program-usage program)))

(cl-defmethod docopt-collect-options ((program docopt-program))
  "Collect the options from the PROGRAM."
  (seq-concatenate
   'list
   (seq-mapcat #'docopt-collect-options (docopt-program-usage program))
   (docopt-program-options program)))

(cl-defmethod docopt-set-shortcut-options ((program docopt-program) options)
  "Set the options shortcut in PROGRAM to OPTIONS."
  (docopt-set-shortcut-options (docopt-program-usage program) options))

(defun docopt-collect-standard-input (program)
  "Collect the standard input from the PROGRAM."
  (let ((results nil))
    (docopt-walk program (lambda (element)
                           (if (docopt-standard-input-p element)
                               (setq results (cons element results)))
                           element))
    (cl-remove-duplicates results :test #'docopt-equal)))

(cl-defmethod clone ((program docopt-program) &rest params)
  "Return a copy of the usage PROGRAM and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method program params)))
    (with-slots (examples footer header options source usage) copy
      (setq examples (clone (docopt-program-examples program)))
      (setq footer (clone (docopt-program-footer program)))
      (setq header (clone (docopt-program-header program)))
      (setq options (clone (docopt-program-options program)))
      (setq source (clone (docopt-program-source program)))
      (setq usage (clone (docopt-program-usage program)))
      copy)))

(cl-defmethod docopt-name ((program docopt-program))
  "Return the name of PROGRAM."
  (when-let ((usage-pattern (car (docopt-program-usage program))))
    (docopt-name usage-pattern)))

(cl-defmethod docopt-equal ((program docopt-program) other)
  "Return t if PROGRAM and OTHER are equal according to #'docopt-equal."
  (with-slots (usage options) program
    (and (docopt-program-p other)
         (equal usage (docopt-program-usage other))
         (equal options (docopt-program-options other)))))

(defun docopt-program-arguments (program)
  "Return the arguments of the PROGRAM."
  (cl-remove-duplicates (docopt-collect-arguments program) :test #'docopt-equal))

(defun docopt-program-commands (program)
  "Return the commands of the PROGRAM."
  (cl-remove-duplicates (docopt-collect-commands program) :test #'docopt-equal))

(defun docopt-program-long-options (program)
  "Return the long options of PROGRAM."
  (seq-filter #'docopt-long-option-p (docopt-program-options program)))

(defun docopt-program-short-options (program)
  "Return the short options of PROGRAM."
  (seq-filter #'docopt-short-option-p (docopt-program-options program)))

(defun docopt-program-argument (program name)
  "Return the long or short argument of PROGRAM by NAME."
  (seq-find (lambda (argument) (equal name (docopt-argument-name argument)))
            (docopt-program-arguments program)))

(defun docopt-program-command (program name)
  "Return the long or short command of PROGRAM by NAME."
  (seq-find (lambda (command) (equal name (docopt-command-name command)))
            (docopt-program-commands program)))

(defun docopt-program-option (program name)
  "Return the long or short option of PROGRAM by NAME."
  (seq-find (lambda (option) (equal name (docopt-option-name option)))
            (docopt-program-options program)))

(defun docopt-program-set-sections (program sections)
  "Set the sections of the PROGRAM to SECTIONS."
  (seq-doseq (section sections)
    (pcase-let ((`(,slot ,value) section))
      (eieio-oset program slot value))))

(defun docopt-program-argv-normalize (program)
  "Return a list of normalized argv elements for PROGRAM."
  (seq-concatenate 'list
                   (cl-remove-duplicates (docopt-collect-arguments program) :test #'docopt-equal)
                   (docopt-collect-commands program)
                   (seq-remove (lambda (option)
                                 (and (docopt-short-option-p option)
                                      (docopt-option-synonym option)))
                               (docopt-program-options program))
                   (docopt-collect-standard-input program)))

(cl-defmethod docopt-string ((program docopt-program))
  "Convert the PROGRAM to a string."
  (thread-last (list (docopt-program-header program)
                     (docopt-string--usage (docopt-program-usage program))
                     (docopt-string--options (docopt-program-options program))
                     (docopt-string--examples (docopt-program-examples program)))
    (seq-remove #'s-blank-p)
    (s-join "\n\n")
    (s-trim)))

(cl-defmethod docopt-walk ((program docopt-program) f)
  "Walk the PROGRAM of an abstract syntax tree and apply F on it."
  (with-slots (header examples footer usage options) program
    (setq header (docopt-walk header f))
    (setq examples (docopt-walk examples f))
    (setq usage (docopt-walk usage f))
    (funcall f program)))

(defun docopt-string--section (header content)
  "Convert the section HEADER and CONTENT to a string."
  (unless (zerop (length content))
    (concat header ":\n  " (string-join content "\n  "))))

(defun docopt-string--example (example)
  "Convert the EXAMPLE to a string."
  (string-join example " "))

(defun docopt-string--examples (examples)
  "Convert the EXAMPLES to a string."
  (docopt-string--section "Examples" (seq-map #'docopt-string--example examples)))

(defun docopt-string--usage (usage)
  "Convert the USAGE to a string."
  (docopt-string--section "Usage" (seq-map #'docopt-string usage)))

(defun docopt-string--synonym (synonym)
  "Convert the SYNONYM to a string."
  (when synonym
    (if (= 1 (length synonym))
        (concat "-" synonym)
      (concat "--" synonym))))

(defun docopt-string--options (options)
  "Convert the OPTIONS to a string."
  (docopt-string--section
   "Options" (thread-last options
               (seq-remove (lambda (option)
                             (and (docopt-short-option-p option)
                                  (docopt-option-synonym option))))
               (seq-map (lambda (option)
                          (format (concat "%-" (number-to-string docopt-string-options-width) "s %s")
                                  (concat
                                   (when-let ((synonym (oref option synonym)))
                                     (concat (docopt-string--synonym synonym) ", "))
                                   (docopt-string option))
                                  (or (docopt-option-description option) "")))))))

;;;; Repeated

(defclass docopt-repeated ()
  ((object
    :initarg :object
    :initform nil
    :accessor docopt-repeated-object
    :documentation "The repeated object."))
  "A class representing a repeatable object.")

(defun docopt-make-repeated (object)
  "Make a new argument using OBJECT."
  (docopt-repeated :object (docopt-set-repeat object t)))

(cl-defmethod docopt-shell-arguments ((repeated docopt-repeated))
  "Return the shell argument list for the REPEATED object."
  (with-slots (object) repeated
    (docopt-shell-arguments object)))

(cl-defmethod docopt-collect-arguments ((repeated docopt-repeated))
  "Collect the arguments from the REPEATED."
  (docopt-collect-arguments (docopt-repeated-object repeated)))

(cl-defmethod docopt-collect-commands ((repeated docopt-repeated))
  "Collect the commands from the REPEATED."
  (docopt-collect-commands (docopt-repeated-object repeated)))

(cl-defmethod docopt-collect-options ((repeated docopt-repeated))
  "Collect the options from the REPEATED."
  (docopt-collect-options (docopt-repeated-object repeated)))

(cl-defmethod docopt-format ((repeated docopt-repeated))
  "Convert the usage REPEATED to a formatted string."
  (docopt-format (docopt-repeated-object repeated)))

(cl-defmethod docopt-string ((repeated docopt-repeated))
  "Convert the usage REPEATED to a string."
  (concat (docopt-string (docopt-repeated-object repeated)) "..."))

(cl-defmethod docopt-walk ((repeated docopt-repeated) f)
  "Walk the REPEATED of an abstract syntax tree and apply F on it."
  (with-slots (object) repeated
    (setq object (docopt-walk object f))
    (funcall f repeated)))

;;;; Standard Input

(defclass docopt-standard-input () ()
  "A class representing the standard input.")

(cl-defmethod docopt-collect-arguments ((input docopt-standard-input))
  "Collect the arguments from the standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-collect-commands ((input docopt-standard-input))
  "Collect the commands from the standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-collect-options ((input docopt-standard-input))
  "Collect the options from the standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-name ((input docopt-standard-input))
  "Return the name of standard INPUT."
  (ignore input) "-")

(cl-defmethod docopt-string ((input docopt-standard-input))
  "Convert the standard INPUT to a string."
  (ignore input)
  "[-]")

(cl-defmethod docopt-walk ((input docopt-standard-input) f)
  "Walk the standard INPUT of an abstract syntax tree and apply F on it."
  (funcall f input))

;;;; Usage Pattern

(defclass docopt-usage-pattern ()
  ((command
    :accessor docopt-usage-pattern-command
    :documentation "The command of the usage pattern."
    :initarg :command
    :initform nil
    :type (or docopt-command null))
   (expressions
    :accessor docopt-usage-pattern-expressions
    :documentation "The expressions of the usage pattern."
    :initarg :expressions
    :initform nil
    :type (or list null)))
  "A class representing a usage pattern.")

(cl-defmethod clone ((pattern docopt-usage-pattern) &rest params)
  "Return a copy of the usage PATTERN and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method pattern params)))
    (with-slots (command expressions) copy
      (setq command (clone (docopt-usage-pattern-command pattern)))
      (setq expressions (clone (docopt-usage-pattern-expressions pattern)))
      copy)))

(cl-defmethod docopt-set-shortcut-options ((pattern docopt-usage-pattern) options)
  "Set the options shortcut in PATTERN to OPTIONS."
  (docopt-set-shortcut-options (docopt-usage-pattern-expressions pattern) options))

(cl-defmethod docopt-shell-arguments ((pattern docopt-usage-pattern))
  "Return the shell argument list for the usage PATTERN."
  (with-slots (command expressions) pattern
    (let ((args (append (docopt-shell-arguments command)
                        (seq-mapcat #'docopt-shell-arguments expressions))))
      (if (string= "--" (car (last args)))
          (butlast args)
        args))))

(cl-defmethod docopt-name ((usage-pattern docopt-usage-pattern))
  "Return the name of USAGE-PATTERN."
  (when-let ((command (docopt-usage-pattern-command usage-pattern)))
    (docopt-name command)))

(cl-defmethod docopt-format ((pattern docopt-usage-pattern))
  "Convert the usage PATTERN to a formatted string."
  (with-slots (command expressions) pattern
    (thread-last (seq-map #'docopt-format expressions)
      (seq-remove #'s-blank-p)
      (s-join " ")
      (concat (docopt-string command) " ")
      (s-trim))))

(cl-defmethod docopt-string ((pattern docopt-usage-pattern))
  "Convert the usage PATTERN to a string."
  (with-slots (command expressions) pattern
    (concat (docopt-string command) " " (s-join " " (seq-map #'docopt-string expressions)))))

(cl-defmethod docopt-walk ((pattern docopt-usage-pattern) f)
  "Walk the usage PATTERN of an abstract syntax tree and apply F on it."
  (with-slots (command expressions) pattern
    (setq command (docopt-walk command f))
    (setq expressions (docopt-walk expressions f))
    (funcall f pattern)))

(cl-defmethod docopt-collect-arguments ((usage-pattern docopt-usage-pattern))
  "Collect the arguments from the USAGE-PATTERN."
  (docopt-collect-arguments (docopt-usage-pattern-expressions usage-pattern)))

(cl-defmethod docopt-collect-commands ((usage-pattern docopt-usage-pattern))
  "Collect the commands from the USAGE-PATTERN."
  (docopt-collect-commands (docopt-usage-pattern-expressions usage-pattern)))

(cl-defmethod docopt-collect-options ((usage-pattern docopt-usage-pattern))
  "Collect the options from the USAGE-PATTERN."
  (docopt-collect-options (docopt-usage-pattern-expressions usage-pattern)))

(defun docopt-make-usage-pattern (command &rest expressions)
  "Make a new usage pattern with COMMAND and EXPRESSIONS."
  (make-instance 'docopt-usage-pattern :command command :expressions expressions))

(defun docopt-usage-pattern-collect-repeatable (usage-pattern)
  "Collect all repeatable elements in USAGE-PATTERN."
  (let ((result nil))
    (docopt-walk usage-pattern
                 (lambda (element)
                   (when (cl-typep element 'docopt-repeatable)
                     (setq result (cons element result)))
                   element))
    (reverse result)))

(defun docopt-usage-pattern-set-repeat (usage-pattern)
  "Set the :repeat slot of repeatable elements occurring more then once in USAGE-PATTERN to t."
  (let ((eithers (docopt-by-type usage-pattern 'docopt-either)))
    (thread-last (docopt-usage-pattern-collect-repeatable usage-pattern)
      (seq-group-by (lambda (element)
                      (cons (eieio-object-class-name element)
                            (docopt-name element))))
      (seq-map #'cdr)
      (seq-filter (lambda (group) (> (length group) 1)))
      ;; Don't mark argument as repeated if it appears in all members of an either
      (seq-remove (lambda (group)
                    (and (cl-typep (car group) 'docopt-argument)
                         (cl-some (lambda (either)
                                    (cl-every (lambda (member)
                                                (docopt-find member (car group)))
                                              (oref either members)))
                                  eithers))))
      (seq-map (lambda (group) (docopt-set-repeat group t))))))

(provide 'docopt-ast)

;;; docopt-ast.el ends here
