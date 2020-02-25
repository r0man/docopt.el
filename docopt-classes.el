;;; docopt-classes.el --- The Docopt classes -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

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

(defclass docopt-either ()
  ((members
    :initarg :members
    :initform nil
    :accessor docopt-either-members
    :documentation "The members of the either."))
  "A class representing a Docopt either.")

(defun docopt-make-either (&rest members)
  "Make a new Docopt argument using MEMBERS."
  (make-instance 'docopt-either :members members))

(defun docopt-either-concat (&rest eithers)
  "Return a new either made of the concatenation of the members of EITHERS."
  (apply #'docopt-make-either (seq-mapcat #'docopt-either-members eithers)))

(defclass docopt-optionable ()
  ((optional
    :initarg :optional
    :initform nil
    :accessor docopt-optional
    :documentation "Whether the object is optional or not."))
  "A class representing a optional Docopt object.")

(cl-defgeneric docopt-set-optional (object value))

(cl-defmethod docopt-set-optional ((object docopt-optionable) value)
  (oset object :optional value) object)

(cl-defmethod docopt-set-optional ((either docopt-either) value)
  (docopt-set-optional (docopt-either-members either) value)
  either)

(cl-defmethod docopt-set-optional ((objects list) value)
  (seq-doseq (element objects) (docopt-set-optional element value)))

(cl-defmethod docopt-set-optional (object _) object)

;;; Repeatable

(defclass docopt-repeatable ()
  ((repeated
    :initarg :repeated
    :initform nil
    :accessor docopt-repeated
    :documentation "Whether the object is repeatable or not."))
  "A class representing a repeatable Docopt object.")

(cl-defgeneric docopt-set-repeatable (object value))

(cl-defmethod docopt-set-repeatable ((object docopt-repeatable) value)
  (oset object :repeated value) object)

(cl-defmethod docopt-set-repeatable ((either docopt-either) value)
  (docopt-set-repeatable (docopt-either-members either) value)
  either)

(cl-defmethod docopt-set-repeatable ((objects list) value)
  (seq-doseq (object objects) (docopt-set-repeatable object value)))

(cl-defmethod docopt-set-repeatable (object _) object)

;;; Argument

(defclass docopt-argument (docopt-optionable docopt-repeatable)
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

(defclass docopt-option-base (docopt-optionable docopt-repeatable)
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

;; Options Shortcut

(defclass docopt-options-shortcut () ()
  "A class representing a Docopt options shortcut.")

(defun docopt-make-options-shortcut ()
  "Make a new Docopt options shortcut."
  (make-instance 'docopt-options-shortcut))

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

(provide 'docopt-classes)

;;; docopt-classes.el ends here
