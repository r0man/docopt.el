;;; docopt.el --- The DOCOPT Emacs mode -*- lexical-binding: t -*-

;; URL: https://github.com/r0man/docopt.el
;; Keywords: docopt
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A DOCOPT parser in Elisp

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'parsec)
(require 'seq)

(defclass docopt-required ()
  ((required-p
    :initarg :required
    :initform t
    :accessor docopt-required
    :documentation "Whether the object is required or not."))
  "A class representing a required DOCOPT object.")

(defclass docopt-argument (docopt-required)
  ((default
     :initarg :default
     :initform nil
     :accessor docopt-argument-default
     :documentation "The default of the argument.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-argument-name
    :documentation "The name of the argument."))
  "A class representing a DOCOPT argument.")

(defclass docopt-option-base (docopt-required)
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
  "A class representing a DOCOPT base option.")

(defclass docopt-long-option (docopt-option-base) ()
  "A class representing a DOCOPT long option.")

(defclass docopt-short-option (docopt-option-base) ()
  "A class representing a DOCOPT short option.")

(defclass docopt-option-line ()
  ((description
    :initarg :description
    :accessor docopt-option-line-description
    :documentation "The description of the option-line.")
   (long-option
    :initarg :long-option
    :accessor docopt-option-line-long-option
    :documentation "The long name of the option line.")
   (short-option
    :initarg :short-option
    :accessor docopt-option-line-short-option
    :documentation "The short name of the option line."))
  "A class representing a DOCOPT option line.")

(defun docopt-make-argument (&optional name default)
  "Make a new DOCOPT argument instance.
Initialize the NAME and DEFAULT slots of the instance."
  (make-instance 'docopt-argument :name name :default default))

(defun docopt-make-option (&optional description long-name short-name argument)
  "Make a new DOCOPT option instance.
Initialize the DESCRIPTION, LONG-NAME, SHORT-NAME and ARGUMENT
slots of the instance."
  (let ((argument (when argument (make-instance 'docopt-argument :name argument))))
    (make-instance
     'docopt-option-line
     :description description
     :long-option (when long-name
                    (make-instance
                     'docopt-long-option
                     :argument argument
                     :description description
                     :name long-name))
     :short-option (when short-name
                     (make-instance
                      'docopt-short-option
                      :argument argument
                      :description description
                      :name short-name)))))

(defvar docopt-example "
Naval Fate.

Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
")

(defun docopt--parse-whitespace ()
  "Parse spaces and newlines."
  (parsec-re "[[:space:]\r\n]"))

(defun docopt--parse-whitespaces ()
  "Parse spaces and newlines."
  (parsec-many-as-string (docopt--parse-whitespace)))

(defun docopt--parse-newlines ()
  "Parse newlines."
  (parsec-many (parsec-newline)))

(defun docopt--parse-parse-title ()
  "Parse the title."
  (docopt--parse-newlines)
  (parsec-re "\\([^.]+\.\\)"))

(defun docopt--parse-parse-description ()
  "Parse the title."
  (parsec-until (parsec-str "Usage:")))

(defun docopt--parse-examples-str ()
  "Return the \"Examples:\" string parser."
  (parsec-str "Examples:"))

(defun docopt--parse-usage-str ()
  "Return the \"Usage:\" parser."
  (parsec-str "Usage:"))

(defun docopt--parse-program-name ()
  "Parse a usage line."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt--parse-command-name ()
  "Parse a command name."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt--parse-subcommand-name ()
  "Parse a subcommand name."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt--parse-usage-line ()
  "Parse a usage line."
  (parsec-collect
   (docopt--parse-program-name)
   (docopt--parse-whitespaces)
   (docopt--parse-command-name)
   (docopt--parse-whitespaces)
   (docopt--parse-subcommand-name)
   (docopt--parse-whitespaces)))

;; Argument

(defun docopt--parse-identifier ()
  "Parse an identifier."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt--parse-spaceship-argument ()
  "Parse a spaceship argument."
  (docopt-make-argument
   (parsec-between
    (parsec-ch ?<) (parsec-ch ?>)
    (docopt--parse-identifier))))

(defun docopt--parse-upper-case-argument ()
  "Parse an upper case argument."
  (let ((case-fold-search nil))
    (docopt-make-argument (parsec-re "[A-Z0-9_-]+"))))

(defun docopt--parse-argument ()
  "Parse an argument."
  (parsec-or (docopt--parse-spaceship-argument)
             (docopt--parse-upper-case-argument)))

;; Short Option

(defun docopt--parse-short-option-name ()
  "Parse a short option name."
  (substring (parsec-re "-[[:alnum:]]") 1))

(defun docopt--parse-short-option-separator ()
  "Parse a short option separator."
  (parsec-re "[[:space:]]"))

(defun docopt--parse-short-option-argument ()
  "Parse an optional short option argument."
  (parsec-optional
   (parsec-try
    (parsec-and
     (parsec-optional (docopt--parse-short-option-separator))
     (docopt--parse-argument)))))

(defun docopt--parse-short-option ()
  "Parse a short option."
  (seq-let [name argument]
      (parsec-collect
       (docopt--parse-short-option-name)
       (docopt--parse-short-option-argument))
    (make-instance 'docopt-short-option :name name :argument argument)))

;; Long Option

(defun docopt--parse-long-option-name ()
  "Parse a long option name."
  (substring (parsec-re "--[[:alnum:]]+") 2))

(defun docopt--parse-long-option-separator ()
  "Parse a long option separator."
  (parsec-or (parsec-ch ?=) (parsec-ch ?\s)))

(defun docopt--parse-long-option-argument ()
  "Parse an optional long option argument."
  (parsec-and (docopt--parse-long-option-separator)
              (docopt--parse-argument)))

(defun docopt--parse-long-option-without-argument ()
  "Parse a long option without an argument."
  (make-instance 'docopt-long-option :name (docopt--parse-long-option-name)))

(defun docopt--parse-long-option-with-argument ()
  "Parse a long option with an argument."
  (seq-let [name argument]
      (parsec-collect
       (docopt--parse-long-option-name)
       (docopt--parse-long-option-argument))
    (make-instance 'docopt-long-option :name name :argument argument)))

(defun docopt--parse-long-option ()
  "Parse a long option."
  (parsec-or (parsec-try (docopt--parse-long-option-with-argument))
             (docopt--parse-long-option-without-argument)))

;; Options

(defun docopt--parse-options-str ()
  "Return the \"Options:\" parser."
  (parsec-str "Options:"))

(defun docopt--parse-option ()
  "Parse a long or short option."
  (parsec-or (docopt--parse-long-option)
             (docopt--parse-short-option)))

;; Option Line

(defun docopt--parse-option-line-begin ()
  "Parse the beginning of an option line."
  (parsec-and (parsec-re "\s*")
              (parsec-lookahead (parsec-ch ?-))))

(defun docopt--parse-option-line-separator ()
  "Parse the next option line."
  (parsec-and (parsec-eol)
              (docopt--parse-option-line-begin)))

(defun docopt--parse-option-line-description ()
  "Parse an option description."
  (parsec-many-till-s
   (parsec-any-ch)
   (parsec-or (parsec-try (docopt--parse-option-line-separator))
              (parsec-eof))))

(defun docopt--parse-option-line ()
  "Parse an option line."
  (seq-let [_ short-option long-option _ description]
      (parsec-collect
       (docopt--parse-whitespaces)
       (parsec-optional (docopt--parse-short-option))
       (parsec-optional (docopt--parse-long-option))
       (docopt--parse-whitespaces)
       (docopt--parse-option-line-description))
    (when long-option
      (oset long-option :description description))
    (when short-option
      (oset short-option :description description))
    (make-instance
     'docopt-option-line
     :description description
     :long-option long-option
     :short-option short-option)))

(defun docopt--parse-option-lines ()
  "Parse an option lines."
  (parsec-many (docopt--parse-option-line)))

(defun docopt--parse-blank-line ()
  "Parse a blank line."
  (parsec-collect
   (parsec-many-as-string (parsec-ch ?\s))
   (parsec-eol)))

;; Usage Line

(defun docopt--parse-usage-line ()
  "Parse a usage line."
  )

(parsec-with-input "naval_fate ship <name> move <x> <y> [--speed=<kn>]"
  (parsec-collect
   (docopt--parse-command-name)
   (docopt--parse-whitespaces)
   (docopt--parse-subcommand-name)
   (docopt--parse-whitespaces)
   (docopt--parse-argument)
   ;; (parsec-sepby (docopt--parse-argument) (docopt--parse-whitespaces))
   ))

(defun docopt--parse-parse-document (document)
  (parsec-with-input document
    (parsec-collect
     (parsec-return (docopt--parse-parse-title)
       (docopt--parse-newlines))
     (parsec-until (parsec-str "Usage:") :end)
     (docopt--parse-whitespaces)
     ;; (parsec-return (docopt--parse-usage-str)
     ;;   (docopt--parse-newlines))
     ;; (docopt--parse-parse-description)
     (parsec-until (parsec-eof)))))

(provide 'docopt)

;;; docopt.el ends here
