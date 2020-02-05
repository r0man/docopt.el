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

(defclass docopt-option ()
  ((argument
    :initarg :argument
    :accessor docopt-option-argument
    :documentation "The argument of the option.")
   (default
     :initarg :default
     :accessor docopt-option-default
     :documentation "The default of the option.")
   (description
    :initarg :description
    :accessor docopt-option-description
    :documentation "The description of the option.")
   (long-name
    :initarg :long-name
    :accessor docopt-option-long-name
    :documentation "The long name of the option.")
   (short-name
    :initarg :short-name
    :accessor docopt-option-short-name
    :documentation "The short name of the option."))
  "A class representing a DOCOPT option.")

(defun docopt-make-option (&optional description long-name short-name argument default)
  "Make a new DOCOPT option instance.
Initialize the DESCRIPTION, LONG-NAME, SHORT-NAME, ARGUMENT and
DEFAULT slots of the instance."
  (make-instance 'docopt-option
                 :argument argument
                 :default default
                 :description description
                 :long-name long-name
                 :short-name short-name))

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

(defun docopt-spaces ()
  "Parse spaces and newlines."
  (parsec-many-as-string
   (parsec-re "[[:space:]\r\n]")))

(defun docopt-argument-name ()
  "Parse an argument name."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-repeated ()
  "Parse the repeated \"...\" string."
  (parsec-str "..."))

(defun docopt-argument ()
  "Parse an argument."
  (parsec-between
   (parsec-ch ?<) (parsec-ch ?>)
   (docopt-argument-name)))

(defun docopt-newlines ()
  "Parse newlines."
  (parsec-many (parsec-newline)))

(defun docopt-parse-title ()
  "Parse the title."
  (docopt-newlines)
  (parsec-re "\\([^.]+\.\\)"))

(defun docopt-parse-description ()
  "Parse the title."
  (parsec-until (parsec-str "Usage:")))

(defun docopt-examples-str ()
  "Return the \"Examples:\" string parser."
  (parsec-str "Examples:"))

(defun docopt-usage-str ()
  "Return the \"Usage:\" parser."
  (parsec-str "Usage:"))

(defun docopt-program-name ()
  "Parse a usage line."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-command-name ()
  "Parse a command name."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-subcommand-name ()
  "Parse a subcommand name."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-usage-line ()
  "Parse a usage line."
  (parsec-collect
   (docopt-program-name)
   (docopt-spaces)
   (docopt-command-name)
   (docopt-spaces)
   (docopt-subcommand-name)
   (docopt-spaces)))

;; (s-match "[[:alnum:]-_]+" "my_program")

(defun docopt-options-str ()
  "Return the \"Options:\" parser."
  (parsec-str "Options:"))

;; Short Option

(defun docopt-short-option-name ()
  "Parse a short option name."
  (parsec-re "-[[:alnum:]]"))

(defun docopt-short-option-separator ()
  "Parse a short option argument."
  (parsec-re "[[:space:]]"))

(defun docopt-short-option-argument ()
  "Parse a short option argument."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-short-option ()
  "Parse a short option."
  (parsec-collect
   (docopt-short-option-name)
   (parsec-optional (docopt-short-option-separator))
   (docopt-short-option-argument)))

;; Long Option

(defun docopt-long-option-name ()
  "Parse a long option name."
  (substring (parsec-re "--[[:alnum:]]+") 2))

(defun docopt-long-option-argument ()
  "Parse a long option argument."
  (parsec-re "[[:alnum:]-_]+"))

(defun docopt-long-option-separator ()
  "Parse a long option separator."
  (parsec-or (parsec-ch ?=) (parsec-ch ?\s)))

(defun docopt-long-option ()
  "Parse a long option."
  (parsec-or
   (parsec-try
    (parsec-collect
     (docopt-long-option-name)
     (docopt-long-option-separator)
     (docopt-long-option-argument)))
   (docopt-long-option-name)))

;; Option Line

(defun docopt-option-begin ()
  "Parse the beginning of an option line."
  (parsec-and (parsec-re "\s*")
              (parsec-lookahead (parsec-ch ?-))))

(defun docopt-option-separator ()
  "Parse the next option line."
  (parsec-and (parsec-eol) (docopt-option-begin)))

(defun docopt--parse-option-description ()
  "Parse an option description."
  (parsec-many-till-s
   (parsec-any-ch)
   (parsec-or
    (parsec-try (docopt-option-separator))
    (parsec-eof))))

(defun docopt--parse-option ()
  "Parse an option line."
  (let* ((result (parsec-collect
                  (docopt-spaces)
                  (docopt-long-option)
                  (docopt-spaces)
                  (docopt--parse-option-description)))
         (long-name (nth 1 result))
         (description (nth 3 result)))
    (docopt-make-option description long-name)))

(defun docopt--parse-options ()
  "Parse an option lines."
  (parsec-many (docopt--parse-option)))

(defun docopt-blank-line ()
  "Parse a blank line."
  (parsec-collect
   (parsec-many-as-string (parsec-ch ?\s))
   (parsec-eol)))

(defun docopt-parse-document (document)
  (parsec-with-input document
    (parsec-collect
     (parsec-return (docopt-parse-title)
       (docopt-newlines))
     (parsec-until (parsec-str "Usage:") :end)
     (docopt-spaces)
     ;; (parsec-return (docopt-usage-str)
     ;;   (docopt-newlines))
     ;; (docopt-parse-description)
     (parsec-until (parsec-eof)))))

(provide 'docopt)

;;; docopt.el ends here
