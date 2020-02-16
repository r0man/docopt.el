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
(require 's)
(require 'seq)
(require 'subr-x)

(defvar docopt-strict-long-options nil
  "Whether to parse long options in strict mode or not.
When t, only allow \"=\" as the long option separator, otherwise
\"=\" and \" \" are allowed.")

(defclass docopt-optionable ()
  ((optional
    :initarg :optional
    :initform nil
    :accessor docopt-optional
    :documentation "Whether the object is optional or not."))
  "A class representing a optional DOCOPT object.")

(defclass docopt-repeatable ()
  ((repeated
    :initarg :repeated
    :initform nil
    :accessor docopt-repeated
    :documentation "Whether the object is repeatable or not."))
  "A class representing a repeatable DOCOPT object.")

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
    :documentation "The name of the argument."))
  "A class representing a DOCOPT argument.")

(defun docopt-make-argument (&rest args)
  "Make a new DOCOPT argument using ARGS."
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
  "A class representing a DOCOPT command.")

(defun docopt-make-command (&rest args)
  "Make a new DOCOPT command using ARGS."
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
  "A class representing a DOCOPT base option.")

;;; Long Option

(defclass docopt-long-option (docopt-option-base) ()
  "A class representing a DOCOPT long option.")

(defun docopt-make-long-option (&rest args)
  "Make a new DOCOPT long option using ARGS."
  (apply 'make-instance 'docopt-long-option args))

;;; Short option

(defclass docopt-short-option (docopt-option-base) ()
  "A class representing a DOCOPT short option.")

(defun docopt-make-short-option (&rest args)
  "Make a new DOCOPT short option using ARGS."
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
  "A class representing a DOCOPT option line.")

(cl-defun docopt-make-option-line (&key description long-name short-name argument argument-name)
  "Make a new DOCOPT option line instance.
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

(defmacro docopt--parse-sep-end-by1 (parser sep)
  "Parse one or more occurrences of PARSER, separated and optionally ended by SEP."
  `(cons ,parser (parsec-return (parsec-many (parsec-try (parsec-and ,sep ,parser)))
                   (parsec-optional ,sep))))

(defmacro docopt--parse-sep-end-by (parser sep)
  "Parse zero or more occurrences of PARSER, separated and optionally ended by SEP."
  `(parsec-optional (docopt--parse-sep-end-by1 ,parser ,sep)))

(defmacro docopt--parse-sep-by1 (parser sep)
  "Parse one or more occurrences of PARSER, separated by SEP."
  `(cons ,parser (parsec-many (parsec-and ,sep ,parser))))

(defmacro docopt--parse-group (open close parser)
  "Parse a group between OPEN and CLOSE using PARSER."
  `(parsec-between (parsec-ch ,open) (parsec-ch ,close) ,parser))

(defun docopt--parse-pipe ()
  "Parse a pipe."
  (parsec-or (parsec-ch ?\|) (parsec-str "'|'")))

(defun docopt--parse-space ()
  "Parse a space character."
  (parsec-ch ?\s))

(defun docopt--parse-spaces ()
  "Parse many space characters."
  (parsec-many-s (docopt--parse-space)))

(defun docopt--parse-whitespace ()
  "Parse a space character, newline or a CRLF."
  (parsec-or (docopt--parse-space) (parsec-eol)))

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

(defun docopt--parse-default (s)
  "Parse the default value from S."
  (when s (nth 1 (s-match "\\[default:\s*\\([^] ]+\\)\s*\\]" s))))

(defun docopt--parse-examples-str ()
  "Return the \"Examples:\" string parser."
  (parsec-str "Examples:"))

(defun docopt--parse-options-shortcut ()
  "Parse the string \"[options]\", aka the options shortcut."
  (parsec-str "[options]"))

(defun docopt--parse-command-name ()
  "Parse a command name."
  (parsec-re "[[:alnum:]][[:alnum:]-_]*"))

;; Argument

(defun docopt--parse-argument-spaceship ()
  "Parse a spaceship argument."
  (docopt-make-argument
   :name (parsec-between
          (parsec-ch ?<) (parsec-ch ?>)
          (parsec-re "[[:alnum:]][[:alnum:]-_]*"))))

(defun docopt--parse-argument-upper-case ()
  "Parse an upper case argument."
  (let ((case-fold-search nil))
    (docopt-make-argument :name (parsec-re "[A-Z0-9][A-Z0-9_-]*"))))

(defun docopt--parse-argument ()
  "Parse an argument."
  (parsec-or (docopt--parse-argument-spaceship)
             (docopt--parse-argument-upper-case)))

;; Long Option

(defun docopt--parse-long-option-name ()
  "Parse a long option name."
  (substring (parsec-re "--[[:alnum:]-_]+") 2))

(defun docopt--parse-long-option-separator ()
  "Parse a long option separator."
  (if docopt-strict-long-options
      (parsec-ch ?=)
    (parsec-or (parsec-ch ?=) (parsec-ch ?\s))))

(defun docopt--parse-long-option ()
  "Parse a long option."
  (seq-let [name argument]
      (parsec-collect
       (docopt--parse-long-option-name)
       (parsec-optional
        (parsec-try
         (parsec-and (docopt--parse-long-option-separator)
                     (docopt--parse-argument)))))
    (docopt-make-long-option :name name :argument argument)))

;; Short Option

(defun docopt--parse-short-option-name ()
  "Parse a short option name."
  (substring (parsec-re "-[[:alnum:]]") 1))

(defun docopt--parse-short-option-separator ()
  "Parse a short option separator."
  (docopt--parse-whitespace))

(defun docopt--parse-short-option ()
  "Parse a short option."
  (seq-let [name argument]
      (parsec-collect
       (docopt--parse-short-option-name)
       (parsec-optional
        (parsec-try
         (parsec-and
          (parsec-optional (docopt--parse-short-option-separator))
          (docopt--parse-argument)))))
    (docopt-make-short-option :name name :argument argument)))

(defun docopt--parse-short-options-stacked ()
  "Parse stacked short options."
  (seq-map (lambda (short-char)
             (docopt-make-short-option :name (char-to-string short-char)))
           (substring (parsec-re "-[[:alnum:]][[:alnum:]]+") 1)))

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
    (let ((default (docopt--parse-default description)))
      (when long-option
        (oset long-option :description description)
        (when-let ((argument (docopt-option-argument long-option)))
          (oset argument :default default)))
      (when short-option
        (oset short-option :description description)
        (when-let ((argument (docopt-option-argument short-option)))
          (oset argument :default default)))
      (make-instance
       'docopt-option-line
       :description description
       :long-option long-option
       :short-option short-option))))

(defun docopt--parse-option-lines ()
  "Parse an option lines."
  (parsec-many (docopt--parse-option-line)))

;; Repeatable

(defun docopt--parse-ellipsis ()
  "Parse the repeatable identifier."
  (parsec-str "..."))

(defun docopt-repeatable--set-maybe (repeatable-obj value)
  "Set the :repeated slot of REPEATABLE-OBJ to VALUE when supported."
  (when (docopt-repeatable-child-p repeatable-obj)
    (oset repeatable-obj :repeated value)))

(defun docopt-repeatable--set-maybe-seq (seq value)
  "Set the :repeated slot of the items in SEQ to VALUE when supported."
  (seq-doseq (element seq) (docopt-repeatable--set-maybe element value)))

(defun docopt--expr-set-repeatable (expr value)
  "Set the :repeated slot of all items in EXPR to VALUE if they support it."
  (cond
   ((sequencep expr) (docopt-repeatable--set-maybe-seq expr value))
   (t (docopt-repeatable--set-maybe expr value))))

(defmacro docopt--parse-repeatable (parser)
  "Parse a repeatable expression with PARSER."
  (let ((object (make-symbol "object"))
        (ellipsis (make-symbol "ellipsis")))
    `(seq-let [,object ,ellipsis]
         (parsec-collect ,parser (parsec-optional (docopt--parse-ellipsis)))
       (when ,ellipsis
         (docopt--expr-set-repeatable ,object t))
       ,object)))

;; Usage Expression

(defun docopt-optionable--set-maybe (optional-obj value)
  "Set the :optional slot of OPTIONAL-OBJ to VALUE when supported."
  (when (docopt-optionable-child-p optional-obj)
    (oset optional-obj :optional value)))

(defun docopt-optionable--set-maybe-seq (seq value)
  "Set the :optional slot of the items in SEQ to VALUE when supported."
  (seq-doseq (element seq) (docopt-optionable--set-maybe element value)))

(defun docopt-optionable--set-maybe-seq-nested (nested-seq value)
  "Set the :optional slot of the items in the NESTED-SEQ to VALUE when supported."
  (seq-mapcat (lambda (seq) (docopt-optionable--set-maybe-seq seq value)) nested-seq))

(defun docopt--expr-set-optional (expr value)
  "Set the :optional slot of all items in EXPR to VALUE if they support it."
  (cond
   ((and (sequencep expr) (sequencep (car expr)))
    (docopt-optionable--set-maybe-seq-nested expr value))
   ((sequencep expr)
    (docopt-optionable--set-maybe-seq expr value))
   (t expr)))

(defun docopt--parse-usage-expr-group (open close)
  "Parse an expression group between OPEN and CLOSE."
  (docopt--parse-group open close (docopt--parse-usage-expr-seq)))

(defun docopt--parse-optional-group ()
  "Parse a optional expression group."
  (docopt--expr-set-optional (docopt--parse-usage-expr-group ?\[ ?\]) t))

(defun docopt--parse-required-group ()
  "Parse a required expression group."
  (docopt--expr-set-optional (docopt--parse-usage-expr-group ?\( ?\)) nil))

(defun docopt--parse-usage-expr-atom ()
  "Parse an atom of a usage expression."
  (parsec-or (docopt--parse-option)
             (docopt--parse-argument)
             (docopt--parse-usage-command)))

(defun docopt--parse-either-end ()
  "Parse the end of an either expression."
  (parsec-and (parsec-re "\s*|\s*") (docopt--parse-usage-expr-seq)))

(defun docopt--parse-either-start (expr)
  "Parse the start of an either expression or return EXPR."
  (if-let ((sep (parsec-peek-p (parsec-re "\s*|\s*"))))
      (cons expr (docopt--parse-either-end))
    expr))

(defun docopt--parse-usage-expr ()
  "Parse an atom of a usage line expression."
  (docopt--parse-repeatable
   (docopt--parse-either-start
    (parsec-or (parsec-try (docopt--parse-short-options-stacked))
               (docopt--parse-required-group)
               (docopt--parse-optional-group)
               (docopt--parse-usage-expr-atom)))))

(defun docopt--parse-usage-expr-seq ()
  "Parse an expression sequence."
  (parsec-sepby (docopt--parse-usage-expr) (docopt--parse-spaces)))

;; Usage Section

(defun docopt--parse-usage-command ()
  "Parse a command in a usage pattern."
  (docopt-make-command :name (docopt--parse-command-name)))

(defun docopt--parse-usage-header ()
  "Parse the \"Usage:\" header."
  (parsec-str "Usage:"))

(defun docopt--parse-usage-line ()
  "Parse a usage line."
  (parsec-and (docopt--parse-spaces) (docopt--parse-usage-expr-seq)))

(defun docopt--parse-usage-patterns ()
  "Parse the usage patterns."
  (parsec-and (docopt--parse-usage-header)
              (parsec-sepby (docopt--parse-usage-line) (parsec-eol))))

(provide 'docopt)

;;; docopt.el ends here
