;;; docopt-parser.el --- The Docopt parser -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The Docopt parser

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'parsec)
(require 's)
(require 'seq)
(require 'docopt-classes)
(require 'subr-x)

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

(defmacro docopt--parse-repeatable (parser)
  "Parse a repeatable expression with PARSER."
  (let ((object (make-symbol "object"))
        (ellipsis (make-symbol "ellipsis")))
    `(seq-let [,object ,ellipsis]
         (parsec-collect ,parser (parsec-optional (docopt--parse-ellipsis)))
       (when ,ellipsis
         (docopt-set-repeatable ,object t))
       ,object)))

;; Usage Expression

(defun docopt--parse-usage-expr-group (open close)
  "Parse an expression group between OPEN and CLOSE."
  (docopt--parse-group open close (docopt--parse-usage-expr-seq)))

(defun docopt--parse-optional-group ()
  "Parse a optional expression group."
  (docopt-set-optional (docopt--parse-usage-expr-group ?\[ ?\]) t))

(defun docopt--parse-required-group ()
  "Parse a required expression group."
  (docopt-set-optional (docopt--parse-usage-expr-group ?\( ?\)) nil))

(defun docopt--parse-usage-expr-atom ()
  "Parse an atom of a usage expression."
  (list (parsec-or (docopt--parse-option)
                   (docopt--parse-argument)
                   (docopt--parse-usage-command))))

(defun docopt--parse-either-end ()
  "Parse the end of an either expression."
  (apply #'append (parsec-and (parsec-re "\s*|\s*") (docopt--parse-usage-expr-seq))))

(defun docopt--parse-either-start (expr)
  "Parse the start of an either expression or return EXPR."
  (if-let ((sep (parsec-peek-p (parsec-re "\s*|\s*"))))
      (list (apply #'docopt-make-either (seq-concatenate 'list expr (docopt--parse-either-end))))
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
  (apply #'append (parsec-and (docopt--parse-spaces) (docopt--parse-usage-expr-seq))))

(defun docopt--parse-usage-patterns ()
  "Parse the usage patterns."
  (parsec-and (docopt--parse-usage-header)
              (parsec-sepby (docopt--parse-usage-line) (parsec-eol))))

;; Program

(defun docopt--parse-program-description ()
  "Parse a Docopt program."
  (s-trim (parsec-many-till-s (parsec-any-ch) (docopt--parse-usage-header))))

(defun docopt--parse-program ()
  "Parse a Docopt program."
  (seq-let [description]
      (parsec-collect (docopt--parse-program-description))
    (docopt-make-program :description description)))

(provide 'docopt-parser)

;;; docopt-parser.el ends here
