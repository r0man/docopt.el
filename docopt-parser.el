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

(defun docopt--parse-blank-line ()
  "Parse a blank line."
  (parsec-and (docopt--parse-spaces) (parsec-eol)))

(defun docopt--parse-optional-newline ()
  "Parse an optional newline."
  (parsec-optional (parsec-try (parsec-collect (docopt--parse-spaces) (parsec-eol)))))

(defun docopt--parse-section-header ()
  "Parse a Docopt section header."
  (parsec-query (parsec-re "^\\([[:alnum:]]+\\):") :group 1))

(defun docopt--parse-pipe ()
  "Parse a pipe."
  (parsec-re "\s*|\s*"))

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
  (let* ((case-fold-search nil)
         (name (parsec-return (parsec-re "[A-Z0-9][A-Z0-9_-]*")
                 (parsec-not-followed-by (parsec-re "[a-z0-9]"))) ))
    (docopt-make-argument :name name)))

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
  (parsec-and (parsec-re "\s*") (parsec-lookahead (parsec-ch ?-))))

(defun docopt--parse-option-line-separator ()
  "Parse an option line separator."
  (parsec-and (parsec-eol) (docopt--parse-option-line-begin)))

(defun docopt--parse-option-line-description ()
  "Parse an option line description."
  (s-trim (parsec-many-till-s
           (parsec-any-ch)
           (parsec-or (parsec-try (docopt--parse-option-line-separator))
                      (parsec-lookahead (parsec-try (docopt--parse-section-header)))
                      (parsec-eof)))))

(defun docopt--parse-option-line-option-separator ()
  "Parse the option separator of a Docopt option line."
  (parsec-or (parsec-re "\s*,\s*") (parsec-re "\s+")))

(defun docopt--parse-option-line-separated-options (option-1 option-2)
  "Parse OPTION-1 and OPTION-2 as separated options."
  (parsec-collect (funcall option-1)
                  (parsec-optional (parsec-try (parsec-and (docopt--parse-option-line-option-separator)
                                                           (funcall option-2))))))

(defun docopt--parse-option-line-long-short-options ()
  "Parse the options of a Docopt option line where long options come first."
  (docopt--parse-option-line-separated-options #'docopt--parse-long-option #'docopt--parse-short-option))

(defun docopt--parse-option-line-short-long-options ()
  "Parse the options of a Docopt option line where short options come first."
  (nreverse (docopt--parse-option-line-separated-options #'docopt--parse-short-option #'docopt--parse-long-option)))

(defun docopt--parse-option-line-options ()
  "Parse the options of a Docopt option line."
  (parsec-or (docopt--parse-option-line-long-short-options)
             (docopt--parse-option-line-short-long-options)))

(defun docopt--parse-option-line-set-option (option description default)
  "Set the :description and :default slots of the OPTION argument to DESCRIPTION and DEFAULT."
  (when option
    (oset option :description description)
    (when-let ((argument (docopt-option-argument option)))
      (oset argument :default default))))

(defun docopt--parse-option-line ()
  "Parse an option line."
  (seq-let [_ [long-option short-option] _ description]
      (parsec-collect
       (docopt--parse-whitespaces)
       (docopt--parse-option-line-options)
       (docopt--parse-whitespaces)
       (docopt--parse-option-line-description))
    (let ((default (docopt--parse-default description)))
      (docopt--parse-option-line-set-option long-option description default)
      (docopt--parse-option-line-set-option short-option description default)
      (make-instance
       'docopt-option-line
       :description description
       :long-option long-option
       :short-option short-option))))

(defun docopt--parse-option-lines ()
  "Parse an option lines."
  (parsec-many (docopt--parse-option-line)))

(defun docopt--parse-options ()
  "Parse the options."
  (parsec-and (docopt--parse-options-str)
              (docopt--parse-optional-newline)
              (docopt--parse-option-lines)))

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

(defun docopt--parse-usage-group (open close)
  "Parse an expression group between OPEN and CLOSE."
  (docopt--parse-group open close (docopt--parse-usage-expr)))

(defun docopt--parse-usage-group-optional ()
  "Parse a optional expression group."
  (docopt-set-optional (docopt--parse-usage-group ?\[ ?\]) t))

(defun docopt--parse-usage-group-required ()
  "Parse a required expression group."
  (docopt-set-optional (docopt--parse-usage-group ?\( ?\)) nil))

(defun docopt--parse-usage-atom ()
  "Parse an atom of a usage line expression."
  (docopt--parse-repeatable
   (parsec-or (docopt--parse-usage-group-optional)
              (docopt--parse-usage-group-required)
              (docopt--parse-long-option)
              (docopt--parse-short-options-stacked)
              (docopt--parse-short-option)
              (docopt--parse-argument)
              (docopt--parse-usage-command))))

(defun docopt--parse-usage-seq ()
  "Parse an expression sequence."
  (docopt--parse-sep-by1
   (parsec-return (docopt--parse-usage-atom)
     (parsec-optional (docopt--parse-spaces)))
   (docopt--parse-spaces)))

(defun docopt--parse-usage-expr ()
  "Parse an expression sequence."
  (parsec-sepby (docopt--parse-usage-seq) (docopt--parse-pipe)))

;; Usage Section

(defun docopt--parse-usage-command ()
  "Parse a command in a usage pattern."
  (docopt-make-command :name (docopt--parse-command-name)))

(defun docopt--parse-usage-header ()
  "Parse the \"Usage:\" header."
  (parsec-str "Usage:"))

(defun docopt--parse-usage-line ()
  "Parse a usage line."
  (apply #'append (parsec-and
                   (docopt--parse-space)
                   (docopt--parse-spaces)
                   (parsec-return (docopt--parse-usage-expr)
                     (parsec-optional (docopt--parse-newlines))))))

(defun docopt--parse-usage ()
  "Parse the Docopt usage patterns."
  (parsec-and
   (docopt--parse-usage-header)
   (docopt--parse-optional-newline)
   (parsec-many (docopt--parse-usage-line))))

;; Sentence

(defun docopt--parse-eof-sentence ()
  "Parse the end of a sentence."
  (parsec-or (parsec-ch ?\.)
             (parsec-ch ?\?)
             (parsec-ch ?\!)))

(defun docopt--parse-sentence ()
  "Parse a sentence."
  (parsec-many-till-s (parsec-any-ch) (docopt--parse-eof-sentence)))

;; Examples

(defun docopt--parse-examples-str ()
  "Parse the \"Examples:\" string."
  (parsec-str "Examples:"))

(defun docopt--split-line (line)
  "Trim and split the LINE."
  (let ((line (s-trim line)))
    (unless (s-blank-p line)
      (s-split "\s+" line ))))

(defun docopt--parse-example-line ()
  "Parse a Docopt example line."
  (docopt--split-line
   (parsec-many-till-s
    (parsec-any-ch)
    (parsec-lookahead
     (parsec-or (parsec-eol)
                (parsec-eof)
                (docopt--parse-section-header))))))

(defun docopt--parse-example-lines ()
  "Parse a Docopt example lines."
  (seq-remove #'null (parsec-sepby (docopt--parse-example-line) (parsec-eol))))

(defun docopt--parse-examples ()
  "Parse the Docopt examples."
  (parsec-optional
   (parsec-and
    (docopt--parse-examples-str)
    (docopt--parse-optional-newline)
    (docopt--parse-example-lines))))

;; Program

(defun docopt--parse-program-title ()
  "Parse a Docopt program title."
  (s-trim (parsec-many-till-s
           (parsec-any-ch)
           (parsec-or (docopt--parse-eof-sentence)
                      (parsec-newline)))))

(defun docopt--parse-program-description ()
  "Parse a Docopt program description."
  (let ((description (s-trim (parsec-until-s (parsec-lookahead (docopt--parse-section-header))))))
    (unless (s-blank-p description)
      description)))

(defun docopt--parse-program-sections (program)
  "Parse and set the Docopt sections for the PROGRAM."
  (parsec-many
   (parsec-or
    (oset program :usage (docopt--parse-usage))
    (oset program :options (docopt--parse-options))
    (oset program :examples (docopt--parse-examples)))))

(defun docopt--parse-program ()
  "Parse a Docopt program."
  (let ((program (docopt-make-program)))
    (seq-let [title description sections]
        (parsec-collect
         (docopt--parse-program-title)
         (docopt--parse-program-description)
         (docopt--parse-program-sections program))
      (oset program :description description)
      (oset program :title title)
      program)))

(provide 'docopt-parser)
