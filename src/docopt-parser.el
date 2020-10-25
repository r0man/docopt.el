;;; docopt-parser.el --- Docopt parser -*- lexical-binding: t -*-

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

;; The Docopt parser

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-either)
(require 'docopt-generic)
(require 'docopt-group)
(require 'docopt-option)
(require 'docopt-options-shortcut)
(require 'docopt-repeated)
(require 'docopt-standard-input)
(require 'docopt-usage-pattern)
(require 'docopt-util)
(require 'eieio)
(require 'parsec)
(require 's)
(require 'seq)
(require 'subr-x)

(defmacro docopt-parser--sep-end-by1 (parser sep)
  "Parse one or more occurrences of PARSER, separated and optionally ended by SEP."
  `(cons ,parser (parsec-return (parsec-many (parsec-try (parsec-and ,sep ,parser)))
                   (parsec-optional ,sep))))

(defmacro docopt-parser--sep-end-by (parser sep)
  "Parse zero or more occurrences of PARSER, separated and optionally ended by SEP."
  `(parsec-optional (docopt-parser--sep-end-by1 ,parser ,sep)))

(defmacro docopt-parser--sep-by1 (parser sep)
  "Parse one or more occurrences of PARSER, separated by SEP."
  `(cons ,parser (parsec-many (parsec-and ,sep ,parser))))

(defmacro docopt-parser--group (open close parser)
  "Parse a group between OPEN and CLOSE using PARSER."
  `(parsec-between
    (parsec-and (parsec-ch ,open) (docopt-parser-spaces))
    (parsec-and (docopt-parser-spaces) (parsec-ch ,close))
    ,parser))

(defun docopt-parser--blank-line ()
  "Parse a blank line."
  (parsec-and (parsec-eol) (docopt-parser-spaces) (parsec-eol)))

(defun docopt-parser--optional-newline ()
  "Parse an optional newline."
  (parsec-optional (parsec-try (parsec-collect (docopt-parser-spaces) (parsec-eol)))))

(defun docopt-parser--section-name ()
  "Parse a Docopt section name."
  (parsec-query (parsec-re "^\\([[:alnum:]]+\\):") :group 1))

(defun docopt-parser--pipe ()
  "Parse a pipe."
  (parsec-re "\s*|\s*"))

(defun docopt-parser--standard-input ()
  "Parse the Docopt standard input."
  (when (parsec-str "[-]") (docopt-standard-input)))

(defun docopt-parser-space ()
  "Parse a space character."
  (parsec-ch ?\s))

(defun docopt-parser-spaces ()
  "Parse many space characters."
  (parsec-many-s (docopt-parser-space)))

(defun docopt-parser-spaces1 ()
  "Parse 1 or more space characters."
  (parsec-many1-s (docopt-parser-space)))

(defun docopt-parser-whitespace ()
  "Parse a space character, newline or a CRLF."
  (parsec-or (docopt-parser-space) (parsec-eol)))

(defun docopt-parser-whitespaces ()
  "Parse spaces and newlines."
  (parsec-many-as-string (docopt-parser-whitespace)))

(defun docopt-parser--newlines ()
  "Parse newlines."
  (parsec-many (parsec-newline)))

(defun docopt-parser--default (s)
  "Parse the default value from S."
  (when s
    (when-let ((default (nth 1 (s-match "\\[default:\s*\\([^]]+\\)\s*\\]" s))))
      (let ((values (s-split "\s+" (s-trim default))))
        (if (= 1 (length values))
            (car values) (apply #'vector values))))))

(defun docopt-parser-command-name ()
  "Parse a command name."
  (parsec-or (parsec-re "[[:alnum:]][[:alnum:]_.-]*")
             (parsec-str "--")))

;; Argument

(defun docopt-parser--argument-spaceship ()
  "Parse a spaceship argument."
  (docopt-argument :name (parsec-between
                          (parsec-ch ?<) (parsec-ch ?>)
                          (parsec-re "[[:alnum:]][[:alnum:]-_:/ ]*"))))

(defun docopt-parser--argument-name (&optional case-insensitive)
  "Parse an argument name CASE-INSENSITIVE."
  (let* ((case-fold-search case-insensitive)
         (name (parsec-return (parsec-re "[A-Z0-9][A-Z0-9/_-]*")
                 (parsec-not-followed-by (parsec-re "[a-z0-9]")))))
    (docopt-argument :name name)))

(defun docopt-parser--argument (&optional case-insensitive)
  "Parse an argument CASE-INSENSITIVE."
  (parsec-or (docopt-parser--argument-spaceship)
             (docopt-parser--argument-name case-insensitive)))

;; Long Option

(defun docopt-parser--long-option-name ()
  "Parse a long option name."
  (substring (parsec-re "--[[:alnum:]-_]+") 2))

(defvar docopt-strict-long-options nil
  "Whether to parse long options in strict mode or not.
When t, only allow \"=\" as the long option separator, otherwise
\"=\" and \" \" are allowed.")

(defun docopt-parser-long-option-separator ()
  "Parse a long option separator."
  (if docopt-strict-long-options
      (parsec-ch ?=)
    (parsec-or (parsec-ch ?=) (parsec-ch ?\s))))

(defun docopt-parser--long-option ()
  "Parse a long option."
  (seq-let [name argument]
      (parsec-collect
       (docopt-parser--long-option-name)
       (parsec-optional
        (parsec-try
         (parsec-and (docopt-parser-long-option-separator)
                     (docopt-parser--argument t)))))
    (docopt-long-option :name name :argument argument)))

;; Short Option

(defun docopt-parser--short-option-name ()
  "Parse a short option name."
  (substring (parsec-re "-[[:alnum:]]") 1))

(defun docopt-parser-short-option-separator ()
  "Parse a short option separator."
  (parsec-or (parsec-ch ?=) (docopt-parser-whitespace)))

(defun docopt-parser--short-option ()
  "Parse a short option."
  (parsec-with-error-message
      (format "Short option parse error: %s" (cdr parsec-last-error-message))
    (seq-let [name argument]
        (parsec-collect
         (docopt-parser--short-option-name)
         (parsec-optional
          (parsec-try
           (parsec-and
            (parsec-optional (docopt-parser-short-option-separator))
            (docopt-parser--argument)))))
      (docopt-short-option :name name :argument argument))))

(defun docopt-parser--short-options-stacked ()
  "Parse stacked short options."
  (seq-map (lambda (short-char)
             (docopt-short-option :name (char-to-string short-char)))
           (substring (parsec-re "-[[:alnum:]][[:alnum:]]+") 1)))

;; Options

(defun docopt-parser--options-str ()
  "Return the \"Options:\" parser."
  (parsec-re "[^\n]*Options:"))

(defun docopt-parser--option ()
  "Parse a long or short option."
  (parsec-or (docopt-parser--long-option)
             (docopt-parser--short-option)))

;; Option Line

(defun docopt-parser--option-line-begin ()
  "Parse the beginning of an option line."
  (parsec-and (parsec-re "\s*") (parsec-lookahead (parsec-ch ?-))))

(defun docopt-parser--option-line-separator ()
  "Parse an option line separator."
  (parsec-and (parsec-eol) (parsec-lookahead (docopt-parser--option-line-begin))))

(defun docopt-parser--option-line-description ()
  "Parse an option line description."
  (docopt-strip (parsec-many-till-s
                 (parsec-any-ch)
                 (parsec-or (parsec-try (docopt-parser--blank-line))
                            (parsec-try (docopt-parser--option-line-separator))
                            (parsec-lookahead
                             (parsec-try
                              (parsec-or
                               (docopt-parser--section-header)
                               (docopt-parser--options-str))))
                            (parsec-eof)))))

(defun docopt-parser--option-line-option-separator ()
  "Parse the option separator of a Docopt option line."
  (parsec-or (parsec-re "\s*,\s*") (parsec-re "\s+")))

(defun docopt-parser--option-line-separated-options (option-1 option-2)
  "Parse OPTION-1 and OPTION-2 as separated options."
  (parsec-collect (funcall option-1)
                  (parsec-optional (parsec-try (parsec-and (docopt-parser--option-line-option-separator)
                                                           (funcall option-2))))))

(defun docopt-parser--option-line-long-short-options ()
  "Parse the options of a Docopt option line where long options come first."
  (docopt-parser--option-line-separated-options
   #'docopt-parser--long-option
   #'docopt-parser--short-option))

(defun docopt-parser--option-line-short-long-options ()
  "Parse the options of a Docopt option line where short options come first."
  (nreverse (docopt-parser--option-line-separated-options
             #'docopt-parser--short-option
             #'docopt-parser--long-option)))

(defun docopt-parser--option-line-options ()
  "Parse the options of a Docopt option line."
  (parsec-or (docopt-parser--option-line-long-short-options)
             (docopt-parser--option-line-short-long-options)))

(defun docopt-parser--option-line ()
  "Parse an option line."
  (seq-let [_ [long-option short-option] _ description]
      (parsec-try
       (parsec-collect
        (docopt-parser-spaces1)
        (docopt-parser--option-line-options)
        (docopt-parser-spaces)
        (docopt-parser--option-line-description)))
    (let ((default (docopt-parser--default description)))
      (seq-remove #'null (docopt-option-link long-option short-option description default)))))

(defun docopt-parser--option-lines ()
  "Parse an option lines."
  (apply #'append (parsec-many (docopt-parser--option-line))))

(defun docopt-parser--options ()
  "Parse the options."
  (apply #'append
         (parsec-many1
          (parsec-and
           (docopt-parser--options-str)
           (docopt-parser--optional-newline)
           (docopt-parser--option-lines)))))

;; Repeatable

(defun docopt-parser--ellipsis ()
  "Parse the repeatable identifier."
  (parsec-re "\s*\\.\\.\\."))

(defmacro docopt-parser--repeatable (parser)
  "Parse a repeatable expression with PARSER."
  (let ((object (make-symbol "object"))
        (ellipsis (make-symbol "ellipsis")))
    `(seq-let [,object ,ellipsis]
         (parsec-collect ,parser (parsec-optional (docopt-parser--ellipsis)))
       (if ,ellipsis
           (docopt-make-repeated ,object)
         ,object))))

;; Usage Expression

(defun docopt-parser--usage-group (state open close)
  "Parse an expression group between OPEN and CLOSE using STATE."
  (-flatten (docopt-parser--group open close (docopt-parser--usage-expr state))))

(defun docopt-parser--usage-group-optional (state)
  "Parse a optional expression group using STATE."
  (apply #'docopt-make-optional-group (docopt-parser--usage-group state ?\[ ?\])))

(defun docopt-parser--usage-group-required (state)
  "Parse a required expression group using STATE."
  (apply #'docopt-make-required-group (docopt-parser--usage-group state ?\( ?\))))

(defun  docopt-parser--options-shortcut ()
  "Parse the Docopt options shortcut."
  (when (parsec-str "[options]")
    (make-instance 'docopt-options-shortcut)))

(defun docopt-parser--usage-option-known-p (name known-options)
  "Return t if the option with NAME is in KNOWN-OPTIONS, otherwise nil."
  (not (null (cl-find name known-options :key #'docopt-option-name :test #'string=))))

(defun docopt-parser--usage-option-known-p (name known-options)
  "Return t if the option with NAME is in KNOWN-OPTIONS, otherwise nil."
  (let ((option (cl-find name known-options :key #'docopt-option-name :test #'string=)))
    (and option (not (null (docopt-option-argument option))))))

(defun docopt-parser--usage-long-option (&optional known-options)
  "Parse a long option in a usage line using KNOWN-OPTIONS."
  (parsec-with-error-message
      (format "Long option usage parse error: %s" (cdr parsec-last-error-message))
    (let ((name (docopt-parser--long-option-name)))
      (if (docopt-parser--usage-option-known-p name known-options)
          (let ((argument (parsec-and (parsec-or (parsec-ch ?=) (parsec-ch ?\s))
                                      (docopt-parser--argument t))))
            (docopt-long-option :name name :argument argument))
        (let ((argument (parsec-optional
                         (parsec-try
                          (parsec-and (parsec-ch ?=)
                                      (docopt-parser--argument t))))))
          (docopt-long-option :name name :argument argument))))))

(defun docopt-parser--usage-short-option (&optional known-options)
  "Parse a short option in a usage line using KNOWN-OPTIONS."
  (parsec-with-error-message
      (format "Long option usage parse error: %s" (cdr parsec-last-error-message))
    (let ((name (parsec-lookahead (docopt-parser--short-option-name))))
      (if (docopt-parser--usage-option-known-p name known-options)
          (let ((name (docopt-parser--short-option-name))
                (argument (parsec-and (parsec-optional (parsec-or (parsec-ch ?=) (docopt-parser-whitespace)))
                                      (docopt-parser--argument t))))
            (docopt-short-option :name name :argument argument))
        (let ((name (docopt-parser--short-option-name))
              (argument (parsec-optional
                         (parsec-try
                          (parsec-and (parsec-ch ?=)
                                      (docopt-parser--argument t))))))
          (docopt-short-option :name name :argument argument))))))

(defun docopt-parser--usage-atom (state)
  "Parse an atom of an usage line expression using STATE."
  (docopt-parser--repeatable
   (parsec-or
    (docopt-parser--standard-input)
    (docopt-parser--options-shortcut)
    (docopt-parser--usage-group-optional state)
    (docopt-parser--usage-group-required state)
    (docopt-parser--long-option)
    (parsec-try
     (parsec-return (docopt-parser--short-option)
       (parsec-lookahead
        (parsec-or (docopt-parser-whitespace)
                   (parsec-str "|")
                   (parsec-str "]")
                   (parsec-str ")")
                   (parsec-eof)))))
    (docopt-parser--short-options-stacked)
    (docopt-parser--argument)
    (docopt-parser--usage-command))))

(defun docopt-parser--usage-atom (state)
  "Parse an atom of an usage line expression using STATE."
  (let ((known-options (cadr (assoc 'options state))))
    (setq my-options known-options)
    (docopt-parser--repeatable
     (parsec-or
      (docopt-parser--standard-input)
      (docopt-parser--options-shortcut)
      (docopt-parser--usage-group-optional state)
      (docopt-parser--usage-group-required state)
      (docopt-parser--usage-long-option known-options)
      (parsec-try
       (parsec-return (docopt-parser--usage-short-option known-options)
         (parsec-lookahead
          (parsec-or (docopt-parser-whitespace)
                     (parsec-str "|")
                     (parsec-str "]")
                     (parsec-str ")")
                     (parsec-eof)))))
      (docopt-parser--short-options-stacked)
      (docopt-parser--argument)
      (docopt-parser--usage-command)))))

(defun docopt-parser--usage-seq (state)
  "Parse an expression sequence using STATE."
  (docopt-parser--sep-by1
   (parsec-return (docopt-parser--usage-atom state)
     (parsec-optional (docopt-parser-spaces)))
   (docopt-parser-spaces)))

(defun docopt-parser--usage-expr (state)
  "Parse an expression sequence."
  (let ((result (parsec-sepby (docopt-parser--usage-seq state) (docopt-parser--pipe))))
    (cond
     ((and (= 1 (length result))
           (sequencep (car result))
           (= 1 (length (car result))))
      (car result))
     ((< 1 (length result))
      (list (apply #'docopt-make-either result)))
     (t result))))

;; Usage Section

(defun docopt-parser--usage-command ()
  "Parse a command in a usage pattern."
  (docopt-command :name (docopt-parser-command-name)))

(defun docopt-parser--usage-header ()
  "Parse the \"Usage:\" header."
  (parsec-str "Usage:"))

(defun docopt-parser--usage-line (state &optional first-line)
  "Parse a usage line using STATE, if FIRST-LINE is t the line must not start with a space."
  (let ((docopt-strict-long-options t))
    (seq-let [command exprs]
        (parsec-and
         (if first-line
             (docopt-parser-spaces)
           (docopt-parser-spaces1))
         (parsec-return
             (parsec-collect
              (docopt-parser--usage-command)
              (parsec-optional
               (parsec-and
                (docopt-parser-spaces1)
                (docopt-parser--usage-expr state))))
           (parsec-optional (docopt-parser--newlines))))
      (apply #'docopt-make-usage-pattern command (-flatten exprs)))))

(defun docopt-parser--usage-lines (state)
  "Parse Docopt usage lines using STATE."
  (seq-let [first rest]
      (parsec-collect
       (docopt-parser--usage-line state t)
       (parsec-many (docopt-parser--usage-line state)))
    (cons first rest)))

(defun docopt-parser--usage (state)
  "Parse the Docopt usage patterns using STATE."
  (parsec-and
   (docopt-parser--usage-header)
   (docopt-parser--optional-newline)
   (docopt-parser--usage-lines state)))

;; Sentence

(defun docopt-parser--eof-sentence ()
  "Parse the end of a sentence."
  (parsec-or (parsec-ch ?\.)
             (parsec-ch ?\?)
             (parsec-ch ?\!)))

(defun docopt-parser--sentence ()
  "Parse a sentence."
  (parsec-many-till-s (parsec-any-ch) (docopt-parser--eof-sentence)))

;; Examples

(defun docopt-parser--examples-str ()
  "Parse the \"Examples:\" string."
  (parsec-re "\s*Examples:"))

(defun docopt--split-line (line)
  "Trim and split the LINE."
  (when-let ((line (docopt-strip line)))
    (s-split "\s+" line )))

(defun docopt-parser--example-line ()
  "Parse a Docopt example line."
  (docopt--split-line
   (parsec-many-till-s
    (parsec-any-ch)
    (parsec-lookahead
     (parsec-or (parsec-eol)
                (parsec-eof)
                (docopt-parser--section-header))))))

(defun docopt-parser--example-lines ()
  "Parse a Docopt example lines."
  (seq-remove #'null (parsec-sepby (docopt-parser--example-line) (parsec-eol))))

(defun docopt-parser--examples ()
  "Parse the Docopt examples."
  (parsec-and (docopt-parser--examples-str)
              (docopt-parser--optional-newline)
              (docopt-parser--example-lines)))

;; Program

(defun docopt-parser--section-header ()
  "Parse a Docopt section header."
  (parsec-return (docopt-parser--section-name)
    (parsec-newline)))

(defun docopt-parser--program-header ()
  "Parse the Docopt PROGRAM header section."
  (when-let (header (docopt-strip (parsec-until-s (parsec-lookahead (parsec-re "usage:[\n\s+]?")))))
    (list :header header)))

(defun docopt-parser--raw-section-seperator ()
  "Parse the section seperator of a Docopt program."
  (parsec-and (parsec-re "\n") (parsec-lookahead (parsec-re "[[:alnum:]]"))))

(defun docopt-parser--section-keyword (s)
  "Convert S into a section keyword, normalizing options."
  (if (s-matches-p ".*options" s)
      :options
    (docopt-keyword s)))

(defun docopt-parser--raw-section ()
  "Parse a section of a Docopt program."
  (when-let ((section (parsec-until-s (parsec-or (parsec-eob) (parsec-lookahead (parsec-re "\n[[:alnum:]]"))))))
    (let ((name (nth 1 (s-match "\\([^:]+\\):[\s\n]*" section))))
      (if (s-blank-p name)
          (list :unknown section)
        (list (docopt-parser--section-keyword name) (docopt-strip section))))))

(defun docopt-parser--raw-sections ()
  "Parse the sections of a Docopt program."
  (seq-let [header sections]
      (parsec-collect (parsec-optional (parsec-try (docopt-parser--program-header)))
                      (parsec-sepby (docopt-parser--raw-section) (docopt-parser--raw-section-seperator)))
    (seq-remove #'null (cons header sections))))

(defun docopt-parser--program-examples ()
  "Parse and set the Docopt PROGRAM examples."
  (list :examples (docopt-parser--examples)))

(defun docopt-parser--program-footer ()
  "Parse and set the Docopt PROGRAM footer."
  (list :footer (parsec-until-s
                 (parsec-or
                  (parsec-eob)
                  (parsec-eof)))))

(defun docopt-parser--program-options ()
  "Parse and set the Docopt PROGRAM options."
  (list :options (docopt-parser--options)))

(defun docopt-parser--program-usage ()
  "Parse and set the Docopt PROGRAM usage."
  (list :usage (docopt-parser--usage)))

(defun docopt-parser--keyword (s)
  "Convert S to a keyword."
  (list :usage (docopt-parser--usage)))

(defun docopt-parser--find-raw-section (name sections)
  "Find the raw section by NAME in SECTIONS."
  (cl-find name sections :key #'car :test #'equal))

(cl-defgeneric docopt-parser--section (program name section)
  "Parse the SECTION of the Docopt PROGRAM dispatching on NAME.")

(cl-defmethod docopt-parser--section (program (name (eql :examples)) section)
  "Parse the examples SECTION of the Docopt PROGRAM dispatching on NAME."
  (with-slots (examples) program
    (setq examples (docopt-with-parse-input section (docopt-parser--examples)))))

(cl-defmethod docopt-parser--section (program (name (eql :header)) section)
  "Parse the header SECTION of the Docopt PROGRAM dispatching on NAME."
  (with-slots (header) program
    (setq header (docopt-strip section))))

(cl-defmethod docopt-parser--section (program (name (eql :options)) section)
  "Parse the options SECTION of the Docopt PROGRAM dispatching on NAME."
  (with-slots (options) program
    (let ((section-options (docopt-with-parse-input section (docopt-parser--options))))
      (setq options (docopt-options-merge options section-options)))))

(cl-defmethod docopt-parser--section (program (name (eql :usage)) section)
  "Parse the usage SECTION of the Docopt PROGRAM dispatching on NAME."
  (with-slots (usage options) program
    (let ((state (list (list 'options options))))
      (setq usage (docopt-with-parse-input section (docopt-parser--usage state))))))

(cl-defmethod docopt-parser--section (program name section)
  "Parse the usage SECTION of the Docopt PROGRAM dispatching on NAME." nil)

(provide 'docopt-parser)

;;; docopt-parser.el ends here
