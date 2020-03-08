;;; docopt-parser.el --- The Docopt parser -*- lexical-binding: t -*-

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

;; The Docopt parser

;;; Code:

(require 'cl-lib)
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
(require 'eieio)
(require 'parsec)
(require 's)
(require 'seq)
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
  `(parsec-between
    (parsec-and (parsec-ch ,open) (docopt--parse-spaces))
    (parsec-and (docopt--parse-spaces) (parsec-ch ,close))
    ,parser))

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

(defun docopt--parse-standard-input ()
  "Parse the Docopt standard input."
  (when (parsec-str "[-]") (docopt-standard-input)))

(defun docopt--parse-space ()
  "Parse a space character."
  (parsec-ch ?\s))

(defun docopt--parse-spaces ()
  "Parse many space characters."
  (parsec-many-s (docopt--parse-space)))

(defun docopt--parse-spaces1 ()
  "Parse 1 or more space characters."
  (parsec-many1-s (docopt--parse-space)))

(defun docopt--parse-whitespace ()
  "Parse a space character, newline or a CRLF."
  (parsec-or (docopt--parse-space) (parsec-eol)))

(defun docopt--parse-whitespaces ()
  "Parse spaces and newlines."
  (parsec-many-as-string (docopt--parse-whitespace)))

(defun docopt--parse-newlines ()
  "Parse newlines."
  (parsec-many (parsec-newline)))

(defun docopt--parse-default (s)
  "Parse the default value from S."
  (when s (nth 1 (s-match "\\[default:\s*\\([^] ]+\\)\s*\\]" s))))

(defun docopt--parse-command-name ()
  "Parse a command name."
  (parsec-re "[[:alnum:]][[:alnum:]-_]*"))

;; Argument

(defun docopt--parse-argument-spaceship ()
  "Parse a spaceship argument."
  (docopt-argument :object-name (parsec-between
                                 (parsec-ch ?<) (parsec-ch ?>)
                                 (parsec-re "[[:alnum:]][[:alnum:]-_:/ ]*"))))

(defun docopt--parse-argument-name (&optional case-insensitive)
  "Parse an argument name CASE-INSENSITIVE."
  (let* ((case-fold-search case-insensitive)
         (name (parsec-return (parsec-re "[A-Z0-9][A-Z0-9/_-]*")
                 (parsec-not-followed-by (parsec-re "[a-z0-9]")))))
    (docopt-argument :object-name name)))

(defun docopt--parse-argument (&optional case-insensitive)
  "Parse an argument CASE-INSENSITIVE."
  (parsec-or (docopt--parse-argument-spaceship)
             (docopt--parse-argument-name case-insensitive)))

;; Long Option

(defun docopt--parse-long-option-name ()
  "Parse a long option name."
  (substring (parsec-re "--[[:alnum:]-_]+") 2))

(defvar docopt-strict-long-options nil
  "Whether to parse long options in strict mode or not.
When t, only allow \"=\" as the long option separator, otherwise
\"=\" and \" \" are allowed.")

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
                     (docopt--parse-argument t)))))
    (docopt-long-option :object-name name :argument argument)))

;; Short Option

(defun docopt--parse-short-option-name ()
  "Parse a short option name."
  (substring (parsec-re "-[[:alnum:]]") 1))

(defun docopt--parse-short-option-separator ()
  "Parse a short option separator."
  (parsec-or (parsec-ch ?=) (docopt--parse-whitespace)))

(defun docopt--parse-short-option ()
  "Parse a short option."
  (parsec-with-error-message
      (format "Short option parse error: %s" (cdr parsec-last-error-message))
    (seq-let [name argument]
        (parsec-collect
         (docopt--parse-short-option-name)
         (parsec-optional
          (parsec-try
           (parsec-and
            (parsec-optional (docopt--parse-short-option-separator))
            (docopt--parse-argument)))))
      (docopt-short-option :object-name name :argument argument))))

(defun docopt--parse-short-options-stacked ()
  "Parse stacked short options."
  (seq-map (lambda (short-char)
             (docopt-short-option :object-name (char-to-string short-char)))
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
  (parsec-and (parsec-eol) (parsec-lookahead (docopt--parse-option-line-begin))))

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

(defun docopt--parse-option-line ()
  "Parse an option line."
  (seq-let [_ [long-option short-option] _ description]
      (parsec-collect
       (docopt--parse-whitespaces)
       (docopt--parse-option-line-options)
       (docopt--parse-spaces)
       (docopt--parse-option-line-description))
    (let ((default (docopt--parse-default description)))
      (seq-remove #'null (docopt-option-link long-option short-option description default)))))

(defun docopt--parse-option-lines ()
  "Parse an option lines."
  (apply #'append (parsec-many (docopt--parse-option-line))))

(defun docopt--parse-options ()
  "Parse the options."
  (parsec-and (docopt--parse-options-str)
              (docopt--parse-optional-newline)
              (docopt--parse-option-lines)))

;; Repeatable

(defun docopt--parse-ellipsis ()
  "Parse the repeatable identifier."
  (parsec-re "\s*\\.\\.\\."))

(defmacro docopt--parse-repeatable (parser)
  "Parse a repeatable expression with PARSER."
  (let ((object (make-symbol "object"))
        (ellipsis (make-symbol "ellipsis")))
    `(seq-let [,object ,ellipsis]
         (parsec-collect ,parser (parsec-optional (docopt--parse-ellipsis)))
       (if ,ellipsis
           (docopt-make-repeated ,object)
         ,object))))

;; Usage Expression

(defun docopt--parse-usage-group (open close)
  "Parse an expression group between OPEN and CLOSE."
  (docopt--flatten (docopt--parse-group open close (docopt--parse-usage-expr))))

(defun docopt--parse-usage-group-optional ()
  "Parse a optional expression group."
  (apply #'docopt-make-optional-group (docopt--parse-usage-group ?\[ ?\])))

(defun docopt--parse-usage-group-required ()
  "Parse a required expression group."
  (apply #'docopt-make-required-group (docopt--parse-usage-group ?\( ?\))))

(defun docopt--parse-options-shortcut ()
  "Parse the Docopt options shortcut."
  (when (parsec-str "[options]")
    (make-instance 'docopt-options-shortcut)))

(defun docopt--parse-usage-atom ()
  "Parse an atom of a usage line expression."
  (docopt--parse-repeatable
   (parsec-or (docopt--parse-standard-input)
              (docopt--parse-options-shortcut)
              (docopt--parse-usage-group-optional)
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
  (let ((result (parsec-sepby (docopt--parse-usage-seq) (docopt--parse-pipe))))
    (cond
     ((and (= 1 (length result))
           (sequencep (car result))
           (= 1 (length (car result))))
      (car result))
     ((< 1 (length result))
      (list (apply #'docopt-make-either result)))
     (t result))))

;; Usage Section

(defun docopt--parse-usage-command ()
  "Parse a command in a usage pattern."
  (docopt-command :object-name (docopt--parse-command-name)))

(defun docopt--parse-usage-header ()
  "Parse the \"Usage:\" header."
  (parsec-str "Usage:"))

(defun docopt--parse-usage-line ()
  "Parse a usage line."
  (seq-let [command exprs]
      (parsec-and
       (docopt--parse-spaces1)
       (parsec-collect
        (docopt--parse-usage-command)
        (parsec-optional
         (parsec-and
          (docopt--parse-spaces1)
          (parsec-return (docopt--parse-usage-expr)
            (parsec-optional (docopt--parse-newlines)))))))
    (apply #'docopt-make-usage-pattern command (docopt--flatten exprs))))

(defun docopt--parse-usage-lines ()
  "Parse Docopt usage lines."
  (parsec-many (docopt--parse-usage-line)))

(defun docopt--parse-usage ()
  "Parse the Docopt usage patterns."
  (parsec-and
   (docopt--parse-usage-header)
   (docopt--parse-optional-newline)
   (docopt--parse-usage-lines)))

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
  (parsec-and (docopt--parse-examples-str)
              (docopt--parse-optional-newline)
              (docopt--parse-example-lines)))

;; Program

(defun docopt--parse-program-header ()
  "Parse and set the Docopt PROGRAM header."
  (let ((header (s-trim (parsec-until-s (parsec-lookahead (parsec-re "\\([[:alnum:]]+\\):"))))))
    (unless (s-blank-p header) (list :header header))))

(defun docopt--parse-program-examples ()
  "Parse and set the Docopt PROGRAM examples."
  (list :examples (docopt--parse-examples)))

(defun docopt--parse-program-footer ()
  "Parse and set the Docopt PROGRAM footer."
  (list :footer (parsec-until-s
                 (parsec-or
                  (parsec-eof)
                  (parsec-lookahead (docopt--parse-section-header))))))

(defun docopt--parse-program-options ()
  "Parse and set the Docopt PROGRAM options."
  (list :options (docopt--parse-options)))

(defun docopt--parse-program-usage ()
  "Parse and set the Docopt PROGRAM usage."
  (list :usage (docopt--parse-usage)))

(defun docopt--parse-program-sections ()
  "Parse and set the Docopt sections for the PROGRAM."
  (seq-remove #'null (cons (docopt--parse-program-header)
                           (parsec-many (parsec-or (docopt--parse-program-usage)
                                                   (docopt--parse-program-options)
                                                   (docopt--parse-program-examples)
                                                   (docopt--parse-program-footer))))))

(defun docopt--parse-program ()
  "Parse a Docopt program."
  (let ((program (docopt-program)))
    (docopt-program-set-sections program (docopt--parse-program-sections))
    (let ((program (docopt-program-remove-unknown-options program)))
      (with-slots (arguments options usage) program
        (docopt-set-shortcut-options program options)
        (setq arguments (docopt-remove-duplicates (docopt-collect-arguments program)))
        (setq options (docopt-options-merge (docopt-remove-duplicates (docopt-collect-options usage)) options))
        (seq-doseq (option options)
          (when (docopt-long-option-p option)
            (oset option :prefixes (docopt-option-prefixes option options))))
        program))))

(provide 'docopt-parser)

;;; docopt-parser.el ends here
