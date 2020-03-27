;;; docopt.el --- A Docopt implementation in Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, tools, processes
;; Homepage: https://github.com/r0man/docopt.el
;; Version: 0.1.0

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

;; A Docopt implementation in Elisp

;;; Code:

(require 'dash)
(require 'eieio)
(require 's)
(require 'seq)

(defclass docopt-pattern () ()
  "A class representing a Docopt pattern.")

(defclass docopt-branch-pattern (docopt-pattern)
  ((children
    :accessor docopt-children
    :documentation "The children of the pattern."
    :initarg :children
    :initform nil
    :type (or list null)))
  "A class representing a Docopt branch pattern.")

(defclass docopt-leaf-pattern (docopt-pattern) ()
  "A class representing a Docopt leaf pattern.")

(defclass docopt-argument (docopt-leaf-pattern)
  ((name
    :accessor docopt-argument-name
    :documentation "The name of the argument."
    :initarg :name
    :initform nil
    :type (or string null)))
  "A class representing a Docopt argument.")

(defclass docopt-command (docopt-argument)
  ((name
    :accessor docopt-comand-name
    :documentation "The name of the command."
    :initarg :name
    :initform nil
    :type (or string null)))
  "A class representing a Docopt command.")

(defclass docopt-either (docopt-branch-pattern) ()
  "A class representing a Docopt either.")

(defclass docopt-one-or-more (docopt-branch-pattern) ()
  "A class representing one or more Docopt elements.")

(defclass docopt-option (docopt-leaf-pattern)
  ((arg-count
    :accessor docopt-option-arg-count
    :documentation "The number of argument of the option."
    :initarg :arg-count
    :initform nil
    :type (or number null))
   (description
    :accessor docopt-option-description
    :documentation "The description name of the option."
    :initarg :description
    :initform nil
    :type (or string null))
   (long
    :accessor docopt-option-long
    :documentation "The long name of the option."
    :initarg :long
    :initform nil
    :type (or string null))
   (short
    :accessor docopt-option-short
    :documentation "The short name of the option."
    :initarg :short
    :initform nil
    :type (or string null))
   (value
    :accessor docopt-option-value
    :documentation "The value of the option."
    :initarg :value
    :initform nil
    :type (or string t null)))
  "A class representing a Docopt option.")

(defclass docopt-optional (docopt-branch-pattern) ()
  "A class representing a Docopt optional element.")

(defclass docopt-options-shortcut (docopt-optional) ()
  "A class representing a Docopt options shortcut.")

(defclass docopt-required (docopt-branch-pattern) ()
  "A class representing a Docopt required element.")

(defclass docopt-program ()
  ((options
    :accessor docopt-program-options
    :documentation "The options of the program."
    :initarg :options
    :initform nil
    :type (or list null))
   (patterns
    :accessor docopt-program-patterns
    :documentation "The patterns of the program."
    :initarg :patterns
    :initform nil
    ;; :type (or list null)
    )
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
  "A class representing a Docopt program.")

(defclass docopt-tokens ()
  ((error
    :accessor docopt-tokens-error
    :documentation "The current error of the tokens."
    :initarg :error
    :initform 'docopt-exit
    :type symbol)
   (list
    :accessor docopt-tokens-list
    :documentation "The tokens as a list of strings."
    :initarg :list
    :initform nil
    :type (or list null)))
  "A class representing Docopt tokens.")

(defun docopt-tokens-argument-p (tokens)
  "Return t if the current token in TOKENS is an argument."
  (when-let ((token (docopt-tokens-current tokens)))
    (or (and (s-starts-with-p "<" token)
             (s-ends-with-p ">" token))
        (s-uppercase-p token))))

(defun docopt-tokens-current (tokens)
  "Return the current token from TOKENS."
  (car (docopt-tokens-list tokens)))

(defun docopt-tokens-current-p (tokens token)
  "Return t if the current token in TOKENS is equal to TOKEN."
  (equal token (docopt-tokens-current tokens)))

(defun docopt-tokens-from-pattern (source)
  "Parse SOURCE and return Docopt tokens."
  (thread-last source
    (s-replace-regexp "\\(\\[\\|\\]\\|(\\|)\\|\|\\|\\.\\.\\.\\)" " \\1 ")
    (s-split "\s+\\|([^ \t\r\n\v\f]*<.*?>)")
    (seq-remove #'s-blank-str-p)
    (docopt-tokens :list)))

(defun docopt-tokens-optional-group-p (tokens)
  "Return t if the current token in TOKENS is a group."
  (equal "[" (docopt-tokens-current tokens)))

(defun docopt-tokens-required-group-p (tokens)
  "Return t if the current token in TOKENS is a group."
  (equal "(" (docopt-tokens-current tokens)))

(defun docopt-tokens-long-option-p (tokens)
  "Return t if the current token in TOKENS is a long option."
  (when-let ((token (docopt-tokens-current tokens)))
    (and (s-starts-with-p "--" token)
         (not (string= "--" token)))))

(defun docopt-tokens-move (tokens)
  "Remove a token from TOKENS."
  (pop (docopt-tokens-list tokens)))

(defun docopt-tokens-short-option-p (tokens)
  "Return t if the current token in TOKENS is a short option."
  (when-let ((token (docopt-tokens-current tokens)))
    (and (s-starts-with-p "-" token)
         (not (member token '("-" "--"))))))

(defun docopt-tokens-options-shortcut-p (tokens)
  "Return t if the current token in TOKENS is the options shortcut."
  (equal "options" (docopt-tokens-current tokens)))

(defun docopt--error (error-sym fmt &rest args)
  "Raise a Docopt error with ERROR-SYM and format FMT using ARGS."
  (signal error-sym (apply #'format fmt args)))

(defun docopt--split (s)
  "Trim and split the string S by whitespace."
  (when s (s-split "[\s\t\r\n\v\f]+" (s-trim s))))

(defun docopt--formal-usage (section)
  "Parse the Docopt formal usage from SECTION."
  (when-let ((s (cadr (s-split ":" section))))
    (let ((split (docopt--split s)))
      (thread-last (-split-when (lambda (s) (string= (car split) s)) split)
        (seq-map (lambda (s) (concat "( " (s-join " " s) " )")))
        (s-join " | ")))))

(defun docopt--parse-section (name source)
  "Parse all Docopt sections with NAME from SOURCE."
  (let ((pattern (concat "^\\([^\n]*" name "[^\n]*\n?\\(?:[ \t].*?\\(?:\n\\|$\\)\\)*\\)")))
    (thread-last (s-match-strings-all pattern source)
      (seq-mapcat #'cdr)
      (seq-map #'s-trim))))

(defun docopt--parse-defaults (source)
  "Parse the default Docopt options from SOURCE."
  (thread-last (docopt--parse-section "options:" source)
    (seq-mapcat (lambda (section)
                  (thread-last section
                    (s-replace "options:" "")
                    (concat "\n")
                    (s-slice-at "\n[ \t]*\\(-[a-z-]+\\)")
                    (seq-remove #'s-blank-p))))
    (seq-map #'docopt--parse-option)
    (seq-remove #'null)))

(defun docopt--parse-long (tokens options)
  "Parse a Docopt long option from TOKENS with OPTIONS."
  (let ((token (docopt-tokens-move tokens)))
    (seq-let [long value] (s-split "=" token)
      (let ((similars (seq-filter (lambda (option) (equal long (docopt-option-long option))) options)))
        (unless similars
          (setq similars (seq-filter (lambda (option)
                                       (and (docopt-option-long option)
                                            (s-starts-with-p (docopt-option-long option) long)))
                                     options) ))
        (cond
         ((> (length similars) 1)
          (docopt--error (docopt-tokens-error tokens) "%s is not a unique prefix: %s" long
                         (s-join ", " (seq-map #'docopt-option-long similars))))
         ((< (length similars) 1)
          (let* ((arg-count (if (s-match "=" token) 1 0))
                 (option (docopt-option :long long :arg-count arg-count)))
            (nconc options (list option))
            (if (equal 'docopt-exit (docopt-tokens-error tokens))
                (list (docopt-option :arg-count arg-count :long long :value value))
              (list option))))
         (t (with-slots (arg-count long short value) (car similars)
              (let ((option (docopt-option :arg-count arg-count :long long :short short :value value)))
                (if (and (zerop arg-count) value)
                    (docopt--error (docopt-tokens-error tokens) "%s must not have an argument" long)
                  (unless value
                    (when (member (docopt-tokens-current tokens) (list nil "--"))
                      (docopt--error (docopt-tokens-error tokens) "%s requires argument" long))
                    (setq value (docopt-tokens-move tokens))))
                (when (equal 'docopt-exit (docopt-tokens-error tokens))
                  (setq value (or value t)))
                (list option)))))))))

(defun docopt--parse-short (tokens options)
  "Parse a Docopt short option from TOKENS with OPTIONS."
  (let* ((token (docopt-tokens-move tokens))
         (left (s-replace-regexp "^-+" "" token))
         (parsed nil))
    (while (not (s-blank-p left))
      (let* ((short (substring left 0 1))
             (option nil)
             (similars (seq-find (lambda (option) (equal short (docopt-option-short option))) options)))
        (setq left (substring left 1))
        (cond
         ((> (length similars) 1)
          (docopt--error (docopt-tokens-error tokens) "%s is specified ambiguously %d times" short (length similars)))
         ((< (length similars) 1)
          (setq option (docopt-option :arg-count 0 :short short))
          (nconc options (list option))
          (when (equal 'docopt-exit (docopt-tokens-error tokens))
            (setq option (docopt-option :arg-count 0 :short short :value t))))
         (t (let* ((similar (car similars))
                   (value nil))
              (setq option (docopt-option
                            :arg-count (docopt-option-arg-count similar)
                            :long (docopt-option-long similar)
                            :short short
                            :value (docopt-option-value similar)))
              (unless (zerop (docopt-option-arg-count option))
                (if (s-blank-p left)
                    (progn
                      (when (member (docopt-tokens-move tokens) '(nil "--"))
                        (docopt--error (docopt-tokens-error tokens) "%s requires argument" short))
                      (setq value (docopt-tokens-move tokens)))
                  (setq value left
                        left "")))
              (when (equal 'docopt-exit (docopt-tokens-error tokens))
                (oset option :value (when value t))))))
        (setq parsed (cons option parsed))))
    (reverse parsed)))

(defun docopt--parse-argument (tokens)
  "Parse a Docopt argument from TOKENS."
  (list (docopt-argument :name (docopt-tokens-move tokens))))

(defun docopt--parse-command (tokens)
  "Parse a Docopt command from TOKENS."
  (list (docopt-command :name (docopt-tokens-move tokens))))

(defun docopt--parse-options-shortcut (tokens)
  "Parse a Docopt command from TOKENS."
  (docopt-tokens-move tokens)
  (list (docopt-options-shortcut)))

(defun docopt--parse-optional-group (tokens)
  "Parse a Docopt required group from TOKENS."
  (docopt-tokens-move tokens)
  (error "TODO: Parse group"))

(defun docopt--parse-required-group (tokens)
  "Parse a Docopt optional group from TOKENS."
  (docopt-tokens-move tokens)
  (error "TODO: Parse group"))

(defun docopt--parse-atom (tokens options)
  "Parse a Docopt atom from TOKENS using OPTIONS."
  (cond
   ((docopt-tokens-optional-group-p tokens)
    (docopt--parse-optional-group tokens)

    (docopt-tokens-required-group-p tokens)
    (docopt--parse-required-group tokens))

   ((docopt-tokens-options-shortcut-p tokens)
    (docopt--parse-options-shortcut tokens))

   ((docopt-tokens-long-option-p tokens)
    (docopt--parse-long tokens options))

   ((docopt-tokens-short-option-p tokens)
    (docopt--parse-short tokens options))

   ((docopt-tokens-argument-p tokens)
    (docopt--parse-argument tokens))

   (t (docopt--parse-command tokens))))

(defun docopt--parse-exprs (tokens options)
  "Parse the Docopt expressions from TOKENS using OPTIONS."
  (docopt--parse-seq tokens options))

(defun docopt--parse-seq (tokens options)
  "Parse a sequence of Docopt expressions from TOKENS using OPTIONS."
  (let ((results nil))
    (while (docopt-tokens-current tokens)
      (unless (member (docopt-tokens-current tokens) '("]" ")" "|"))
        (let ((atoms (docopt--parse-atom tokens options)))
          (when (docopt-tokens-current-p tokens "...")
            (setq atoms (docopt-one-or-more :children atoms))
            (docopt-tokens-move tokens))
          (setq results (cons atoms results)))))
    (reverse results)))

(defun docopt--parse-patterns (source options)
  "Parse the usage patterns from Docopt SOURCE using OPTIONS."
  (let* ((tokens (docopt-tokens-from-pattern source))
         (result (docopt--parse-exprs tokens options)))
    result))

(defun docopt--parse-option (source)
  "Parse a Docopt option from SOURCE."
  (let ((short nil)
        (long nil)
        (arg-count 0)
        (value nil))
    (seq-let [options description] (s-split "\s\\{2,\\}" (s-trim source))
      (unless (s-blank-p options)
        (let ((options (s-replace "=" " " (s-replace "," " " options))))
          (seq-doseq (s (s-split "\s+" options))
            (cond
             ((s-starts-with-p "--" s)
              (setq long s))
             ((s-starts-with-p "-" s)
              (setq short s))
             (t (setq arg-count 1))))
          (when (> arg-count 0)
            (setq value (cadr (s-match "\\[default: \\(.*\\)\\]" description))))
          (docopt-option
           :arg-count arg-count
           :description description
           :long long
           :short short
           :value value))))))

(defun docopt--parse-usage (source)
  "Parse the Docopt usage section from SOURCE."
  (let ((sections (docopt--parse-section "usage:" source)))
    (when (zerop (length sections))
      (error "No Docopt usage section found"))
    (when (> (length sections) 1)
      (error "More than one Docopt usage section found"))
    (car sections)))

(defun docopt-parse-program (source)
  "Parse the Docopt program from SOURCE."
  (let ((program (docopt-program :source source))
        (usage (docopt--parse-usage source)))
    (with-slots (options patterns) program
      (setq options (docopt--parse-defaults source))
      (setq patterns (docopt--parse-patterns (docopt--formal-usage usage) options))
      program
      patterns)))

(provide 'docopt)

;;; docopt.el ends here

;; (require 'cl-print)
;; (setq cl-print-readably t)

;; (docopt-parse-program "Usage: program --help")
;; (docopt-parse-program "Usage: program a")

;; (docopt-parse-program docopt-naval-fate-str)

;; (docopt--parse-long (docopt-tokens-from-pattern "--help") nil)
;; (docopt--parse-long (docopt-tokens-from-pattern "--help=yo") nil)

;; (setq my-options (list (docopt-option :short "x")))

;; (nconc (list 1) my-list)

;; (docopt--parse-short (docopt-tokens-from-pattern "-ab") my-options)

;; (require 'cl-print)
;; (setq cl-print-readably t)

;; (docopt--parse-long (docopt-tokens-from-pattern "--help=yo") nil)
