;; docopt.el --- A Docopt implementation in Elisp -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 's)
(require 'seq)

;; Generics

(cl-defgeneric docopt--flat (pattern &optional types)
  "Flatten the PATTERN and filter by TYPES.")

(cl-defgeneric docopt--match (pattern left &optional collected)
  "Match PATTERN against the argument vector LEFT and COLLECTED.")

(cl-defgeneric docopt--single-match (pattern left)
  "Match PATTERN against the argument vector LEFT.")

;; Pattern

(defclass docopt-pattern () ()
  "A class representing a Docopt pattern.")

(defun docopt-pattern--transform (pattern)
  "Transform the Docopt PATTERN into an (almost) equivalent one using a single either."
  (let ((result nil)
        (groups (list (list pattern)))
        (child-p (lambda (child)
                   (or (docopt-either-p child)
                       (docopt-one-or-more-p child)
                       (docopt-optional-p child)
                       (docopt-options-shortcut-p child)
                       (docopt-required-p child)))))
    (while groups
      (let ((children (pop groups)))
        (if (cl-some child-p children)
            (let ((child (seq-find child-p children)))
              (setq children (delete child children))
              (cond
               ((docopt-either-p child)
                (seq-doseq (child (docopt-children child))
                  (setq groups (append groups (list (cons child children))))))
               ((docopt-one-or-more-p child)
                (setq groups (append groups
                                     (list (append (docopt-children child)
                                                   (docopt-children child)
                                                   children)))))
               (t (setq groups (append groups (list (append (docopt-children child) children)))))))
          (setq result (append result (list children))))))
    (docopt-either :children (seq-map (lambda (result) (docopt-required :children result)) result))))

(defun docopt-pattern--fix-identities (pattern &optional uniq)
  "Rewrite elements in PATTERN to point at UNIQ elements."
  (if (docopt-branch-pattern-child-p pattern)
      (let ((uniq (cl-remove-duplicates (or uniq (docopt--flat pattern)) :test #'equal)))
        (with-slots (children) pattern
          (seq-map-indexed
           (lambda (child index)
             (if (docopt-branch-pattern-child-p child)
                 (docopt-pattern--fix-identities child uniq)
               (setq children (-replace-at index (seq-find (lambda (element) (equal child element)) uniq) children ))))
           children)
          pattern))
    pattern))

(defun docopt-pattern--fix-repeating-arguments (pattern)
  "Rewrite the :value slot of elements in PATTERN that should accumulate or increment values."
  (docopt-pattern--fix-identities pattern)
  (let ((either (seq-map #'docopt-children (docopt-children (docopt-pattern--transform pattern)))))
    (seq-map  (lambda (element)
                (seq-map (lambda (child)
                           (when (or (docopt-argument-p child)
                                     (and (docopt-option-p child)
                                          (> (docopt-option-arg-count child) 0)))
                             (with-slots (value) child
                               (cond ((not value)
                                      (setq value []))
                                     ((stringp value)
                                      (setq value (docopt--split value))))))
                           (when (or (docopt-command-p child)
                                     (and (docopt-option-p child)
                                          (zerop (docopt-option-arg-count child))))
                             (oset child :value 0)))
                         (seq-filter (lambda (child)
                                       (> (-count (lambda (x) (equal child x)) element) 1))
                                     element)))
              either)
    pattern))

;; Branch pattern

(defclass docopt-branch-pattern (docopt-pattern)
  ((children
    :accessor docopt-children
    :documentation "The children of the pattern."
    :initarg :children
    :initform nil
    :type (or list null)))
  "A class representing a Docopt branch pattern.")

(cl-defmethod docopt--flat ((pattern docopt-branch-pattern) &optional types)
  "Flatten the branch PATTERN and filter by TYPES."
  (if (member (eieio--object-class-tag pattern) types)
      (list pattern)
    (seq-mapcat (lambda (child)
                  (docopt--flat child types))
                (docopt-children pattern))))

;; Leaf pattern

(defclass docopt-leaf-pattern (docopt-pattern)
  ((name
    :accessor docopt-name
    :documentation "The name of the pattern."
    :initarg :name
    :initform nil
    :type (or string null))
   (value
    :accessor docopt-value
    :documentation "The value of the pattern."
    :initarg :value
    :initform nil
    :type (or string t null)))
  "A class representing a Docopt leaf pattern.")

(cl-defmethod docopt--flat ((pattern docopt-leaf-pattern) &optional types)
  "Flatten the leaf PATTERN and filter by TYPES."
  (when (or (not types) (member (eieio--object-class-tag pattern) types))
    (list pattern)))

(cl-defmethod docopt--match ((pattern docopt-leaf-pattern) left &optional collected)
  "Match PATTERN against the argument vector LEFT and COLLECTED."
  (with-slots (name value) pattern
    (seq-let [pos match] (docopt--single-match pattern left)
      (if match
          (let ((left (append (seq-take left pos) (seq-drop left (+ 1 pos))))
                (same-names (seq-filter (lambda (element) (equal name (docopt-name element))) collected)))
            (if (or (integerp value)
                    (listp value))
                (let ((increment (if (integerp value)
                                     1
                                   (if (stringp (docopt-value match))
                                       (list  (docopt-value match) )
                                     (docopt-value match)))))
                  (if (not same-names)
                      (with-slots (value) match
                        (setq value increment)
                        (list t left (append collected (list match))))
                    (with-slots (value) (car similar-names)
                      (setq value (+ value increment))
                      (list t left collected))))
              (list t left (append collected (list match)))))
        (list nil left collected)))))

;; Argument

(defclass docopt-argument (docopt-leaf-pattern) ()
  "A class representing a Docopt argument.")

(cl-defmethod docopt--single-match ((argument docopt-argument) left)
  "Match ARGUMENT against the argument vector LEFT."
  (thread-last left
    (seq-map-indexed
     (lambda (element index)
       (when (docopt-argument-p element)
         (list index (docopt-argument
                      :name (docopt-name argument)
                      :value (docopt-value element))))))
    (seq-remove #'null)
    (car)))

;; Command

(defclass docopt-command (docopt-argument) ()
  "A class representing a Docopt command.")

(cl-defmethod docopt--single-match ((command docopt-command) left)
  "Match COMMAND against the argument vector LEFT."
  (thread-last left
    (seq-map-indexed
     (lambda (element index)
       (when (docopt-argument-p element)
         (list index (docopt-command
                      :name (docopt-name command)
                      :value t)))))
    (seq-remove #'null)
    (car)))

;; Either

(defclass docopt-either (docopt-branch-pattern) ()
  "A class representing a Docopt either.")

(defun docopt-make-either (&rest children)
  "Make a new Docopt either element using CHILDREN."
  (docopt-either :children children))

;; One or More

(defclass docopt-one-or-more (docopt-branch-pattern) ()
  "A class representing one or more Docopt elements.")

(defun docopt-make-one-or-more (&rest children)
  "Make a new Docopt one-or-more element using CHILDREN."
  (docopt-one-or-more :children children))

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

;; Optional

(defclass docopt-optional (docopt-branch-pattern) ()
  "A class representing a Docopt optional element.")

(defun docopt-make-optional (&rest children)
  "Make a new Docopt optional instance using CHILDREN."
  (docopt-optional :children children))

;; Options Shortcut

(defclass docopt-options-shortcut (docopt-optional) ()
  "A class representing a Docopt options shortcut.")

;; Required

(defclass docopt-required (docopt-branch-pattern) ()
  "A class representing a Docopt required element.")

(defun docopt-make-required (&rest children)
  "Make a new Docopt required instance using CHILDREN."
  (docopt-required :children children))

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
         (t (let* ((similar (car similars))
                   (option (docopt-option
                            :arg-count (docopt-option-arg-count similar)
                            :description (docopt-option-description similar)
                            :long (docopt-option-long similar)
                            :short (docopt-option-short similar)
                            :value (docopt-option-value similar))))
              (if (zerop (docopt-option-arg-count option))
                  (when value
                    (docopt--error (docopt-tokens-error tokens) "%s must not have an argument" long))
                (unless value
                  (when (member (docopt-tokens-current tokens) (list nil "--"))
                    (docopt--error (docopt-tokens-error tokens) "%s requires argument" long))
                  (setq value (docopt-tokens-move tokens))))
              (when (equal 'docopt-exit (docopt-tokens-error tokens))
                (setq value (or value t)))
              (list option))))))))

(defun docopt--parse-short (tokens options)
  "Parse a Docopt short option from TOKENS with OPTIONS."
  (let* ((token (docopt-tokens-move tokens))
         (left (s-replace-regexp "^-+" "" token))
         (parsed nil))
    (while (not (s-blank-p left))
      (let* ((short (concat "-" (substring left 0 1)))
             (option nil)
             (similars (seq-filter (lambda (option) (equal short (docopt-option-short option))) options)))
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
                            :description (docopt-option-description similar)
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

(defun docopt--parse-optional-group (tokens options)
  "Parse a Docopt required group from TOKENS using OPTIONS."
  (let ((token (docopt-tokens-current tokens)))
    (docopt-tokens-move tokens)
    (let ((exprs (docopt--parse-exprs tokens options)))
      (unless (equal "]" (docopt-tokens-move tokens))
        (docopt--error (docopt-tokens-error tokens) "unmatched '%s'" token))
      (list (docopt-optional :children exprs)))))

(defun docopt--parse-required-group (tokens options)
  "Parse a Docopt optional group from TOKENS using OPTIONS."
  (let ((token (docopt-tokens-current tokens)))
    (docopt-tokens-move tokens)
    (let ((exprs (docopt--parse-exprs tokens options)))
      (unless (equal ")" (docopt-tokens-move tokens))
        (docopt--error (docopt-tokens-error tokens) "unmatched '%s'" token))
      (list (docopt-required :children exprs)))))

(defun docopt--parse-atom (tokens options)
  "Parse a Docopt atom from TOKENS using OPTIONS."
  (cond ((docopt-tokens-optional-group-p tokens)
         (docopt--parse-optional-group tokens options))
        ((docopt-tokens-required-group-p tokens)
         (docopt--parse-required-group tokens options))
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
  (let ((seq (docopt--parse-seq tokens options)))
    (if (not (equal "|" (docopt-tokens-current tokens)))
        seq
      (let ((result (if (> (length seq) 1)
                        (list (docopt-required :children seq))
                      seq)))
        (while (equal "|" (docopt-tokens-current tokens))
          (docopt-tokens-move tokens)
          (setq seq (docopt--parse-seq tokens options))
          (setq result (append result (if (> (length seq) 1)
                                          (list (docopt-required :children seq))
                                        seq))))
        (if (> (length result) 1)
            (list (docopt-either :children result))
          result)))))

(defun docopt--parse-seq (tokens options)
  "Parse a sequence of Docopt expressions from TOKENS using OPTIONS."
  (let ((results nil))
    (while (not (member (docopt-tokens-current tokens) '(nil "]" ")" "|")))
      (let ((atoms (docopt--parse-atom tokens options)))
        (when (docopt-tokens-current-p tokens "...")
          (setq atoms (list (docopt-one-or-more :children atoms)))
          (docopt-tokens-move tokens))
        (setq results (cons atoms results))))
    (apply #'append (reverse results))))

(defun docopt--parse-patterns (source options)
  "Parse the usage patterns from Docopt SOURCE using OPTIONS."
  (let* ((tokens (docopt-tokens-from-pattern source))
         (result (docopt--parse-exprs tokens options)))
    (when (docopt-tokens-current tokens)
      (docopt--error (docopt-tokens-error tokens) "unexpected ending: %s"
                     (s-join " " (docopt-tokens-list tokens))))
    (docopt-required :children result)))

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
      (docopt--error 'docopt-language-error "\"usage:\" (case-insensitive) not found"))
    (when (> (length sections) 1)
      (docopt--error 'docopt-language-error "More than one \"usage:\" (case-insensitive)"))
    (car sections)))

(defun docopt-parse-program (source)
  "Parse the Docopt program from SOURCE."
  (let ((program (docopt-program :source source))
        (usage (docopt--parse-usage source)))
    (with-slots (options patterns) program
      (setq options (docopt--parse-defaults source))
      (setq patterns (docopt--parse-patterns (docopt--formal-usage usage) options))
      (seq-doseq (shortcut (docopt--flat patterns '(docopt-options-shortcut)))
        (oset shortcut :children (cl-remove-duplicates
                                  (append options (docopt--flat patterns '(docopt-option)))
                                  :test #'equal)))
      program)))

(cl-defun docopt--parse-argv (program source &optional options-first)
  "Parse the argument vector of the Docopt PROGRAM from SOURCE according to OPTIONS-FIRST."
  (let ((tokens (docopt-tokens-from-pattern source))
        (options (docopt-program-options program))
        (parsed nil))
    (while (docopt-tokens-current tokens)
      (let ((token (docopt-tokens-current tokens)))
        (cond ((string= "--" token)
               (return-from docopt--parse-argv
                 (append (reverse parsed)
                         (seq-map (lambda (value) (docopt-argument :value value))
                                  (docopt-tokens-list tokens))) ))
              ((s-starts-with-p "--" token)
               (setq parsed (append (docopt--parse-long tokens options) parsed)))
              ((and (s-starts-with-p "-" token)
                    (not (equal "-" token)))
               (setq parsed (append (docopt--parse-short tokens options) parsed)))
              (options-first
               (return-from docopt--parse-argv
                 (append (reverse parsed)
                         (seq-map (lambda (value) (docopt-argument :value value))
                                  (docopt-tokens-list tokens)))))
              (t (setq parsed (cons (docopt-argument :value (docopt-tokens-move tokens)) parsed))))))
    (reverse parsed)))

(defun docopt-parse-argv (program source &optional options-first)
  "Parse the argument vector of the Docopt PROGRAM from SOURCE according to OPTIONS-FIRST."
  (let ((argv (docopt--parse-argv program source options-first))
        (patterns (docopt-program-patterns program)))
    (docopt--match patterns argv)))

(defun docopt--enumerate (lst)
  "Return the elements of LST with an index."
  (seq-map-indexed (lambda (element index) (list index element)) lst))

;; (docopt-parse-argv my-program "naval_fate.py ")

(provide 'docopt)

;;; docopt.el ends here

;; (require 'cl-print)
;; (setq cl-print-readably t)
                                        ;
;; (setq my-program (docopt-parse-program docopt-naval-fate-str))
;; (setq my-program (docopt-parse-program "Usage: program [options] add\nOptions:\n  --help  Help"))

;; (docopt-parse-program "Usage: program add")

;; (docopt-parse-program docopt-naval-fate-str)

;; (docopt-parse-program "Naval Fate.
;; Usage:
;;   naval_fate.py ship new <name>...
;;   naval_fate.py ship <name> move <x> <y> [--speed=<kn>]
;;   naval_fate.py ship shoot <x> <y>
;;   naval_fate.py mine (set|remove) <x> <y> [--moored|--drifting]
;;   naval_fate.py -h | --help
;;   naval_fate.py --version
;; Options:
;;   -h --help     Show this screen.
;;   --version     Show version.
;;   --speed=<kn>  Speed in knots [default: 10].
;;   --moored      Moored (anchored) mine.
;;   --drifting    Drifting mine.")

;; (docopt-parse-program "Usage: naval_fate.py ship new <name>...")
;; (docopt-parse-program "Usage: naval_fate.py ship <name> move <x> <y> [--speed=<kn>]")
;; (docopt-parse-program "Usage: naval_fate.py ship shoot <x> <y>")
;; (docopt-parse-program "Usage: naval_fate.py mine (set|remove) <x> <y> [--moored|--drifting]")
;; (docopt-parse-program "Usage: naval_fate.py -h | --help")
;; (docopt-parse-program "Usage: naval_fate.py --version")

;; (docopt--parse-long (docopt-tokens-from-pattern "--help") nil)
;; (docopt--parse-long (docopt-tokens-from-pattern "--help=yo") nil)

;; (setq my-options (list (docopt-option :short "x")))

;; (nconc (list 1) my-list)

;; (docopt--parse-short (docopt-tokens-from-pattern "-ab") my-options)

;; (require 'cl-print)
;; (setq cl-print-readably t)

;; (docopt--parse-long (docopt-tokens-from-pattern "--help=yo") nil)

;; (docopt-program-patterns my-program)
