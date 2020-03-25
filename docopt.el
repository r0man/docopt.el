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

(require 'eieio)
(require 's)
(require 'seq)

(defclass docopt-option ()
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
    :type (or string null)))
  "A class representing a Docopt option.")

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
    :type (or list null))
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
                  (let* ((s (s-replace "options:" "" section))
                         (split (s-slice-at "\n[ \t]*\\(-[a-z-]+\\)" (concat "\n" s))))
                    (seq-remove #'s-blank-p (seq-map #'s-trim split)))))))

(defun docopt--parse-option (source)
  "Parse a Docopt option from SOURCE."
  (let ((short nil)
        (long nil)
        (arg-count 0)
        (value nil))
    (seq-let [options description] (s-split "\s\\{2,\\}" source)
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
         :value value)))))

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
    program))

(provide 'docopt)

;;; docopt.el ends here
