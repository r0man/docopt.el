;;; docopt-program.el --- Docopt program -*- lexical-binding: t -*-

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

;; The Docopt program class

;;; Code:

(require 'docopt-abbrev)
(require 'docopt-generic)
(require 'docopt-option)
(require 'docopt-key)
(require 'eieio)
(require 'seq)

(defcustom docopt-string-options-width 20
  "The width of the options on a Docopt options line."
  :type 'number
  :group 'docopt)

(defclass docopt-program ()
  ((examples
    :accessor docopt-program-examples
    :documentation "The examples of the program."
    :initarg :examples
    :initform nil
    :type (or list null))
   (footer
    :accessor docopt-program-footer
    :documentation "The footer of the program."
    :initarg :footer
    :initform nil
    :type (or string null))
   (header
    :accessor docopt-program-header
    :documentation "The header of the program."
    :initarg :header
    :initform nil
    :type (or string null))
   (name
    :accessor docopt-program-name
    :documentation "The name of the program."
    :initarg :name
    :initform nil
    :type (or string null))
   (options
    :accessor docopt-program-options
    :documentation "The options of the program."
    :initarg :options
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

(cl-defmethod docopt-collect-arguments ((program docopt-program))
  "Collect the arguments from the Docopt PROGRAM."
  (seq-mapcat #'docopt-collect-arguments (docopt-program-usage program)))

(cl-defmethod docopt-collect-commands ((program docopt-program))
  "Collect the commands from the Docopt PROGRAM."
  (seq-mapcat #'docopt-collect-commands (docopt-program-usage program)))

(cl-defmethod docopt-collect-options ((program docopt-program))
  "Collect the options from the Docopt PROGRAM."
  (seq-concatenate
   'list
   (seq-mapcat #'docopt-collect-options (docopt-program-usage program))
   (docopt-program-options program)))

(defun docopt-collect-standard-input (program)
  "Collect the standard input from the Docopt PROGRAM."
  (let ((results nil))
    (docopt-walk program (lambda (element)
                           (if (docopt-standard-input-p element)
                               (setq results (cons element results)))
                           element))
    (docopt-remove-duplicates results)))

(cl-defmethod clone ((program docopt-program) &rest params)
  "Return a copy of the usage PROGRAM and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method program params)))
    (with-slots (examples footer header options source usage) copy
      (setq examples (clone (docopt-program-examples program)))
      (setq footer (clone (docopt-program-footer program)))
      (setq header (clone (docopt-program-header program)))
      (setq options (clone (docopt-program-options program)))
      (setq source (clone (docopt-program-source program)))
      (setq usage (clone (docopt-program-usage program)))
      copy)))

(cl-defmethod docopt-name ((program docopt-program))
  "Return the name of PROGRAM."
  (when-let ((usage-pattern (car (docopt-program-usage program))))
    (docopt-name usage-pattern)))

(cl-defmethod docopt-equal ((program docopt-program) other)
  "Return t if PROGRAM and OTHER are equal-ish."
  (with-slots (usage options) program
    (and (docopt-program-p other)
         (equal usage (docopt-program-usage other))
         (equal options (docopt-program-options other)))))

(defun docopt-program-arguments (program)
  "Return the arguments of the PROGRAM."
  (docopt-remove-duplicates (docopt-collect-arguments program)))

(defun docopt-program-commands (program)
  "Return the commands of the PROGRAM."
  (docopt-remove-duplicates (docopt-collect-commands program)))

(defun docopt-program-long-options (program)
  "Return the long options of PROGRAM."
  (seq-filter #'docopt-long-option-p (docopt-program-options program)))

(defun docopt-program-short-options (program)
  "Return the short options of PROGRAM."
  (seq-filter #'docopt-short-option-p (docopt-program-options program)))

(defun docopt-program-argument (program name)
  "Return the long or short argument of PROGRAM by NAME."
  (seq-find (lambda (argument) (equal name (docopt-argument-name argument)))
            (docopt-program-arguments program)))

(defun docopt-program-command (program name)
  "Return the long or short command of PROGRAM by NAME."
  (seq-find (lambda (command) (equal name (docopt-command-name command)))
            (docopt-program-commands program)))

(defun docopt-program-option (program name)
  "Return the long or short option of PROGRAM by NAME."
  (seq-find (lambda (option) (equal name (docopt-option-name option)))
            (docopt-program-options program)))

(defun docopt-program-shell-command (command)
  "Run the shell COMMAND with the --help option and parse the result as a Docopt program."
  (let ((program (docopt-parse (shell-command-to-string (concat command " --help")))))
    (setf (oref program name) command)
    program))

(defun docopt-program-set-sections (program sections)
  "Set the sections of the PROGRAM to SECTIONS."
  (seq-doseq (section sections)
    (seq-let [slot value] section
      (eieio-oset program slot value))))

(defun docopt-program-argv-normalize (program)
  "Return a list of normalized Docopt argv elements for PROGRAM."
  (seq-concatenate 'list
                   (docopt-remove-duplicates (docopt-collect-arguments program))
                   (docopt-collect-commands program)
                   (seq-remove (lambda (option)
                                 (and (docopt-short-option-p option)
                                      (docopt-option-synonym option)))
                               (docopt-program-options program))
                   (docopt-collect-standard-input program)))

(cl-defmethod docopt-string ((program docopt-program))
  "Convert the Docopt PROGRAM to a string."
  (thread-last (list (docopt-program-header program)
                     (docopt-string--usage (docopt-program-usage program))
                     (docopt-string--options (docopt-program-options program))
                     (docopt-string--examples (docopt-program-examples program)))
    (seq-remove #'s-blank-p)
    (s-join "\n\n")
    (docopt-strip)))

(cl-defmethod docopt-walk ((program docopt-program) f)
  "Walk the PROGRAM of an abstract syntax tree and apply F on it."
  (with-slots (header examples footer usage options) program
    (setq header (docopt-walk header f))
    (setq examples (docopt-walk examples f))
    (setq usage (docopt-walk usage f))
    (funcall f program)))

(defun docopt-string--section (header content)
  "Convert the Docopt section HEADER and CONTENT to a string."
  (unless (zerop (length content))
    (concat header ":\n  " (string-join content "\n  "))))

(defun docopt-string--example (example)
  "Convert the Docopt EXAMPLE to a string."
  (string-join example " "))

(defun docopt-string--examples (examples)
  "Convert the Docopt EXAMPLES to a string."
  (docopt-string--section "Examples" (seq-map #'docopt-string--example examples)))

(defun docopt-string--usage (usage)
  "Convert the Docopt USAGE to a string."
  (docopt-string--section "Usage" (seq-map #'docopt-string usage)))

(defun docopt-string--synonym (synonym)
  "Convert the Docopt SYNONYM to a string."
  (when synonym
    (if (= 1 (length synonym))
        (concat "-" synonym)
      (concat "--" synonym))))

(defun docopt-string--options (options)
  "Convert the Docopt OPTIONS to a string."
  (docopt-string--section
   "Options" (thread-last options
               (seq-remove (lambda (option)
                             (and (docopt-short-option-p option)
                                  (docopt-option-synonym option))))
               (seq-map (lambda (option)
                          (format (concat "%-" (number-to-string docopt-string-options-width) "s %s")
                                  (concat
                                   (when-let ((synonym (oref option synonym)))
                                     (concat (docopt-string--synonym synonym) ", "))
                                   (docopt-string option))
                                  (or (docopt-option-description option) "")))))))

(provide 'docopt-program)

;;; docopt-program.el ends here
