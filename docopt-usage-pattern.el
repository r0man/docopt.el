;;; docopt-usage-pattern.el --- The Docopt usage pattern class -*- lexical-binding: t -*-

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

;; The Docopt usage pattern class

;;; Code:

(require 'docopt-generic)
(require 'eieio)
(require 'subr-x)

(defclass docopt-usage-pattern ()
  ((command
    :accessor docopt-usage-pattern-command
    :documentation "The command of the usage pattern."
    :initarg :command
    :initform nil
    :type (or docopt-command null))
   (expressions
    :accessor docopt-usage-pattern-expressions
    :documentation "The expressions of the usage pattern."
    :initarg :expressions
    :initform nil
    :type (or list null)))
  "A class representing a Docopt usage pattern.")

(cl-defmethod clone ((pattern docopt-usage-pattern) &rest params)
  "Return a copy of the usage PATTERN and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method pattern params)))
    (with-slots (command expressions) copy
      (setq command (clone (docopt-usage-pattern-command pattern)))
      (setq expressions (clone (docopt-usage-pattern-expressions pattern)))
      copy)))

(cl-defmethod docopt-shell-arguments ((pattern docopt-usage-pattern))
  "Return the shell argument list for the usage PATTERN."
  (with-slots (command expressions) pattern
    (append (docopt-shell-arguments command)
            (seq-mapcat #'docopt-shell-arguments expressions))))

(cl-defmethod docopt-name ((usage-pattern docopt-usage-pattern))
  "Return the name of USAGE-PATTERN."
  (when-let ((command (docopt-usage-pattern-command usage-pattern)))
    (docopt-name command)))

(cl-defmethod docopt-format ((pattern docopt-usage-pattern))
  "Convert the Docopt usage PATTERN to a formatted string."
  (with-slots (command expressions) pattern
    (concat (docopt-string command) " " (s-join " " (seq-map #'docopt-format expressions)))))

(cl-defmethod docopt-string ((pattern docopt-usage-pattern))
  "Convert the Docopt usage PATTERN to a string."
  (with-slots (command expressions) pattern
    (concat (docopt-string command) " " (s-join " " (seq-map #'docopt-string expressions)))))

(cl-defmethod docopt-walk ((pattern docopt-usage-pattern) f)
  "Walk the usage PATTERN of an abstract syntax tree and apply F on it."
  (with-slots (command expressions) pattern
    (setq command (docopt-walk command f))
    (setq expressions (docopt-walk expressions f))
    (funcall f pattern)))

(cl-defmethod docopt-collect-arguments ((usage-pattern docopt-usage-pattern))
  "Collect the arguments from the Docopt USAGE-PATTERN."
  (docopt-collect-arguments (docopt-usage-pattern-expressions usage-pattern)))

(cl-defmethod docopt-collect-commands ((usage-pattern docopt-usage-pattern))
  "Collect the commands from the Docopt USAGE-PATTERN."
  (docopt-collect-commands (docopt-usage-pattern-expressions usage-pattern)))

(cl-defmethod docopt-collect-options ((usage-pattern docopt-usage-pattern))
  "Collect the options from the Docopt USAGE-PATTERN."
  (docopt-collect-options (docopt-usage-pattern-expressions usage-pattern)))

(defun docopt-make-usage-pattern (command &rest expressions)
  "Make a new Docopt usage pattern with COMMAND and EXPRESSIONS."
  (make-instance 'docopt-usage-pattern :command command :expressions expressions))

(defun docopt-usage-pattern-collect-repeatable (usage-pattern)
  "Collect all repeatable elements in USAGE-PATTERN."
  (let ((result nil))
    (docopt-walk usage-pattern
                 (lambda (element)
                   (when (cl-typep element 'docopt-repeatable)
                     (setq result (cons element result)))
                   element))
    (reverse result)))

(defun docopt-usage-pattern-set-repeat (usage-pattern)
  "Set the :repeat slot of repeatable elements occurring more then once in USAGE-PATTERN to t."
  (thread-last (docopt-usage-pattern-collect-repeatable usage-pattern)
    (seq-group-by (lambda (element)
                    (cons (eieio-object-class-name element)
                          (docopt-name element))))
    (seq-map #'cdr)
    (seq-filter (lambda (group) (> (length group) 1)))
    (seq-map (lambda (group) (docopt-set-repeat group t)))))

(provide 'docopt-usage-pattern)

;;; docopt-usage-pattern.el ends here
