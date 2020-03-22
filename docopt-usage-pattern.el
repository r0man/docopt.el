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

(cl-defmethod docopt-copy ((pattern docopt-usage-pattern))
  "Return a copy of the usage PATTERN."
  (let ((copy (copy-sequence pattern)))
    (with-slots (command expressions) copy
      (setq command (docopt-copy (docopt-usage-pattern-command argument)))
      (setq expressions (docopt-copy (docopt-usage-pattern-expressions argument)))
      copy)))

(cl-defmethod docopt-walk ((pattern docopt-usage-pattern) f)
  "Walk the usage PATTERN of an abstract syntax tree and apply F on it."
  (let ((pattern (copy-sequence pattern)))
    (with-slots (command expressions) pattern
      (setq command (docopt-walk command f))
      (setq expressions (docopt-walk expressions f))
      (funcall f pattern))))

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

(defun docopt-usage-pattern-options (usage-pattern)
  "Return the options of the USAGE-PATTERN."
  (seq-mapcat (lambda (expr)
                (docopt-usage-pattern-expr-option expr))
              (docopt-usage-pattern-expressions usage-pattern)))

(provide 'docopt-usage-pattern)

;;; docopt-usage-pattern.el ends here
