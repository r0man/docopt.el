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

(defun docopt-make-usage-pattern (command &rest expressions)
  "Make a new Docopt usage pattern with COMMAND and EXPRESSIONS."
  (make-instance 'docopt-usage-pattern :command command :expressions expressions))

(provide 'docopt-usage-pattern)

;;; docopt-usage-pattern.el ends here
