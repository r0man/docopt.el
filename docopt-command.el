;;; docopt-command.el --- The Docopt command class -*- lexical-binding: t -*-

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

;; The Docopt command class

;;; Code:

(require 'eieio)

(defclass docopt-command ()
  ((arguments
    :initarg :arguments
    :initform nil
    :accessor docopt-command-arguments
    :documentation "The arguments of the command.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-command-name
    :documentation "The name of the command.")
   (long-options
    :initarg :long-options
    :initform nil
    :accessor docopt-command-long-options
    :documentation "The long-options of the command.")
   (short-options
    :initarg :short-options
    :initform nil
    :accessor docopt-command-short-options
    :documentation "The short-options of the command."))
  "A class representing a Docopt command.")

(defun docopt-make-command (&rest args)
  "Make a new Docopt command using ARGS."
  (apply 'make-instance 'docopt-command args))

(provide 'docopt-command)

;;; docopt-command.el ends here
