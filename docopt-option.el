;;; docopt-option.el --- The Docopt option class -*- lexical-binding: t -*-

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

;; The Docopt option class

;;; Code:

(require 'eieio)

(defclass docopt-option ()
  ((argument
    :initarg :argument
    :initform nil
    :accessor docopt-option-argument
    :documentation "The argument of the option.")
   (description
    :initarg :description
    :initform nil
    :accessor docopt-option-description
    :documentation "The description of the option.")
   (name
    :initarg :name
    :initform nil
    :accessor docopt-option-name
    :documentation "The long name of the option."))
  "A class representing a Docopt base option.")

;;; Long Option

(defclass docopt-long-option (docopt-option) ()
  "A class representing a Docopt long option.")

(defun docopt-make-long-option (&rest args)
  "Make a new Docopt long option using ARGS."
  (apply 'make-instance 'docopt-long-option args))

;;; Short option

(defclass docopt-short-option (docopt-option) ()
  "A class representing a Docopt short option.")

(defun docopt-make-short-option (&rest args)
  "Make a new Docopt short option using ARGS."
  (apply 'make-instance 'docopt-short-option args))

(provide 'docopt-option)

;;; docopt-option.el ends here
