;;; docopt-argument.el --- The Docopt argument class -*- lexical-binding: t -*-

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

;; The Docopt argument class

;;; Code:

(require 'eieio)

(defclass docopt-argument ()
  ((default
     :accessor docopt-argument-default
     :documentation "The default of the argument."
     :initarg :default
     :initform nil
     :type (or string null))
   (name
    :accessor docopt-argument-name
    :documentation "The name of the argument."
    :initarg :name
    :initform nil
    :type (or string null))
   (value
    :accessor docopt-argument-value
    :documentation "The value of the argument."
    :initarg :value
    :initform nil
    :type (or string null)))
  "A class representing a Docopt argument.")

(defun docopt-make-argument (&rest args)
  "Make a new Docopt argument using ARGS."
  (apply 'make-instance 'docopt-argument args))

(provide 'docopt-argument)

;;; docopt-argument.el ends here
