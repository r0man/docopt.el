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

(require 'docopt-argument)
(require 'eieio)

(defclass docopt-option ()
  ((argument
    :accessor docopt-option-argument
    :documentation "The argument of the option."
    :initarg :argument
    :initform nil
    :type (or docopt-argument null))
   (description
    :accessor docopt-option-description
    :documentation "The description of the option."
    :initarg :description
    :initform nil
    :type (or string null))
   (name
    :accessor docopt-option-name
    :documentation "The long name of the option."
    :initarg :name
    :initform nil
    :type (or string null)))
  "A class representing a Docopt base option.")

;;; Long Option

(defclass docopt-long-option (docopt-option) ()
  "A class representing a Docopt long option.")

;;; Short option

(defclass docopt-short-option (docopt-option) ()
  "A class representing a Docopt short option.")

(defun docopt-option-set-default (option default)
  "Set the default argument value of OPTION to DEFAULT."
  (when-let ((argument (docopt-option-argument option)))
    (oset argument :default default)))

(defun docopt-option-set-description-and-default (option description default)
  "Set the DESCRIPTION and DEFAULT of the OPTION."
  (when option
    (oset option :description description)
    (docopt-option-set-default option default)))

(provide 'docopt-option)

;;; docopt-option.el ends here
