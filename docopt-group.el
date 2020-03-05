;;; docopt-group.el --- The Docopt group class -*- lexical-binding: t -*-

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

;; The Docopt group class

;;; Code:

(require 'docopt-generic)
(require 'eieio)

(defclass docopt-group ()
  ((members
    :accessor docopt-group-members
    :documentation "The argument of the option."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a Docopt group.")

;;; Optional Group

(defclass docopt-optional-group (docopt-group) ()
  "A class representing a required Docopt group.")

(defun docopt-make-optional-group (&rest members)
  "Make a new optional Docopt group with MEMBERS."
  (make-instance 'docopt-optional-group :members members))

;;; Required Group

(defclass docopt-required-group (docopt-group) ()
  "A class representing a required Docopt group.")

(defun docopt-make-required-group (&rest members)
  "Make a new required Docopt group with MEMBERS."
  (make-instance 'docopt-required-group :members members))

(cl-defmethod docopt-collect-arguments ((group docopt-group))
  "Collect the arguments from the Docopt GROUP."
  (docopt-collect-arguments (docopt-group-members group)))

(provide 'docopt-group)

;;; docopt-group.el ends here
