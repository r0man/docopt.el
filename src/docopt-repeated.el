;;; docopt-repeated.el --- Docopt repeated -*- lexical-binding: t -*-

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

;; The Docopt repeated class

;;; Code:

(require 'docopt-generic)
(require 'eieio)

(defclass docopt-repeated ()
  ((object
    :initarg :object
    :initform nil
    :accessor docopt-repeated-object
    :documentation "The repeated object."))
  "A class representing a repeatable object.")

(defun docopt-make-repeated (object)
  "Make a new argument using OBJECT."
  (docopt-repeated :object (docopt-set-repeat object t)))

(cl-defmethod docopt-shell-arguments ((repeated docopt-repeated))
  "Return the shell argument list for the REPEATED object."
  (with-slots (object) repeated
    (docopt-shell-arguments object)))

(cl-defmethod docopt-collect-arguments ((repeated docopt-repeated))
  "Collect the arguments from the REPEATED."
  (docopt-collect-arguments (docopt-repeated-object repeated)))

(cl-defmethod docopt-collect-commands ((repeated docopt-repeated))
  "Collect the commands from the REPEATED."
  (docopt-collect-commands (docopt-repeated-object repeated)))

(cl-defmethod docopt-collect-options ((repeated docopt-repeated))
  "Collect the options from the REPEATED."
  (docopt-collect-options (docopt-repeated-object repeated)))

(cl-defmethod docopt-format ((repeated docopt-repeated))
  "Convert the usage REPEATED to a formatted string."
  (docopt-format (docopt-repeated-object repeated)))

(cl-defmethod docopt-string ((repeated docopt-repeated))
  "Convert the usage REPEATED to a string."
  (concat (docopt-string (docopt-repeated-object repeated)) "..."))

(cl-defmethod docopt-walk ((repeated docopt-repeated) f)
  "Walk the REPEATED of an abstract syntax tree and apply F on it."
  (with-slots (object) repeated
    (setq object (docopt-walk object f))
    (funcall f repeated)))

;; Repeatable

(defclass docopt-repeatable ()
  ((repeat
    :initarg :repeat
    :initform nil
    :accessor docopt-repeat-p
    :documentation "Whether the object can be repeated or not."))
  "A class providing a :repeat slot for a object.")

(cl-defmethod docopt-set-repeat ((object docopt-repeatable) value)
  "Set the :repeat slot of OBJECT to VALUE."
  (setf (oref object :repeat) value)
  object)

(provide 'docopt-repeated)

;;; docopt-repeated.el ends here
