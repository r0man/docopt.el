;;; docoptoup-repeated.el --- The Docopt repeated class -*- lexical-binding: t -*-

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
  "A class representing a repeatable Docopt object.")

(defun docopt-make-repeated (object)
  "Make a new Docopt argument using OBJECT."
  (make-instance 'docopt-repeated :object object))

(cl-defmethod docopt-collect-arguments ((repeated docopt-repeated))
  "Collect the arguments from the Docopt REPEATED."
  (docopt-collect-arguments (docopt-repeated-object repeated)))

(provide 'docopt-repeated)

;;; docopt-repeated.el ends here
