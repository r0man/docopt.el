;;; docopt-value.el --- Docopt value -*- lexical-binding: t -*-

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

;; The Docopt value base class

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(defclass docopt-value-base ()
  ((value
    :initarg :value
    :initform nil
    :accessor docopt-value
    :documentation "The value of the Docopt object."))
  "A base class for Docopt objects that have a value slot.")

(provide 'docopt-value)

;;; docopt-value.el ends here
