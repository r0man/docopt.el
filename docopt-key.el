;;; docopt-key.el --- The Docopt key -*- lexical-binding: t -*-

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

;; The Docopt key base class

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(defclass docopt-key-base ()
  ((key
    :initarg :key
    :initform nil
    :accessor docopt-key
    :documentation "The key of the Docopt object."))
  "A base class for Docopt objects that have a key slot.")

(defun docopt-assign-keys (objects keys)
  "Set the key slots of OBJECTS to the values in KEYS."
  (cl-mapcar (lambda (object key)
               (setf (oref object key) key)
               object)
             objects keys))

(provide 'docopt-key)

;;; docopt-key.el ends here
