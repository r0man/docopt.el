;;; docopt-optional.el --- The Docopt optional class -*- lexical-binding: t -*-

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

;; The Docopt optional class

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'seq)

(defclass docopt-optionable ()
  ((optional
    :initarg :optional
    :initform nil
    :accessor docopt-optional-p
    :documentation "Whether the object is optional or not."))
  "A class representing a optional Docopt object.")

(cl-defgeneric docopt-set-optional (object optional)
  "Set the :optional slot of OBJECT to OPTIONAL.")

(cl-defmethod docopt-set-optional ((lst list) optional)
  "Set the :optional slot of the elements of LST to OPTIONAL."
  (seq-doseq (element lst) (docopt-set-optional element optional)))

(cl-defmethod docopt-set-optional ((object docopt-optionable) optional)
  "Set the :optional slot of OBJECT to OPTIONAL."
  (oset object :optional optional))

(cl-defmethod docopt-set-optional ((object t) optional)
  "Set the :optional slot of OBJECT to OPTIONAL." nil)

(provide 'docopt-optional)

;;; docopt-optional.el ends here
