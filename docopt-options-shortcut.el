;;; docopt-options-shortcut.el --- The Docopt options shortcut class -*- lexical-binding: t -*-

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

;; The Docopt options shortcut class

;;; Code:

(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-either)
(require 'docopt-group)
(require 'docopt-option)
(require 'docopt-option-line)
(require 'docopt-program)
(require 'docopt-repeated)
(require 'docopt-standard-input)
(require 'docopt-usage-pattern)
(require 'eieio)
(require 'seq)

(defclass docopt-options-shortcut ()
  ((options
    :initarg :options
    :initform nil
    :accessor docopt-options-shortcut-options
    :documentation "The options of the options shortcut."))
  "A class representing a Docopt options shortcut.")

(defun docopt-make-options-shortcut (&rest options)
  "Make a new Docopt options shortcut using OPTIONS."
  (make-instance 'docopt-options-shortcut :options options))

(cl-defgeneric docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS.")

(cl-defmethod docopt-set-shortcut-options ((group docopt-group) options)
  "Set the options shortcut in GROUP to OPTIONS."
  (docopt-set-shortcut-options (docopt-group-members group) options))

(cl-defmethod docopt-set-shortcut-options ((either docopt-either) options)
  "Set the options shortcut in EITHER to OPTIONS."
  (docopt-set-shortcut-options (docopt-either-members either) options))

(cl-defmethod docopt-set-shortcut-options ((lst list) options)
  "Set the options shortcut of the elements in LST to OPTIONS."
  (seq-doseq (element lst) (docopt-set-shortcut-options element options)))

(cl-defmethod docopt-set-shortcut-options ((pattern docopt-usage-pattern) options)
  "Set the options shortcut in PATTERN to OPTIONS."
  (docopt-set-shortcut-options (docopt-usage-pattern-expressions pattern) options))

(cl-defmethod docopt-set-shortcut-options ((program docopt-program) options)
  "Set the options shortcut in PROGRAM to OPTIONS."
  (docopt-set-shortcut-options (docopt-program-usage program) options))

(cl-defmethod docopt-set-shortcut-options ((shortcut docopt-options-shortcut) options)
  "Set the options shortcut in SHORTCUT to OPTIONS."
  (oset shortcut :options options))

(cl-defmethod docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS.")

(provide 'docopt-options-shortcut)

;;; docopt-options-shortcut.el ends here
