;;; docopt-options-shortcut.el --- Docopt options shortcut -*- lexical-binding: t -*-

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

;; The Docopt options shortcut class

;;; Code:

(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-either)
(require 'docopt-group)
(require 'docopt-option)
(require 'docopt-program)
(require 'docopt-repeated)
(require 'docopt-standard-input)
(require 'docopt-usage-pattern)
(require 'eieio)
(require 'seq)

(defclass docopt-options-shortcut ()
  ((options
    :accessor docopt-options-shortcut-options
    :documentation "The options of the options shortcut."
    :initarg :options
    :initform nil
    :type (or list null)))
  "A class representing a options shortcut.")

(cl-defmethod docopt-shell-arguments ((shortcut docopt-options-shortcut))
  "Return the shell argument list for the options SHORTCUT."
  (with-slots (options) shortcut
    (thread-last options
      (docopt-option-remove-synonyms)
      (seq-mapcat #'docopt-shell-arguments)
      (seq-remove #'null))))

(cl-defmethod docopt-collect-arguments ((shortcut docopt-options-shortcut))
  "Collect the arguments from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-collect-commands ((shortcut docopt-options-shortcut))
  "Collect the commands from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-collect-options ((shortcut docopt-options-shortcut))
  "Collect the options from the SHORTCUT."
  (ignore shortcut) nil)

(cl-defmethod docopt-format ((shortcut docopt-options-shortcut))
  "Convert the options SHORTCUT to a formatted string."
  (with-slots (options) shortcut
    (thread-last options
      (docopt-option-remove-synonyms)
      (seq-filter (lambda (option)
                    (if-let ((argument (oref option argument)))
                        (not (null (docopt-value argument)))
                      (not (null (docopt-value option))))))
      (seq-map #'docopt-format)
      (seq-remove #'null)
      (s-join " "))))

(cl-defmethod docopt-string ((shortcut docopt-options-shortcut))
  "Convert the options SHORTCUT to a string."
  (ignore shortcut)
  "[options]")

(cl-defmethod docopt-walk ((shortcut docopt-options-shortcut) f)
  "Walk the SHORTCUT of an abstract syntax tree and apply F on it."
  (with-slots (options) shortcut
    (setq options (docopt-walk options f))
    (funcall f shortcut)))

(defun docopt-make-options-shortcut (&rest options)
  "Make a new options shortcut using OPTIONS."
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
  (setf (oref shortcut :options) options))

(cl-defmethod docopt-set-shortcut-options (object options)
  "Set the options shortcut in OBJECT to OPTIONS."
  (ignore object options))

(provide 'docopt-options-shortcut)

;;; docopt-options-shortcut.el ends here
