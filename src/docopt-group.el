;;; docopt-group.el --- Docopt group -*- lexical-binding: t -*-

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

;; The Docopt group class

;;; Code:

(require 'docopt-generic)
(require 'docopt-optional)
(require 'eieio)
(require 's)
(require 'seq)

(defclass docopt-group ()
  ((members
    :accessor docopt-group-members
    :documentation "The argument of the option."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a Docopt group.")

(cl-defmethod clone ((group docopt-group) &rest params)
  "Return a copy of the GROUP and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method group params)))
    (setf (oref copy :members) (clone (docopt-group-members group)))
    copy))

(cl-defmethod docopt-shell-arguments ((group docopt-group))
  "Return the shell argument list for the GROUP."
  (with-slots (members) group
    (seq-mapcat #'docopt-shell-arguments members)))

(cl-defmethod docopt-set-repeat ((group docopt-group) value)
  "Set the :repeat slot of the GROUP members to VALUE."
  (docopt-set-repeat (docopt-group-members group) value)
  group)

(cl-defmethod docopt-walk ((group docopt-group) f)
  "Walk the GROUP of an abstract syntax tree and apply F on it."
  (with-slots (members) group
    (setq members (docopt-walk members f))
    (funcall f group)))

;;; Optional Group

(defclass docopt-optional-group (docopt-group docopt-optionable) ()
  "A class representing a required Docopt group.")

(defun docopt-make-optional-group (&rest members)
  "Make a new optional Docopt group with MEMBERS."
  (let ((group (docopt-optional-group :members members)))
    (docopt-set-optional group t)
    group))

(cl-defmethod docopt-set-optional ((group docopt-optional-group) optional)
  "Set the :optional slot of the GROUP members to OPTIONAL."
  (cl-call-next-method group optional)
  (docopt-set-optional (docopt-group-members group) optional))

(cl-defmethod docopt-format ((group docopt-optional-group))
  "Convert the Docopt usage GROUP to a formatted string."
  (with-slots (members) group
    (concat "[" (s-join " " (seq-map #'docopt-format members)) "]")))

(cl-defmethod docopt-string ((group docopt-optional-group))
  "Convert the Docopt usage GROUP to a string."
  (with-slots (members) group
    (concat "[" (s-join " " (seq-map #'docopt-string members)) "]")))

;;; Required Group

(defclass docopt-required-group (docopt-group) ()
  "A class representing a required Docopt group.")

(defun docopt-make-required-group (&rest members)
  "Make a new required Docopt group with MEMBERS."
  (make-instance 'docopt-required-group :members members))

(cl-defmethod docopt-collect-arguments ((group docopt-group))
  "Collect the arguments from the Docopt GROUP."
  (docopt-collect-arguments (docopt-group-members group)))

(cl-defmethod docopt-collect-commands ((group docopt-group))
  "Collect the commands from the Docopt GROUP."
  (docopt-collect-commands (docopt-group-members group)))

(cl-defmethod docopt-collect-options ((group docopt-group))
  "Collect the options from the Docopt GROUP."
  (docopt-collect-options (docopt-group-members group)))

(cl-defmethod docopt-format ((group docopt-required-group))
  "Convert the Docopt required GROUP to a formatted string."
  (with-slots (members) group
    (concat "(" (s-join " " (seq-map #'docopt-format members)) ")")))

(cl-defmethod docopt-string ((group docopt-required-group))
  "Convert the Docopt required GROUP to a string."
  (with-slots (members) group
    (concat "(" (s-join " " (seq-map #'docopt-string members)) ")")))

(provide 'docopt-group)

;;; docopt-group.el ends here
