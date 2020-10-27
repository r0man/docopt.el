;;; docopt-argument.el --- Docopt argument -*- lexical-binding: t -*-

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

;; The Docopt argument class

;;; Code:

(require 'dash)
(require 'docopt-generic)
(require 'docopt-key)
(require 'docopt-optional)
(require 'docopt-repeated)
(require 'docopt-util)
(require 'docopt-value)
(require 'eieio)
(require 'eieio-base)

(defclass docopt-argument
  (docopt-key-base docopt-optionable docopt-repeatable docopt-value-base)
  ((default
     :accessor docopt-argument-default
     :documentation "The default of the argument."
     :initarg :default
     :initform nil
     :type (or string vector null))
   (name
    :accessor docopt-argument-name
    :documentation "The name of the argument."
    :initarg :name
    :initform nil
    :type (or string null)))
  "A class representing a Docopt argument.")

(cl-defmethod clone ((argument docopt-argument) &rest params)
  "Return a copy of the ARGUMENT and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method argument params)))
    (with-slots (default value name) copy
      (setq default (clone (docopt-argument-default argument)))
      (setq name (clone (docopt-argument-name argument)))
      (setq value (clone (docopt-value argument)))
      copy)))

(cl-defmethod docopt-shell-arguments ((argument docopt-argument))
  "Return the shell argument list for the ARGUMENT."
  (with-slots (value) argument
    (when value (list value))))

(cl-defmethod docopt-equal ((argument docopt-argument) object)
  "Return t if ARGUMENT and OBJECT are equal-ish."
  (and (docopt-argument-p object)
       (string= (docopt-argument-name argument)
                (docopt-argument-name object))))

(cl-defmethod docopt-collect-arguments ((argument docopt-argument))
  "Collect the arguments from the Docopt ARGUMENT."
  (list argument))

(cl-defmethod docopt-collect-arguments ((lst list))
  "Collect the arguments from the list LST."
  (-flatten (seq-map #'docopt-collect-arguments lst)))

(cl-defmethod docopt-format ((argument docopt-argument))
  "Convert the Docopt usage ARGUMENT to a formatted string."
  (with-slots (name value) argument
    (concat "<" (cond
                 (value (docopt-bold value))
                 ((s-uppercase? name) name)
                 (t name)) ">")))

(cl-defmethod docopt-name ((argument docopt-argument))
  "Return the name of ARGUMENT."
  (docopt-argument-name argument))

(cl-defmethod docopt-string ((argument docopt-argument))
  "Convert the Docopt usage ARGUMENT to a string."
  (let ((name (docopt-argument-name argument)))
    (if (s-uppercase? name) name (concat "<" name ">"))))

(cl-defmethod docopt-walk ((argument docopt-argument) f)
  "Walk the ARGUMENT of an abstract syntax tree and apply F on it."
  (with-slots (default name value) argument
    (setq default (docopt-walk default f))
    (setq name (docopt-walk name f))
    (setq value (docopt-walk value f))
    (funcall f argument)))

(defun docopt-argument-merge (argument-1 argument-2)
  "Merge ARGUMENT-2 into ARGUMENT-1."
  (cond
   ((and argument-1 argument-2)
    (with-slots (default name value) argument-1
      (setq default (or default (docopt-argument-default argument-2)))
      (setq value (or value (docopt-value argument-2)))
      (setq name (or name (docopt-argument-name argument-2)))
      argument-1))
   (argument-1 argument-1)
   (argument-2 argument-2)))

(provide 'docopt-argument)

;;; docopt-argument.el ends here
