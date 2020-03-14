;;; docopt-argument.el --- The Docopt argument class -*- lexical-binding: t -*-

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

;; The Docopt argument class

;;; Code:

(require 'docopt-generic)
(require 'docopt-repeated)
(require 'docopt-optional)
(require 'eieio)
(require 'eieio-base)

(defclass docopt-argument (docopt-repeatable docopt-optionable eieio-named)
  ((default
     :accessor docopt-argument-default
     :documentation "The default of the argument."
     :initarg :default
     :initform nil
     :type (or string null))
   (value
    :accessor docopt-argument-value
    :documentation "The value of the argument."
    :initarg :value
    :initform nil
    :type (or string null)))
  "A class representing a Docopt argument.")

(cl-defmethod docopt-copy ((argument docopt-argument))
  "Make a copy of ARGUMENT."
  (with-slots (default value) argument
    (let ((copy (copy-sequence argument)))
      (oset copy :default (docopt-copy default))
      (oset copy :value (docopt-copy value))
      copy)))

(cl-defmethod docopt-equal ((argument docopt-argument) object)
  "Return t if ARGUMENT and OBJECT are equal-ish."
  (and (docopt-argument-p object)
       (string= (eieio-object-name-string argument)
                (eieio-object-name-string object))))

(cl-defmethod docopt-collect-arguments ((argument docopt-argument))
  "Collect the arguments from the Docopt ARGUMENT."
  (list argument))

(cl-defmethod docopt-collect-arguments ((lst list))
  "Collect the arguments from the list LST."
  (docopt--flatten (seq-map #'docopt-collect-arguments lst)))

(cl-defmethod docopt-collect-commands ((argument docopt-argument))
  "Collect the commands from the Docopt ARGUMENT." nil)

(cl-defmethod docopt-collect-options ((_ docopt-argument))
  "Collect the options from the Docopt OPTION." nil)

(cl-defmethod docopt-walk ((argument docopt-argument) f)
  "Walk the ARGUMENT of an abstract syntax tree and apply F on it."
  (let ((argument (copy-sequence argument)))
    (with-slots (default object-name value) argument
      (setq default (docopt-walk default f))
      (setq object-name (docopt-walk object-name f))
      (setq value (docopt-walk value f))
      (funcall f argument))))

(defun docopt-argument-merge (argument-1 argument-2)
  "Merge ARGUMENT-2 into ARGUMENT-1."
  (cond
   ((and argument-1 argument-2)
    (with-slots (default object-name value) argument-1
      (setq default (or default (oref argument-2 :default)))
      (setq value (or value (oref argument-2 :value)))
      (setq object-name (or object-name (oref argument-2 :object-name)))
      argument-1))
   (argument-1 argument-1)
   (argument-2 argument-2)))

(provide 'docopt-argument)

;;; docopt-argument.el ends here
