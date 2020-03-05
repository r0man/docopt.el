;;; docopt-option-line.el --- The Docopt option line class -*- lexical-binding: t -*-

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

;; The Docopt option line class

;;; Code:

(require 'docopt-argument)
(require 'docopt-option)
(require 'eieio)

(defclass docopt-option-line ()
  ((description
    :accessor docopt-option-line-description
    :documentation "The description of the option-line."
    :initarg :description
    :initform nil
    :type (or string null))
   (long-option
    :accessor docopt-option-line-long-option
    :documentation "The long name of the option line."
    :initarg :long-option
    :initform nil
    :type (or docopt-long-option null))
   (short-option
    :accessor docopt-option-line-short-option
    :documentation "The short name of the option line."
    :initarg :short-option
    :initform nil
    :type (or docopt-short-option null)))
  "A class representing a Docopt option line.")

(cl-defun docopt-make-option-line (&key description long-name short-name argument argument-name)
  "Make a new Docopt option line instance.
Initialize the DESCRIPTION, LONG-NAME, SHORT-NAME, ARGUMENT and ARGUMENT-NAME
slots of the instance."
  (let ((argument (cond
                   ((and argument
                         (object-of-class-p argument 'docopt-argument)) argument)
                   (argument-name (docopt-argument :object-name argument-name)))))
    (docopt-option-line
     :description description
     :long-option (when long-name
                    (docopt-long-option
                     long-name
                     :argument argument
                     :description description))
     :short-option (when short-name
                     (docopt-short-option
                      short-name
                      :argument argument
                      :description description)))))

(cl-defgeneric docopt-option-line-matches-p (option-line object)
  "Return t if OBJECT does match OPTION-LINE.")

(cl-defmethod docopt-option-line-matches-p (option-line (description string))
  "Return t if the DESCRIPTION does match with the one in OPTION-LINE."
  (equal (docopt-option-line-description option-line) description))

(cl-defmethod docopt-option-line-matches-p (option-line (option docopt-long-option))
  "Return t if the long OPTION does match with the one in OPTION-LINE."
  (when-let ((long-option (docopt-option-line-long-option option-line)))
    (equal (oref long-option object-name)
           (oref option object-name))))

(cl-defmethod docopt-option-line-matches-p (option-line (option docopt-short-option))
  "Return t if the short OPTION does match with the one in OPTION-LINE."
  (when-let ((short-option (docopt-option-line-short-option option-line)))
    (equal (oref short-option object-name)
           (oref option object-name))))

(provide 'docopt-option-line)

;;; docopt-option-line.el ends here
