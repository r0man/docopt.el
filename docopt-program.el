;;; docopt-program.el --- The Docopt program class -*- lexical-binding: t -*-

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

;; The Docopt program class

;;; Code:

(require 'docopt-option-line)
(require 'eieio)
(require 'seq)

(defclass docopt-program ()
  ((header
    :accessor docopt-program-header
    :documentation "The header of the program."
    :initarg :header
    :initform nil
    :type (or string null))
   (examples
    :accessor docopt-program-examples
    :documentation "The examples of the program."
    :initarg :examples
    :initform nil
    :type (or list null))
   (footer
    :accessor docopt-program-footer
    :documentation "The footer of the program."
    :initarg :footer
    :initform nil
    :type (or string null))
   (usage
    :accessor docopt-program-usage
    :documentation "The usage information of the program."
    :initarg :usage
    :initform nil
    :type (or list null))
   (options
    :accessor docopt-program-options
    :documentation "The options of the program."
    :initarg :options
    :initform nil
    :type (or list null)))
  "A class representing a Docopt program.")

(defun docopt-make-program (&rest args)
  "Make a new Docopt program using ARGS."
  (apply 'make-instance 'docopt-program args))

(defun docopt-program-options-list (program)
  "Return a list of long/short option pairs of the Docopt PROGRAM."
  (seq-map (lambda (option-line)
             (list (docopt-option-line-long-option option-line)
                   (docopt-option-line-short-option option-line)))
           (docopt-program-options program)))

(defun docopt-program-find-option-line (program object)
  "Find the option line in PROGRAM for the given OBJECT (description, long or short option)."
  (seq-find (lambda (option-line) (docopt-option-line-matches-p option-line object))
            (docopt-program-options program)))

(provide 'docopt-program)

;;; docopt-program.el ends here
