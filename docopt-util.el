;;; docopt-util.el --- Docopt utility functions -*- lexical-binding: t -*-

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

;; The Docopt utility functions

;;; Code:

(require 'docopt-generic)
(require 'cl-lib)
(require 's)

(defun docopt-bold (text)
  "Add the `bold' face property to TEXT."
  (when text (propertize (format "%s" text) 'face 'bold)))

(defun docopt-strip (s)
  "Trim S, return nil if only the empty string is left."
  (let ((s (s-trim s)))
    (unless (s-blank-p s) s)))

(defun docopt-substring (s from to)
  "Return the substring in S from FROM to TO, or nil when out of bounds."
  (substring s (min from (length s)) (min to (length s))))

(defun docopt--parsec-error-p (result)
  "Return t if the car of RESULT is a 'parsec-error."
  (and (sequencep result) (equal 'parsec-error (car result))))

(defun docopt-remove-duplicates (lst)
  "Remove duplicate Docopt objects from LST."
  (cl-remove-duplicates lst :test #'docopt-equal))

(defun docopt-by-type (object type)
  "Find all elements in OBJECT with the given TYPE."
  (let ((objects nil))
    (docopt-walk object
                 (lambda (element)
                   (when (cl-typep element type)
                     (setq objects (cons element objects)))
                   element))
    (docopt-remove-duplicates objects)))

(defun docopt-find (object element)
  "Find ELEMENT in OBJECT."
  (let ((objects nil))
    (docopt-walk object
                 (lambda (object)
                   (when (docopt-equal element object)
                     (setq objects (cons object objects)))
                   object))
    (car objects)))

(provide 'docopt-util)

;;; docopt-util.el ends here
