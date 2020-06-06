;;; docopt-util.el --- The Docopt utility functions -*- lexical-binding: t -*-

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

(provide 'docopt-util)

;;; docopt-util.el ends here