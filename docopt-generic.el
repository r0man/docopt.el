;;; docopt-generic.el --- The Docopt generic functions -*- lexical-binding: t -*-

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

;; The Docopt generic functions

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defgeneric docopt-collect-arguments (object)
  "Collect the arguments from the Docopt OBJECT.")

(cl-defgeneric docopt-collect-commands (object)
  "Collect the arguments from the Docopt OBJECT.")

(cl-defgeneric docopt-collect-options (object)
  "Collect the options from the Docopt OBJECT.")

(cl-defgeneric docopt-equal (object-1 object-2)
  "Return t if OBJECT-1 and OBJECT-2 are equal-ish.")

(cl-defmethod docopt-equal (object-1 object-2)
  "Return t if OBJECT-1 and OBJECT-2 are equal-ish."
  (equal object-1 object-2))

(cl-defgeneric docopt-walk (object f)
  "Walk the OBJECT of an abstract syntax tree and apply F on it.")

(cl-defmethod docopt-walk ((lst list) f)
  "Walk the list LST of an abstract syntax tree and apply F on it."
  (seq-map (lambda (element) (docopt-walk element f)) lst))

(cl-defmethod docopt-walk ((s string) f)
  "Walk the string S of an abstract syntax tree and apply F on it."
  (funcall f s))

(provide 'docopt-generic)

;;; docopt-generic.el ends here
