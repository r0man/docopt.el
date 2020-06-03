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
(require 's)
(require 'seq)

(cl-defmethod clone ((lst list))
  "Return a copy of LST."
  (copy-sequence lst))

(cl-defmethod clone ((v vector))
  "Return a copy of V."
  (copy-sequence v))

(cl-defmethod clone ((s string))
  "Return a copy of S."
  (copy-sequence s))

(cl-defgeneric docopt-shell-arguments (object)
  "Return the shell argument list for the OBJECT.")

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

(cl-defgeneric docopt-format (object)
  "Convert the Docopt OBJECT to a formatted string.")

(cl-defmethod docopt-format ((lst list))
  "Convert the list LST to a formatted string."
  (s-join " " (seq-map #'docopt-format lst)))

(cl-defmethod docopt-format ((object t))
  "Convert the Docopt OBJECT to a formatted string."
  (docopt-string object))

(cl-defgeneric docopt-name (object)
  "Return the name of OBJECT.")

(cl-defgeneric docopt-set-repeat (object value)
  "Set the :repeat slot of OBJECT to VALUE.")

(cl-defmethod docopt-set-repeat ((lst list) value)
  "Set the :repeat slot of the LST elements to VALUE."
  (seq-map (lambda (element) (docopt-set-repeat element value)) lst))

(cl-defgeneric docopt-string (object)
  "Convert the Docopt OBJECT to a string.")

(cl-defmethod docopt-string ((lst list))
  "Convert the list LST to a string."
  (s-join " " (seq-map #'docopt-string lst)))

(cl-defgeneric docopt-walk (object f)
  "Walk the OBJECT of an abstract syntax tree and apply F on it.")

(cl-defmethod docopt-walk ((lst list) f)
  "Walk the list LST of an abstract syntax tree and apply F on it."
  (funcall f (seq-map (lambda (element) (docopt-walk element f)) lst)))

(cl-defmethod docopt-walk ((s string) f)
  "Walk the string S of an abstract syntax tree and apply F on it."
  (funcall f s))

(cl-defmethod docopt-walk ((v vector) f)
  "Walk the vector V of an abstract syntax tree and apply F on it."
  (funcall f v))

(provide 'docopt-generic)

;;; docopt-generic.el ends here
