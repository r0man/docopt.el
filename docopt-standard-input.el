;;; docopt-standard-input.el --- Docopt standard input -*- lexical-binding: t -*-

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

;; The Docopt standard input class

;;; Code:

(require 'docopt-generic)
(require 'eieio)

(defclass docopt-standard-input () ()
  "A class representing the Docopt standard input.")

(cl-defmethod docopt-collect-arguments ((input docopt-standard-input))
  "Collect the arguments from the Docopt standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-collect-commands ((input docopt-standard-input))
  "Collect the commands from the Docopt standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-collect-options ((input docopt-standard-input))
  "Collect the options from the Docopt standard INPUT."
  (ignore input) nil)

(cl-defmethod docopt-name ((input docopt-standard-input))
  "Return the name of standard INPUT."
  (ignore input) "-")

(cl-defmethod docopt-string ((input docopt-standard-input))
  "Convert the Docopt options standard INPUT to a string."
  (ignore input)
  "[-]")

(cl-defmethod docopt-walk ((input docopt-standard-input) f)
  "Walk the standard INPUT of an abstract syntax tree and apply F on it."
  (funcall f input))

(provide 'docopt-standard-input)

;;; docopt-standard-input.el ends here
