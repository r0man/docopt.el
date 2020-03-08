;;; docopt-standard-input.el --- The Docopt standard input class -*- lexical-binding: t -*-

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

;; The Docopt standard input class

;;; Code:

(require 'docopt-generic)
(require 'eieio)

(defclass docopt-standard-input () ()
  "A class representing the Docopt standard input.")

(cl-defmethod docopt-collect-arguments ((_ docopt-standard-input))
  "Collect the arguments from the Docopt STANDARD-INPUT." nil)

(cl-defmethod docopt-collect-commands ((standard-input docopt-standard-input))
  "Collect the commands from the Docopt STANDARD-INPUT."
  standard-input)

(cl-defmethod docopt-collect-options ((_ docopt-standard-input))
  "Collect the options from the Docopt STANDARD-INPUT." nil)

(cl-defmethod docopt-walk ((standard-input docopt-standard-input) f)
  "Walk the STANDARD-INPUT of an abstract syntax tree and apply F on it."
  (funcall f (copy-sequence standard-input)))

(provide 'docopt-standard-input)

;;; docopt-standard-input.el ends here
