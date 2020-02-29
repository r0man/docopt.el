;;; docopt.el --- The Docopt Emacs mode -*- lexical-binding: t -*-

;; URL: https://github.com/r0man/docopt.el
;; Keywords: docopt
;; Version: 0.1.0

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, command line argument
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

;; Docopt for Elisp

;;; Code:

(require 'docopt-argv)
(require 'docopt-classes)
(require 'docopt-parser)
(require 'parsec)

;;;###autoload
(defun docopt-parse-program (s)
  "Parse the Docopt program S."
  (parsec-with-input s (docopt--parse-program)))

;;;###autoload
(defun doctopt-parse-argv (program s)
  "Parse the argument vector S of the Docopt PROGRAM."
  (parsec-with-input s (docopt-argument-parser program)))

(provide 'docopt)

;;; docopt.el ends here
