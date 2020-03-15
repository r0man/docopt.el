;;; docopt.el --- A Docopt implementation in Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, tools, processes
;; Homepage: https://github.com/r0man/docopt.el
;; Version: 0.1.0

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

;; A Docopt implementation in Elisp

;;; Code:

(require 'docopt-argv)
(require 'docopt-parser)
(require 'parsec)
(require 's)

;;;###autoload
(defun docopt-parse (s)
  "Parse the Docopt program from S."
  (let ((program (parsec-with-input s (docopt--parse-program))))
    (oset program :source (s-trim s))
    program))

;;;###autoload
(defun docopt-eval-ast (program argv)
  "Parse the Docopt command line argument vector ARGV with PROGRAM."
  (docopt-argv-parse program argv))

;;;###autoload
(defun docopt-eval (program argv)
  "Evaluate the Docopt command line argument vector ARGV with PROGRAM."
  (docopt-argv-eval program argv))

(provide 'docopt)

;;; docopt.el ends here
