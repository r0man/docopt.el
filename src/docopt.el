;;; docopt.el --- A Docopt implementation in Elisp -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Keywords: docopt, tools, processes
;; Package-Requires: ((emacs "26.3") (dash "2.17.0") (emacs "26.1") (f "0.20.0") (let-alist "1.0.6") (parsec "0.1.3") (s "1.12.0") (transient "0.3.0"))
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

(require 'docopt-analyzer)
(require 'docopt-argv)
(require 'docopt-parser)
(require 'docopt-transient)
(require 'docopt-util)
(require 'parsec)
(require 's)

(define-error 'docopt-invalid-program
  "Invalid Docopt program.")

;;;###autoload
(defun docopt-parse (s)
  "Parse the program from S."
  (let ((program (make-instance 'docopt-program :source s))
        (raw-sections (parsec-with-input s (docopt-parser--raw-sections))))
    (when (docopt--parsec-error-p raw-sections)
      (signal 'docopt-invalid-program program))
    (seq-doseq (section raw-sections)
      (when (equal :options (car section))
        (docopt-parser--section program (car section) (cadr section))))
    (seq-doseq (section raw-sections)
      (unless (equal :options (car section))
        (docopt-parser--section program (car section) (cadr section))))
    (docopt-analyze-program program)))

;;;###autoload
(defun docopt-shell-command (command)
  "Run the shell COMMAND with the --help option and parse the result as a program."
  (let ((program (docopt-parse (shell-command-to-string (concat command " --help")))))
    (setf (oref program name) command)
    program))

;;;###autoload
(defun docopt-eval-ast (program s)
  "Parse the argument vector from S using the PROGRAM."
  (docopt-argv-parse program s))

;;;###autoload
(defun docopt-eval (program s)
  "Parse the argument vector from S using the PROGRAM."
  (docopt--argv-to-alist program (docopt-argv-parse program s)))

;;;###autoload
(defun docopt (command)
  "Invoke the transient command for the shell COMMAND."
  (interactive (list (read-from-minibuffer "Docopt: ")))
  (docopt-transient (docopt-shell-command command)))

(provide 'docopt)

;;; docopt.el ends here
