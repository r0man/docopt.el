;;; docopt-command.el --- The Docopt command class -*- lexical-binding: t -*-

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

;; The Docopt command class

;;; Code:

(require 'docopt-generic)
(require 'docopt-optional)
(require 'eieio)
(require 'eieio-base)

(defclass docopt-command (docopt-optionable eieio-named) ()
  "A class representing a Docopt command.")

(cl-defmethod docopt-collect-arguments ((_ docopt-command))
  "Collect the arguments from the Docopt COMMAND." nil)

(cl-defmethod docopt-collect-commands ((command docopt-command))
  "Collect the commands from the Docopt COMMAND." command)

(cl-defmethod docopt-collect-commands ((lst list))
  "Collect the commands from the list LST."
  (docopt--flatten (seq-map #'docopt-collect-commands lst)))

(cl-defmethod docopt-collect-options ((_ docopt-command))
  "Collect the options from the Docopt COMMAND." nil)

(cl-defmethod docopt-copy ((command docopt-command))
  "Return a copy of the COMMAND."
  (let ((copy (copy-sequence command)))
    (with-slots (object-name optional) copy
      (setq object-name (docopt-copy (object-name-string command)))
      (setq optional (docopt-copy (docopt-command-optional command)))
      copy)))

(cl-defmethod docopt-walk ((command docopt-command) f)
  "Walk the COMMAND of an abstract syntax tree and apply F on it."
  (let ((command (copy-sequence command)))
    (with-slots (object-name) command
      (setq object-name (docopt-walk object-name f))
      (funcall f command))))

(provide 'docopt-command)

;;; docopt-command.el ends here
