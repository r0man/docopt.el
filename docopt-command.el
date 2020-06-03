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

(require 'dash)
(require 'docopt-key)
(require 'docopt-generic)
(require 'docopt-optional)
(require 'docopt-repeated)
(require 'docopt-value)
(require 'eieio)
(require 'eieio-base)

(defclass docopt-command
  (docopt-key-base docopt-optionable docopt-repeatable docopt-value-base)
  ((name
    :accessor docopt-command-name
    :documentation "The name of the command."
    :initarg :name
    :initform nil
    :type (or string null))
   (incompatible
    :accessor docopt-command-incompatible
    :documentation "The list of incompatible commands."
    :initarg :incompatible
    :initform nil
    :type (or list null)))
  "A class representing a Docopt command.")

(cl-defmethod clone ((command docopt-command) &rest params)
  "Return a copy of the COMMAND and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method command params)))
    (with-slots (name optional) copy
      (setq name (clone (docopt-command-name command)))
      copy)))

(cl-defmethod docopt-shell-arguments ((command docopt-command))
  "Return the shell argument list for the COMMAND."
  (with-slots (value) command
    (when value (list (docopt-command-name command)))))

(cl-defmethod docopt-collect-arguments ((_ docopt-command))
  "Collect the arguments from the Docopt COMMAND." nil)

(cl-defmethod docopt-collect-commands ((command docopt-command))
  "Collect the commands from the Docopt COMMAND." command)

(cl-defmethod docopt-collect-commands ((lst list))
  "Collect the commands from the list LST."
  (-flatten (seq-map #'docopt-collect-commands lst)))

(cl-defmethod docopt-collect-options ((_ docopt-command))
  "Collect the options from the Docopt COMMAND." nil)

(cl-defmethod docopt-format ((command docopt-command))
  "Convert the Docopt usage COMMAND to a formatted string."
  (let ((s (docopt-string command)))
    (if (docopt-value command) (docopt-bold s) s)))

(cl-defmethod docopt-name ((command docopt-command))
  "Return the name of COMMAND."
  (docopt-command-name command))

(cl-defmethod docopt-string ((command docopt-command))
  "Convert the Docopt usage COMMAND to a string."
  (docopt-command-name command))

(cl-defmethod docopt-walk ((command docopt-command) f)
  "Walk the COMMAND of an abstract syntax tree and apply F on it."
  (with-slots (name) command
    (setq name (docopt-walk name f))
    (funcall f command)))

(provide 'docopt-command)

;;; docopt-command.el ends here
