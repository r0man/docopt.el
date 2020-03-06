;;; docopt-either.el --- The Docopt either class -*- lexical-binding: t -*-

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

;; The Docopt either class

;;; Code:

(require 'docopt-generic)
(require 'eieio)
(require 'seq)

(defclass docopt-either ()
  ((members
    :accessor docopt-either-members
    :documentation "The members of the either."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a Docopt either.")

(defun docopt-make-either (&rest members)
  "Make a new Docopt argument using MEMBERS and OPTIONAL."
  (make-instance 'docopt-either :members members))

(defun docopt-either-concat (&rest eithers)
  "Return a new either made of the concatenation of the members of EITHERS."
  (apply #'docopt-make-either (seq-mapcat #'docopt-either-members eithers)))

(cl-defmethod docopt-collect-arguments ((either docopt-either))
  "Collect the arguments from the Docopt EITHER."
  (delete-dups (seq-mapcat #'docopt-collect-arguments (docopt-either-members either))))

(cl-defmethod docopt-collect-commands ((either docopt-either))
  "Collect the commands from the Docopt EITHER."
  (delete-dups (seq-mapcat #'docopt-collect-commands (docopt-either-members either))))

(cl-defmethod docopt-collect-options ((either docopt-either))
  "Collect the options from the Docopt EITHER."
  (delete-dups (seq-mapcat #'docopt-collect-options (docopt-either-members either))))

(provide 'docopt-either)

;;; docopt-either.el ends here
