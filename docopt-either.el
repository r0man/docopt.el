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
(require 'docopt-optional)
(require 'docopt-value)
(require 'eieio)
(require 's)
(require 'seq)

(defclass docopt-either (docopt-optionable docopt-value-base)
  ((members
    :accessor docopt-either-members
    :documentation "The members of the either."
    :initarg :members
    :initform nil
    :type (or list null)))
  "A class representing a Docopt either.")

(cl-defmethod clone ((either docopt-either) &rest params)
  "Return a copy of EITHER and apply PARAMS."
  (let ((copy (apply #'cl-call-next-method either params)))
    (with-slots (members) copy
      (setq members (clone (docopt-either-members either)))
      copy)))

(cl-defmethod docopt-shell-arguments ((either docopt-either))
  "Return the shell argument list for the EITHER."
  (thread-last (oref either members)
    (seq-map (lambda (members) (seq-mapcat #'docopt-shell-arguments members)))
    (seq-remove #'null)
    (car)))

(cl-defmethod docopt-format ((either docopt-either))
  "Convert the Docopt EITHER to a formatted string."
  (with-slots (members) either
    (s-join " | " (seq-map #'docopt-format members))))

(cl-defmethod docopt-set-repeat ((either docopt-either) value)
  "Set the :repeat slot of the EITHER members to VALUE."
  (docopt-set-repeat (docopt-either-members either) value)
  either)

(cl-defmethod docopt-string ((either docopt-either))
  "Convert the Docopt EITHER to a string."
  (with-slots (members) either
    (s-join " | " (seq-map #'docopt-string members))))

(defun docopt-make-either (&rest members)
  "Make a new Docopt argument using MEMBERS and OPTIONAL."
  (make-instance 'docopt-either :members members))

(defun docopt-either-concat (&rest eithers)
  "Return a new either made of the concatenation of the members of EITHERS."
  (apply #'docopt-make-either (seq-mapcat #'docopt-either-members eithers)))

(defun docopt-either-all-type-p (either type)
  "Return t if all members of EITHER have a length of 1 and are of TYPE."
  (and (cl-typep either 'docopt-either)
       (cl-every (lambda (members)
                   (and (= 1 (length members))
                        (cl-every (lambda (member)
                                    (cl-typep member type))
                                  members)))
                 (docopt-either-members either))))

(cl-defmethod docopt-collect-arguments ((either docopt-either))
  "Collect the arguments from the Docopt EITHER."
  (seq-mapcat #'docopt-collect-arguments (docopt-either-members either)))

(cl-defmethod docopt-collect-commands ((either docopt-either))
  "Collect the commands from the Docopt EITHER."
  (seq-mapcat #'docopt-collect-commands (docopt-either-members either)))

(cl-defmethod docopt-collect-options ((either docopt-either))
  "Collect the options from the Docopt EITHER."
  (seq-mapcat #'docopt-collect-options (docopt-either-members either)))

(cl-defmethod docopt-walk ((either docopt-either) f)
  "Walk the EITHER of an abstract syntax tree and apply F on it."
  (with-slots (members) either
    (setq members (docopt-walk members f))
    (funcall f either)))

(provide 'docopt-either)

;;; docopt-either.el ends here
