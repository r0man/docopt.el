;;; docopt-string.el --- The Docopt string functions -*- lexical-binding: t -*-

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

;; The Docopt string functions

;;; Code:

(require 'cl-lib)
(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-either)
(require 'docopt-group)
(require 'docopt-option)
(require 'docopt-option-line)
(require 'docopt-options-shortcut)
(require 'docopt-repeated)
(require 'docopt-standard-input)
(require 'docopt-usage-pattern)
(require 's)

(defcustom docopt-string-options-width 20
  "The width of the options on a Docopt options line."
  :type 'number
  :group 'docopt)

(cl-defgeneric docopt-string (object)
  "Convert the Docopt OBJECT to a string.")

(defun docopt-string--example (example)
  "Convert the Docopt EXAMPLE to a string."
  (string-join example " "))

(defun docopt-string--join (separator elements)
  "Apply `docopt-string' on ELEMENTS and join them with SEPARATOR."
  (string-join (seq-map #'docopt-string elements) separator))

(defun docopt-string--section (header content)
  "Convert the Docopt section HEADER and CONTENT to a string."
  (concat header ":\n  " (string-join content "\n  ")))

(defun docopt-string--examples (examples)
  "Convert the Docopt EXAMPLES to a string."
  (docopt-string--section "Examples" (seq-map #'docopt-string--example examples)))

(defun docopt-string--usage (usage)
  "Convert the Docopt USAGE to a string."
  (docopt-string--section "Usage" (seq-map #'docopt-string usage)))

(defun docopt-string--option-argument (option)
  "Convert the Docopt OPTION argument to a string."
  (when-let ((argument (docopt-option-argument option)))
    (concat "=" (docopt-string argument))))

(defun docopt-string--options (options)
  "Convert the Docopt OPTIONS to a string."
  (docopt-string--section "Options" (seq-map #'docopt-string options)))

(defun docopt-string--option-line-options (option-line)
  "Convert the options of the OPTION-LINE to a string."
  (thread-last (list (docopt-option-line-short-option option-line)
                     (docopt-option-line-long-option option-line))
    (seq-remove #'null)
    (seq-map #'docopt-string)
    (s-join ", ")))

(cl-defmethod docopt-string ((argument docopt-argument))
  "Convert the Docopt usage ARGUMENT to a string."
  (concat "<" (oref argument object-name) ">"))

(cl-defmethod docopt-string ((command docopt-command))
  "Convert the Docopt usage COMMAND to a string."
  (oref command object-name))

(cl-defmethod docopt-string ((either docopt-either))
  "Convert the Docopt EITHER to a string."
  (docopt-string--join " | " (docopt-either-members either)))

(cl-defmethod docopt-string ((lst list))
  "Convert the list LST to a string."
  (docopt-string--join " " lst))

(cl-defmethod docopt-string ((option docopt-long-option))
  "Convert the Docopt long OPTION to a string."
  (concat "--" (oref option object-name) (docopt-string--option-argument option)))

(cl-defmethod docopt-string ((option docopt-short-option))
  "Convert the Docopt short OPTION to a string."
  (concat "-" (oref option object-name) (docopt-string--option-argument option)))

(cl-defmethod docopt-string ((group docopt-optional-group))
  "Convert the Docopt usage GROUP to a string."
  (concat "[" (docopt-string--join " " (docopt-group-members group)) "]"))

(cl-defmethod docopt-string ((program docopt-program))
  "Convert the Docopt PROGRAM to a string."
  (thread-last (list (docopt-program-header program)
                     (docopt-string--usage (docopt-program-usage program))
                     (docopt-string--options (docopt-program-options program))
                     (docopt-string--examples (docopt-program-examples program)))
    (seq-remove #'s-blank-p)
    (s-join "\n\n")))

(cl-defmethod docopt-string ((repeated docopt-repeated))
  "Convert the Docopt usage REPEATED to a string."
  (concat (docopt-string (docopt-repeated-object repeated)) "..."))

(cl-defmethod docopt-string ((group docopt-required-group))
  "Convert the Docopt required GROUP to a string."
  (concat "(" (docopt-string--join " " (docopt-group-members group)) ")"))

(cl-defmethod docopt-string ((line docopt-option-line))
  "Convert the Docopt option LINE to a string."
  (format (concat "%-" (number-to-string docopt-string-options-width) "s %s")
          (docopt-string--option-line-options line)
          (docopt-option-line-description line)))

(cl-defmethod docopt-string ((shortcut docopt-options-shortcut))
  "Convert the Docopt options SHORTCUT to a string."
  "[options]")

(cl-defmethod docopt-string ((shortcut docopt-standard-input))
  "Convert the Docopt options SHORTCUT to a string."
  "[-]")

(cl-defmethod docopt-string ((pattern docopt-usage-pattern))
  "Convert the Docopt usage PATTERN to a string."
  (concat (docopt-string (docopt-usage-pattern-command pattern)) " "
          (docopt-string--join " " (docopt-usage-pattern-expressions pattern))))

(provide 'docopt-string)

;;; docopt-string.el ends here
