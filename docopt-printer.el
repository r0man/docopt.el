;;; docopt-printer.el --- The Docopt printer -*- lexical-binding: t -*-

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

;; The Docopt printer

;;; Code:

(require 'cl-lib)
(require 'docopt-classes)
(require 's)

(defun docopt-to-string--example (example)
  "Convert the Docopt EXAMPLE to a string."
  (string-join example " "))

(defun docopt-to-string--examples (examples)
  "Convert the Docopt EXAMPLES to a string."
  (concat "Examples:\n  " (string-join (seq-map #'docopt-to-string--example examples) "\n  ")))

(defun docopt-to-string--usage (usage)
  "Convert the Docopt USAGE to a string."
  (concat "Usage:\n  " (string-join (seq-map #'docopt-to-string usage) "\n  ")))

(defun docopt-to-string--options (options)
  "Convert the Docopt OPTIONS to a string."
  (concat "Options:\n  " (string-join (seq-map #'docopt-to-string options) "\n  ")))

(defun docopt-to-string--option-line-options (option-line)
  "Convert the options of the OPTION-LINE to a string."
  (thread-last (list (docopt-option-line-short-option option-line)
                     (docopt-option-line-long-option option-line))
    (seq-remove #'null)
    (seq-map #'docopt-to-string)
    (s-join ", ")))

(defun docopt-to-string-join (separator elements)
  "Apply `docopt-to-string' on ELEMENTS and join them with SEPARATOR."
  (string-join (seq-map #'docopt-to-string elements) separator))

(cl-defgeneric docopt-to-string (object)
  "Convert the Docopt OBJECT to a string.")

(cl-defmethod docopt-to-string ((argument docopt-argument))
  "Convert the Docopt usage ARGUMENT to a string."
  (concat "<" (docopt-argument-name argument) ">"))

(cl-defmethod docopt-to-string ((command docopt-command))
  "Convert the Docopt usage COMMAND to a string."
  (docopt-command-name command))

(cl-defmethod docopt-to-string ((either docopt-either))
  "Convert the Docopt EITHER to a string."
  (docopt-to-string-join " | " (docopt-either-members either)))

(cl-defmethod docopt-to-string ((lst list))
  "Convert the list LST to a string."
  (docopt-to-string-join " " lst))

(cl-defmethod docopt-to-string ((option docopt-long-option))
  "Convert the Docopt long OPTION to a string."
  (concat "--" (docopt-option-name option)
          (when-let ((argument (docopt-option-argument option)))
            (concat "=" (docopt-to-string argument)))))

(cl-defmethod docopt-to-string ((option docopt-short-option))
  "Convert the Docopt short OPTION to a string."
  (concat "-" (docopt-option-name option)
          (when-let ((argument (docopt-option-argument option)))
            (concat "=" (docopt-to-string argument)))))

(cl-defmethod docopt-to-string ((group docopt-optional-group))
  "Convert the Docopt usage GROUP to a string."
  (concat "[" (docopt-to-string-join " " (docopt-group-members group)) "]"))

(cl-defmethod docopt-to-string ((program docopt-program))
  "Convert the Docopt PROGRAM to a string."
  (string-join (list (docopt-program-header program)
                     (docopt-to-string--usage (docopt-program-usage program))
                     (docopt-to-string--options (docopt-program-options program))
                     (docopt-to-string--examples (docopt-program-examples program)))
               "\n\n"))

(cl-defmethod docopt-to-string ((repeated docopt-repeated))
  "Convert the Docopt usage REPEATED to a string."
  (concat (docopt-to-string (docopt-repeated-object repeated)) "..."))

(cl-defmethod docopt-to-string ((group docopt-required-group))
  "Convert the Docopt required GROUP to a string."
  (concat "(" (docopt-to-string-join " " (docopt-group-members group)) ")"))

(cl-defmethod docopt-to-string ((pattern docopt-usage-pattern))
  "Convert the Docopt usage PATTERN to a string."
  (concat (docopt-to-string (docopt-usage-pattern-command pattern)) " "
          (docopt-to-string-join " " (docopt-usage-pattern-expressions pattern))))

(cl-defmethod docopt-to-string ((line docopt-option-line))
  "Convert the Docopt option LINE to a string."
  (let ((long-option (docopt-option-line-long-option line))
        (short-option (docopt-option-line-short-option line)))
    (format "%-20s %s"
            (docopt-to-string--option-line-options line)
            (docopt-option-line-description line))))

(cl-defmethod docopt-to-string ((shortcut docopt-options-shortcut))
  "Convert the Docopt options SHORTCUT to a string."
  "[options]")

(cl-defmethod docopt-to-string ((shortcut docopt-standard-input))
  "Convert the Docopt options SHORTCUT to a string."
  "[-]")

(provide 'docopt-printer)

;;; docopt-printer.el ends here
