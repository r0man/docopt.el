;;; docopt-naval-fate.el --- The Naval Fate example program -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 8 Mar 2020
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

;; The Naval Fate example program

;;; Code:

(require 'docopt)
(require 'docopt-transient)
(require 'parsec)
(require 's)

(defvar docopt-naval-fate-header-str
  "Naval Fate.\n\n")

(defvar docopt-naval-fate-usage-str
  "Usage:
  naval-fate ship new <name>...
  naval-fate ship <name> move <x> <y> [--speed=<kn>]
  naval-fate ship shoot <x> <y>
  naval-fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval-fate -h | --help
  naval-fate --version")

(defvar docopt-naval-fate-options-str
  "Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.")

(defvar docopt-naval-fate-examples-str
  "Examples:
  naval-fate ship new SHIP-123
  naval-fate ship SHIP-123 move 1 2 --speed=10")

(defvar docopt-naval-fate-str
  (concat docopt-naval-fate-header-str
          docopt-naval-fate-usage-str "\n\n"
          docopt-naval-fate-options-str "\n\n"
          docopt-naval-fate-examples-str "\n"))

(defvar docopt-naval-fate-options-ast
  (parsec-with-input docopt-naval-fate-options-str
    (docopt-parser--options)))

(defvar docopt-naval-fate-usage-ast
  (parsec-with-input docopt-naval-fate-usage-str
    (docopt-parser--usage (list 'options docopt-naval-fate-options-ast))))

(defvar docopt-naval-fate-examples-ast
  (parsec-with-input docopt-naval-fate-examples-str
    (docopt-parser--examples)))

;; Naval Fate Options

(defvar docopt-naval-fate-option-drifting
  (docopt-long-option
   :description "Drifting mine."
   :name "drifting"
   :prefixes '("driftin" "drifti" "drift" "drif" "dri" "dr" "d")))

(defvar docopt-naval-fate-option-h
  (docopt-short-option
   :description "Show this screen."
   :name "h"
   :synonym "help"))

(defvar docopt-naval-fate-option-help
  (docopt-long-option
   :description "Show this screen."
   :name "help"
   :prefixes '("hel" "he" "h")
   :synonym "h"))

(defvar docopt-naval-fate-option-moored
  (docopt-long-option
   :description "Moored (anchored) mine."
   :name "moored"
   :prefixes '("moore" "moor" "moo" "mo" "m")))

(defvar docopt-naval-fate-option-speed
  (docopt-long-option
   :argument (docopt-argument :name "kn" :default "10")
   :description "Speed in knots [default: 10]."
   :name "speed"
   :prefixes '("spee" "spe" "sp" "s")))

(defvar docopt-naval-fate-option-version
  (docopt-long-option
   :description "Show version."
   :name "version"
   :prefixes '("versio" "versi" "vers" "ver" "ve" "v")))

(with-slots (incompatible) docopt-naval-fate-option-h
  (setf incompatible (list docopt-naval-fate-option-help)))

(with-slots (incompatible) docopt-naval-fate-option-help
  (setf incompatible (list docopt-naval-fate-option-h)))

(with-slots (incompatible) docopt-naval-fate-option-moored
  (setf incompatible (list docopt-naval-fate-option-drifting)))

(with-slots (incompatible) docopt-naval-fate-option-drifting
  (setf incompatible (list docopt-naval-fate-option-moored)))

;; Usage patterns

(defvar docopt-naval-fate-pattern-ship-new
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   (docopt-command :name "ship")
   (docopt-command :name "new")
   (docopt-make-repeated (docopt-argument :name "name"))))

(defvar docopt-naval-fate-pattern-ship-name
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   (docopt-command :name "ship")
   (docopt-argument :name "name" :repeat t)
   (docopt-command :name "move")
   (docopt-argument :name "x")
   (docopt-argument :name "y")
   (docopt-make-optional-group docopt-naval-fate-option-speed)))

(defvar docopt-naval-fate-pattern-ship-shoot
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   (docopt-command :name "ship")
   (docopt-command :name "shoot")
   (docopt-argument :name "x")
   (docopt-argument :name "y")))

(defvar docopt-naval-fate-set-command
  (docopt-command :name "set"))

(defvar docopt-naval-fate-remove-command
  (docopt-command :name "remove"))

(with-slots (incompatible) docopt-naval-fate-set-command
  (setf incompatible (list docopt-naval-fate-remove-command)))

(with-slots (incompatible) docopt-naval-fate-remove-command
  (setf incompatible (list docopt-naval-fate-set-command)))

(defvar docopt-naval-fate-pattern-mine
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   (docopt-command :name "mine")
   (docopt-make-required-group
    (docopt-make-either
     (list docopt-naval-fate-set-command)
     (list docopt-naval-fate-remove-command)))
   (docopt-argument :name "x")
   (docopt-argument :name "y")
   (docopt-make-optional-group
    (docopt-make-either
     (list docopt-naval-fate-option-moored)
     (list docopt-naval-fate-option-drifting)))))

(defvar docopt-naval-fate-pattern-help
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   (docopt-make-either
    (list docopt-naval-fate-option-h)
    (list docopt-naval-fate-option-help))))

(defvar docopt-naval-fate-pattern-version
  (docopt-make-usage-pattern
   (docopt-command :name "naval-fate")
   docopt-naval-fate-option-version))

(defvar docopt-naval-fate
  (docopt-parse docopt-naval-fate-str))

(defun docopt-naval-fate-transient ()
  "Invoke the Docopt Naval Fate transient."
  (interactive)
  (docopt-transient-define-command docopt-naval-fate)
  (docopt-transient-invoke-command docopt-naval-fate))

(provide 'docopt-naval-fate)

;;; docopt-naval-fate.el ends here
