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
(require 'parsec)
(require 's)

(defvar docopt-naval-fate-header-str
  "Naval Fate.\n\n")

(defvar docopt-naval-fate-usage-str
  "Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version")

(defvar docopt-naval-fate-options-str
  "Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.")

(defvar docopt-naval-fate-examples-str
  "Examples:
  naval_fate ship new SHIP-123
  naval_fate ship SHIP-123 move 1 2 --speed=10")

(defvar docopt-naval-fate-str
  (concat docopt-naval-fate-header-str
          docopt-naval-fate-usage-str "\n\n"
          docopt-naval-fate-options-str "\n\n"
          docopt-naval-fate-examples-str "\n"))

(defvar docopt-naval-fate-usage-ast
  (parsec-with-input docopt-naval-fate-usage-str
    (docopt--parse-usage)))

(defvar docopt-naval-fate-options-ast
  (parsec-with-input docopt-naval-fate-options-str
    (docopt--parse-options)))

(defvar docopt-naval-fate-examples-ast
  (parsec-with-input docopt-naval-fate-examples-str
    (docopt--parse-examples)))

;; Naval Fate Options

(defvar docopt-naval-fate-option-drifting
  (docopt-long-option
   :description "Drifting mine."
   :object-name "drifting"
   :prefixes '("driftin" "drifti" "drift" "drif" "dri" "dr" "d")))

(defvar docopt-naval-fate-option-h
  (docopt-short-option
   :description "Show this screen."
   :object-name "h"
   :synonym "help"))

(defvar docopt-naval-fate-option-help
  (docopt-long-option
   :description "Show this screen."
   :object-name "help"
   :prefixes '("hel" "he" "h")
   :synonym "h"))

(defvar docopt-naval-fate-option-moored
  (docopt-long-option
   :description "Moored (anchored) mine."
   :object-name "moored"
   :prefixes '("moore" "moor" "moo" "mo" "m")))

(defvar docopt-naval-fate-option-speed
  (docopt-long-option
   :argument (docopt-argument :object-name "kn" :default "10")
   :description "Speed in knots [default: 10]."
   :object-name "speed"
   :prefixes '("spee" "spe" "sp" "s")))

(defvar docopt-naval-fate-option-version
  (docopt-long-option
   :description "Show version."
   :object-name "version"
   :prefixes '("versio" "versi" "vers" "ver" "ve" "v")))

;; Usage patterns

(defvar docopt-naval-fate-pattern-ship-new
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   (docopt-command :object-name "ship")
   (docopt-command :object-name "new")
   (docopt-make-repeated (docopt-argument :object-name "name"))))

(defvar docopt-naval-fate-pattern-ship-name
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   (docopt-command :object-name "ship")
   (docopt-argument :object-name "name")
   (docopt-command :object-name "move")
   (docopt-argument :object-name "x")
   (docopt-argument :object-name "y")
   (docopt-make-optional-group docopt-naval-fate-option-speed)))

(defvar docopt-naval-fate-pattern-ship-shoot
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   (docopt-command :object-name "ship")
   (docopt-command :object-name "shoot")
   (docopt-argument :object-name "x")
   (docopt-argument :object-name "y")))

(defvar docopt-naval-fate-pattern-mine
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   (docopt-command :object-name "mine")
   (docopt-make-required-group
    (docopt-make-either
     (list (docopt-command :object-name "set"))
     (list (docopt-command :object-name "remove"))))
   (docopt-argument :object-name "x")
   (docopt-argument :object-name "y")
   (docopt-make-optional-group
    (docopt-make-either
     (list docopt-naval-fate-option-moored)
     (list docopt-naval-fate-option-drifting)))))

(defvar docopt-naval-fate-pattern-help
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   (docopt-make-either
    (list docopt-naval-fate-option-h)
    (list docopt-naval-fate-option-help))))

(defvar docopt-naval-fate-pattern-version
  (docopt-make-usage-pattern
   (docopt-command :object-name "naval_fate")
   docopt-naval-fate-option-version))

(defvar docopt-naval-fate
  (docopt-parse docopt-naval-fate-str))

(provide 'docopt-naval-fate)

;;; docopt-naval-fate.el ends here
