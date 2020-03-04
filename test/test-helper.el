;;; test-helper.el --- The docopt test helpers -*- lexical-binding: t -*-

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

;;  The docopt test helpers.

;;; Code:

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

(defvar docopt-naval-fate-usage-ast
  (parsec-with-input docopt-naval-fate-usage-str
    (docopt--parse-usage)))

(defvar docopt-naval-fate-options-ast
  (parsec-with-input docopt-naval-fate-options-str
    (docopt--parse-options)))

(defvar docopt-naval-fate-examples-ast
  (parsec-with-input docopt-naval-fate-examples-str
    (docopt--parse-examples)))

(defvar docopt-naval-fate-str
  (concat docopt-naval-fate-header-str
          docopt-naval-fate-usage-str "\n\n"
          docopt-naval-fate-options-str "\n\n"
          docopt-naval-fate-examples-str "\n"))

(defvar docopt-naval-fate
  (docopt-parse-program docopt-naval-fate-str))

(provide 'test-helper)

;;; test-helper.el ends here
