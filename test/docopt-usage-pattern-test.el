;;; docopt-usage-pattern-test.el --- The Docopt usage pattern tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
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

;; The Docopt usage pattern tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 'docopt-generic)
(require 'test-helper)

(describe "Evaluating docopt-format with the Naval Fate usage pattern"
  :var ((patterns (docopt-program-usage (docopt-parse docopt-naval-fate-str))))

  (it "at index 0 should return \"naval-fate ship new <name>...\""
    (expect (docopt-format (nth 0 patterns))
            :to-equal "naval-fate ship new <name>..."))

  (it "at index 1 should return \"naval-fate ship <name> move <x> <y> [--speed=<kn>]\""
    (expect (docopt-format (nth 1 patterns))
            :to-equal "naval-fate ship <name> move <x> <y> [--speed=<kn>]"))

  (it "at index 2 should return \"naval-fate ship shoot <x> <y>\""
    (expect (docopt-format (nth 2 patterns))
            :to-equal "naval-fate ship shoot <x> <y>"))

  (it "at index 3 should return \"naval-fate mine (set | remove) <x> <y> [--moored | --drifting]\""
    (expect (docopt-format (nth 3 patterns))
            :to-equal "naval-fate mine (set | remove) <x> <y> [--moored | --drifting]"))

  (it "at index 4 should return \"naval-fate -h | --help\""
    (expect (docopt-format (nth 4 patterns))
            :to-equal "naval-fate -h | --help"))

  (it "at index 5 should return \"naval-fate --version\""
    (expect (docopt-format (nth 5 patterns))
            :to-equal "naval-fate --version")))

(describe "Evaluating docopt-string with the Naval Fate usage pattern"
  :var ((patterns (docopt-program-usage (docopt-parse docopt-naval-fate-str))))

  (it "at index 0 should return \"naval-fate ship new <name>...\""
    (expect (docopt-string (nth 0 patterns))
            :to-equal "naval-fate ship new <name>..."))

  (it "at index 1 should return \"naval-fate ship <name> move <x> <y> [--speed=<kn>]\""
    (expect (docopt-string (nth 1 patterns))
            :to-equal "naval-fate ship <name> move <x> <y> [--speed=<kn>]"))

  (it "at index 2 should return \"naval-fate ship shoot <x> <y>\""
    (expect (docopt-string (nth 2 patterns))
            :to-equal "naval-fate ship shoot <x> <y>"))

  (it "at index 3 should return \"naval-fate mine (set | remove) <x> <y> [--moored | --drifting]\""
    (expect (docopt-string (nth 3 patterns))
            :to-equal "naval-fate mine (set | remove) <x> <y> [--moored | --drifting]"))

  (it "at index 4 should return \"naval-fate -h | --help\""
    (expect (docopt-string (nth 4 patterns))
            :to-equal "naval-fate -h | --help"))

  (it "at index 5 should return \"naval-fate --version\""
    (expect (docopt-string (nth 5 patterns))
            :to-equal "naval-fate --version")))

;;; docopt-usage-pattern-test.el ends here
