;;; docopt-ast-test.el --- The Docopt AST tests -*- lexical-binding: t -*-

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

;; The Docopt AST tests

;;; Code:

(require 'buttercup)
(require 'docopt-ast)
(require 'docopt-naval-fate)

;;;; Either

(describe "Concatenate the members of eithers"
  (it "should concatenate the members"
    (expect (docopt-either-concat
             (docopt-make-either (docopt-argument :name "A")
                                 (docopt-argument :name "B"))
             (docopt-make-either (docopt-argument :name "C"))
             (docopt-make-either (docopt-argument :name "D")))
            :to-equal
            (docopt-make-either
             (docopt-argument :name "A")
             (docopt-argument :name "B")
             (docopt-argument :name "C")
             (docopt-argument :name "D")))))

;;;; Option

(describe "Computing option prefixes"
  (it "should return the prefixes for Naval Fate --help"
    (expect (docopt-option-prefixes
             (docopt-program-option docopt-naval-fate "help")
             (docopt-program-options docopt-naval-fate))
            :to-equal '("hel" "he" "h"))))

(describe "Removing synonyms"
  (it "should remove short options that have a long option synonym"
    (expect (docopt-option-remove-synonyms
             (list (docopt-long-option :name "help" :synonym "h")
                   (docopt-short-option :name "h" :synonym "help")
                   (docopt-short-option :name "v" :synonym "version")))
            :to-equal (list (docopt-long-option :name "help" :synonym "h")
                            (docopt-short-option :name "v" :synonym "version")))))

;;;; Program

(describe "Find an option by"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "long option should return nil when not found"
    (expect (docopt-program-option program "UNKNOWN")
            :to-equal nil))

  (it "short option should return nil when not found"
    (expect (docopt-program-option program "U")
            :to-equal nil))

  (it "long option should return the long option"
    (expect (docopt-program-option program "help")
            :to-equal docopt-naval-fate-option-help))

  (it "short option should return the short option"
    (expect (docopt-program-option program "h")
            :to-equal docopt-naval-fate-option-h)))

(describe "The copy of a program"
  :var ((program (docopt-parse docopt-naval-fate-str)))
  (it "should be equal to the original"
    (expect (clone program) :to-equal program)))

(describe "Printing naval fate"
  :var ((program (docopt-parse docopt-naval-fate-str)))
  (it "and parsing it should return the same program"
    (expect (docopt-equal program  (docopt-parse (docopt-string program)))
            :to-equal t)))

;;;; Usage Pattern

(describe "Evaluating docopt-format with the Naval Fate usage pattern"
  :var ((patterns (docopt-program-usage (docopt-parse docopt-naval-fate-str))))

  (it "at index 0 should return \"naval-fate ship new <name>\""
    (expect (docopt-format (nth 0 patterns))
            :to-equal "naval-fate ship new <name>"))

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

;;; docopt-ast-test.el ends here
