;;; docopt-argv-test.el --- The Docopt argument parser tests -*- lexical-binding: t -*-

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

;; The Docopt argument parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 'docopt-argv)
(require 'test-helper)

(describe "The `docopt-argv-parser` parser"

  (it "should parse a required argument"
    (expect (parsec-with-input "my-arg"
              (docopt-argv-parser (docopt-make-argument :name "ARG")))
            :to-equal (docopt-make-argument :name "ARG" :value "my-arg")))

  (it "should parse an optional argument"
    (expect (parsec-with-input "my-arg"
              (docopt-argv-parser (docopt-make-argument :name "ARG")))
            :to-equal (docopt-make-argument :name "ARG" :value "my-arg")))

  (it "should parse a short option"
    (expect (parsec-with-input "-h"
              (docopt-argv-parser (docopt-make-short-option :name "h")))
            :to-equal (docopt-make-short-option :name "h")))

  (it "should parse a long option"
    (expect (parsec-with-input "--help"
              (docopt-argv-parser (docopt-make-long-option :name "help")))
            :to-equal (docopt-make-long-option :name "help")))

  (it "should parse a long option with argument separated by equals sign"
    (expect (parsec-with-input "--speed=10"
              (docopt-argv-parser
               (docopt-make-long-option
                :argument (docopt-make-argument :name "kn")
                :name "speed")))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "kn" :value "10")
                       :name "speed")))

  (it "should parse a long option with argument separated by whitespace"
    (expect (parsec-with-input "--speed 10"
              (docopt-argv-parser
               (docopt-make-long-option
                :argument (docopt-make-argument :name "kn")
                :name "speed")))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "kn" :value "10")
                       :name "speed")))

  (it "should parse a command"
    (expect (parsec-with-input "naval_fate"
              (docopt-argv-parser (docopt-make-command :name "naval_fate")))
            :to-equal (docopt-make-command :name "naval_fate")))

  (it "should parse a list of arguments"
    (expect (parsec-with-input "a b"
              (docopt-argv-parser
               (parsec-with-input "A B" (docopt--parse-usage-expr))))
            :to-equal (list (docopt-make-argument :name "A" :value "a")
                            (docopt-make-argument :name "B" :value "b"))))

  (it "should parse an optional group"
    (expect (parsec-with-input "a b"
              (docopt-argv-parser
               (parsec-with-input "[A B]" (docopt--parse-usage-expr))))
            :to-equal (list (docopt-make-argument :name "A" :value "a")
                            (docopt-make-argument :name "B" :value "b"))))

  (it "should parse a required group"
    (expect (parsec-with-input "a b"
              (docopt-argv-parser
               (parsec-with-input "(A B)" (docopt--parse-usage-expr))))
            :to-equal (list (docopt-make-argument :name "A" :value "a")
                            (docopt-make-argument :name "B" :value "b"))))

  (it "should parse a usage pattern"
    (expect (parsec-with-input "naval_fate --help"
              (docopt-argv-parser
               (parsec-with-input "Usage: naval_fate -h | --help"
                 (docopt--parse-usage))))
            :to-equal (list (docopt-make-command :name "naval_fate")
                            (docopt-make-long-option :name "help")))))

(describe "Parsing an either"
  :var ((exprs (parsec-with-input "a|-b|--c" (docopt--parse-usage-expr))))

  (it "should parse the branch with a command"
    (expect (parsec-with-input "a" (docopt-argv-parser exprs))
            :to-equal (docopt-make-command :name "a")))

  (it "should parse the branch with a short option"
    (expect (parsec-with-input "-b" (docopt-argv-parser exprs))
            :to-equal (docopt-make-short-option :name "b")))

  (it "should parse the branch with a long option"
    (expect (parsec-with-input "--c" (docopt-argv-parser exprs))
            :to-equal (docopt-make-long-option :name "c"))))

(describe "Parsing optional short options within an either"
  :var ((exprs (parsec-with-input "[-a|-b]" (docopt--parse-usage-expr))))

  (it "should parse the empty string"
    (expect (parsec-with-input "" (docopt-argv-parser exprs))
            :to-equal nil))

  (it "should parse the first branch"
    (expect (parsec-with-input "-a" (docopt-argv-parser exprs))
            :to-equal (docopt-make-short-option :name "a")))

  (it "should parse the second branch"
    (expect (parsec-with-input "-b" (docopt-argv-parser exprs))
            :to-equal (docopt-make-short-option :name "b"))))

(describe "Parsing a command followed by optional short options within an either"
  :var ((exprs (parsec-with-input "cmd [-a|-b]" (docopt--parse-usage-expr))))

  (it "should parse just the command"
    (expect (parsec-with-input "cmd" (docopt-argv-parser exprs))
            :to-equal (list (docopt-make-command :name "cmd") nil)))

  (it "should parse the command and the first branch"
    (expect (parsec-with-input "cmd -a" (docopt-argv-parser exprs))
            :to-equal  (list (docopt-make-command :name "cmd")
                             (docopt-make-short-option :name "a"))))

  (it "should parse the command and the second branch"
    (expect (parsec-with-input "cmd -b" (docopt-argv-parser exprs))
            :to-equal  (list (docopt-make-command :name "cmd")
                             (docopt-make-short-option :name "b")))))

(describe "Parsing an options shortcut"
  :var ((shortcut (docopt-make-options-shortcut
                   (list (docopt-make-long-option :name "aa")
                         (docopt-make-short-option :name "a"))
                   (list (docopt-make-long-option :name "bb")
                         (docopt-make-short-option :name "b")))))

  (it "should parse no options"
    (expect (parsec-with-input "" (docopt-argv-parser shortcut))
            :to-equal nil))

  (it "should parse a single short option"
    (expect (parsec-with-input "-a" (docopt-argv-parser shortcut))
            :to-equal (list (docopt-make-short-option :name "a"))))

  (it "should parse a single long option"
    (expect (parsec-with-input "--aa" (docopt-argv-parser shortcut))
            :to-equal (list (docopt-make-long-option :name "aa"))))

  (it "should parse multiple option"
    (expect (parsec-with-input "-a -b --bb --aa" (docopt-argv-parser shortcut))
            :to-equal (list (docopt-make-short-option :name "a")
                            (docopt-make-short-option :name "b")
                            (docopt-make-long-option :name "bb")
                            (docopt-make-long-option :name "aa")))))

(describe "The `docopt-parse-argv-alist` function"
  :var ((program (docopt-parse-program docopt-naval-fate-str)))

  (it "should parse \"naval_fate mine set 1 2 --moored\""
    (expect (docopt-parse-argv-alist program "naval_fate mine set 1 2 --moored")
            :to-equal '((mine . t)
                        (set . t)
                        (<x> . "1")
                        (<y> . "2")
                        (--moored . t))))

  (it "should parse \"naval_fate mine set 1 2\""
    (expect (docopt-parse-argv-alist program "naval_fate mine set 1 2")
            :to-equal '((mine . t)
                        (set . t)
                        (<x> . "1")
                        (<y> . "2"))))

  (it "should parse \"naval_fate mine set 1 2 --drifting\""
    (expect (docopt-parse-argv-alist program "naval_fate mine set 1 2 --drifting")
            :to-equal '((mine . t)
                        (set . t)
                        (<x> . "1")
                        (<y> . "2")
                        (--drifting . t))))

  (it "should parse \"naval_fate ship SHIP-123 move 1 2 --speed=10\""
    (expect (docopt-parse-argv-alist program "naval_fate ship SHIP-123 move 1 2 --speed=10")
            :to-equal '((ship . t)
                        (<name> . "SHIP-123")
                        (move . t)
                        (<x> . "1")
                        (<y> . "2")
                        (--speed . "10")))))

(describe "Parsing a program without arguments"
  (it "should return just the program"
    (expect (docopt-parse-argv-alist (docopt-parse-program "Usage: prog") "prog")
            :to-equal '())))

;; (docopt--parse-argv-simple-list*
;;  (list (docopt-short-option :name "a")
;;        (docopt-short-option :name "b")
;;        (docopt-short-option :name "c")))

;; (parsec-with-input "-a -b -c"
;;   (docopt-argv-parser
;;    (list (docopt-short-option :name "a")
;;          (docopt-short-option :name "b")
;;          (docopt-short-option :name "c"))))

;; (parsec-with-input "-b  -c"
;;   (docopt-argv-parser
;;    (list (docopt-short-option :name "a")
;;          (docopt-short-option :name "b")
;;          (docopt-short-option :name "c"))))

;;; docopt-argv-test.el ends here
