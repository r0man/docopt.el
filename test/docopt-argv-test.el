;;; docopt-argv-test.el --- The Docopt argument parser tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, command line argument
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
(require 'docopt-argv)
(require 'test-helper)

(describe "The `docopt-argument-parser` parser"

  (it "should parse a required argument"
    (expect (parsec-with-input "my-arg"
              (docopt-argument-parser (docopt-make-argument :name "ARG")))
            :to-equal (docopt-make-argument :name "ARG" :value "my-arg")))

  (it "should parse an optional argument"
    (expect (parsec-with-input "my-arg"
              (docopt-argument-parser (docopt-make-argument :name "ARG" :optional t)))
            :to-equal (docopt-make-argument :name "ARG" :optional t :value "my-arg")))

  (it "should parse an optional blank argument"
    (expect (parsec-with-input ""
              (docopt-argument-parser (docopt-make-argument :name "ARG" :optional t)))
            :to-equal nil))

  (it "should parse a short option"
    (expect (parsec-with-input "-h"
              (docopt-argument-parser (docopt-make-short-option :name "h")))
            :to-equal (docopt-make-short-option :name "h")))

  (it "should parse a long option"
    (expect (parsec-with-input "--help"
              (docopt-argument-parser (docopt-make-long-option :name "help")))
            :to-equal (docopt-make-long-option :name "help")))

  (it "should parse a long option with argument separated by equals sign"
    (expect (parsec-with-input "--speed=10"
              (docopt-argument-parser
               (docopt-make-long-option
                :argument (docopt-make-argument :name "kn")
                :name "speed"
                :optional t)))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "kn" :value "10")
                       :name "speed"
                       :optional t)))

  (it "should parse a long option with argument separated by whitespace"
    (expect (parsec-with-input "--speed 10"
              (docopt-argument-parser
               (docopt-make-long-option
                :argument (docopt-make-argument :name "kn")
                :name "speed"
                :optional t)))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "kn" :value "10")
                       :name "speed"
                       :optional t)))

  (it "should parse a command"
    (expect (parsec-with-input "naval_fate"
              (docopt-argument-parser (docopt-make-command :name "naval_fate")))
            :to-equal (docopt-make-command :name "naval_fate")))

  (it "should parse an either"
    (expect (parsec-with-input "a"
              (docopt-argument-parser
               (parsec-with-input "a|b" (docopt--parse-usage-expr))))
            :to-equal (docopt-make-command :name "a"))
    (expect (parsec-with-input "b"
              (docopt-argument-parser
               (parsec-with-input "a|b" (docopt--parse-usage-expr))))
            :to-equal (docopt-make-command :name "b")))

  (it "should parse a list of arguments"
    (expect (parsec-with-input "a b"
              (docopt-argument-parser
               (parsec-with-input "A B" (docopt--parse-usage-expr))))
            :to-equal (list (list (docopt-make-argument :name "A" :value "a")
                                  (docopt-make-argument :name "B" :value "b")))))

  (it "should parse an optional group"
    (expect (parsec-with-input "a b"
              (docopt-argument-parser
               (parsec-with-input "[A B]" (docopt--parse-usage-expr))))
            :to-equal (list (docopt-make-argument :name "A" :value "a" :optional t)
                            (docopt-make-argument :name "B" :value "b" :optional t))))

  (it "should parse a required group"
    (expect (parsec-with-input "a b"
              (docopt-argument-parser
               (parsec-with-input "(A B)" (docopt--parse-usage-expr))))
            :to-equal (list (docopt-make-argument :name "A" :value "a")
                            (docopt-make-argument :name "B" :value "b"))))

  (it "should parse a usage pattern"
    (expect (parsec-with-input "naval_fate --help"
              (docopt-argument-parser
               (parsec-with-input "Usage: naval_fate -h | --help"
                 (docopt--parse-usage))))
            :to-equal (list (docopt-make-command :name "naval_fate")
                            (docopt-make-long-option :name "help")))))

;;; docopt-argv-test.el ends here
