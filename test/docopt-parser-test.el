;;; docopt-parser-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

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

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt-parser)

(describe "Parsing tokens"

  (it "should parse a space"
    (expect (parsec-with-input " " (docopt--parse-space)) :to-equal " "))

  (it "should parse spaces"
    (expect (parsec-with-input "  " (docopt--parse-spaces)) :to-equal "  "))

  (it "should parse a space character as white space"
    (expect (parsec-with-input " " (docopt--parse-whitespace)) :to-equal " "))

  (it "should parse a newline as white space"
    (expect (parsec-with-input "\n" (docopt--parse-whitespace)) :to-equal "\n"))

  (it "should parse CRLF as white space and return newline"
    (expect (parsec-with-input "\r\n" (docopt--parse-whitespace)) :to-equal "\n"))

  (it "should parse \"[options]\""
    (expect (parsec-with-input "[options]" (docopt--parse-options-shortcut))
            :to-equal (docopt-make-options-shortcut)))

  (it "should parse \"Examples:\""
    (expect (parsec-with-input "Examples:" (docopt--parse-examples-str))
            :to-equal "Examples:"))

  (it "should parse \"Usage:\""
    (expect (parsec-with-input "Usage:" (docopt--parse-usage-header))
            :to-equal "Usage:"))

  (it "should parse \"Options:\""
    (expect (parsec-with-input "Options:" (docopt--parse-options-str))
            :to-equal "Options:"))

  (it "should parse the \"-h\" short option name"
    (expect (parsec-with-input "-h" (docopt--parse-short-option-name))
            :to-equal "h"))

  (it "should parse the \"--help\" long option name"
    (expect (parsec-with-input "--help" (docopt--parse-long-option-name))
            :to-equal "help"))

  (it "should parse the \"--help-me\" long option name"
    (expect (parsec-with-input "--help-me" (docopt--parse-long-option-name))
            :to-equal "help-me"))

  (it "should parse a section header"
    (expect (parsec-with-input "Examples:" (docopt--parse-section-header))
            :to-equal "Examples")))

(describe "The argument parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<host>" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host")))

  (it "should parse a spaceship argument containing a slash"
    (expect (parsec-with-input "<km/h>" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "km/h")))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "HOST" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST")))

  (it "should parse a upper case argument containing a slash"
    (expect (parsec-with-input "KM/H" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "KM/H"))))

(describe "The default parser"

  (it "should handle nil"
    (expect (docopt--parse-default nil) :to-be nil))

  (it "should parse a decimal as a default"
    (expect (docopt--parse-default "[default: 2.95]") :to-equal "2.95"))

  (it "should parse a default without spaces"
    (expect (docopt--parse-default "[default:2.95]") :to-equal "2.95"))

  (it "should parse a default with spaces"
    (expect (docopt--parse-default "[default:  2.95  ]") :to-equal "2.95"))

  (it "should parse a filename as a default"
    (expect (docopt--parse-default "[default: test.txt]") :to-equal "test.txt"))

  (it "should parse the current directory as a default"
    (expect (docopt--parse-default "[default: ./]") :to-equal "./")))


(describe "The long option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "--help" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "help")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "--path PATH" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "PATH")
                       :name "path")))

  (it "should parse an option with a space separated spaceship argument"
    (expect (parsec-with-input "--path <path>" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "path")
                       :name "path")))

  (it "should parse an option with a \"=\" separated upper case argument"
    (expect (parsec-with-input "--path=PATH" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "PATH")
                       :name "path")))

  (it "should parse an option with a \"=\" separated lower case argument"
    (expect (parsec-with-input "--path=path" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "path")
                       :name "path")))

  (it "should parse an option with a \"=\" separated spaceship argument"
    (expect (parsec-with-input "--path=<path>" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "path")
                       :name "path"))))


(describe "The short option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "-h" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "h")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "-p PATH" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option
                       :argument (docopt-make-argument :name "PATH")
                       :name "p")))

  (it "should parse an option with a not separated argument"
    (expect (parsec-with-input "-pPATH" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option
                       :argument (docopt-make-argument :name "PATH")
                       :name "p"))))

(describe "The option line description parser"
  (it "should parse single-line descriptions"
    (expect (parsec-with-input "Show version.\n  More version help."
              (docopt--parse-option-line-description))
            :to-equal "Show version.\n  More version help."))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat "Show version.\n"
                        "  More version help.\n"
                        "  --moored      Moored (anchored) mine.\n")
              (docopt--parse-option-line-description))
            :to-equal "Show version.\n  More version help.")))

(describe "The option line options parser"

  (it "should parse short option only"
    (expect (parsec-with-input "-h  Show this help." (docopt--parse-option-line-options))
            :to-equal (list nil (docopt-make-short-option :name "h"))))

  (it "should parse long option only"
    (expect (parsec-with-input "--help  Show this help." (docopt--parse-option-line-options))
            :to-equal (list (docopt-make-long-option :name "help") nil)))

  (it "should parse short options first"
    (expect (parsec-with-input "-h, --help  Show this help." (docopt--parse-option-line-options))
            :to-equal (list (docopt-make-long-option :name "help")
                            (docopt-make-short-option :name "h"))))

  (it "should parse short options first with minimal spacing"
    (expect (parsec-with-input "-h,--help Show this help." (docopt--parse-option-line-options))
            :to-equal (parsec-with-input "-h,--help Show this help." (docopt--parse-option-line-options))))

  (it "should parse long options first"
    (expect (parsec-with-input "--help, -h  Show this help." (docopt--parse-option-line-options))
            :to-equal (list (docopt-make-long-option :name "help")
                            (docopt-make-short-option :name "h"))))

  (it "should parse long options first with minimal spacing"
    (expect (parsec-with-input "--help,-h Show this help." (docopt--parse-option-line-options))
            :to-equal (list (docopt-make-long-option :name "help")
                            (docopt-make-short-option :name "h")))))

(describe "The option line parser"

  (it "should parse a short option without an argument"
    (expect (parsec-with-input "-h  Show this help." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line :description "Show this help." :short-name "h")))

  (it "should parse a short and long option without an argument"
    (expect (parsec-with-input "-h, --help  Show this help." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line :description "Show this help." :long-name "help" :short-name "h")))

  (it "should parse a short option with a space separated argument"
    (expect (parsec-with-input "-p PATH  Path to files." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument-name "PATH"
                       :description "Path to files."
                       :short-name "p")))

  (it "should parse a short option with a not separated argument"
    (expect (parsec-with-input "-pPATH  Path to files." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument-name "PATH"
                       :description "Path to files."
                       :short-name "p")))

  (it "should parse a long option without an argument"
    (expect (parsec-with-input "--help  Show this help." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :description "Show this help."
                       :long-name "help")))

  (it "should parse a long and shortcut option without an argument"
    (expect (parsec-with-input "--help, -h  Show this help." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :description "Show this help."
                       :long-name "help"
                       :short-name "h")))

  (it "should parse a long option with an argument"
    (expect (parsec-with-input "--path PATH  Path to files." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument-name "PATH"
                       :description "Path to files."
                       :long-name "path")))

  (it "should parse a short option with a default argument"
    (expect (parsec-with-input "-c K  The K coefficient [default: 2.95]" (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument (docopt-make-argument :name "K" :default "2.95")
                       :description "The K coefficient [default: 2.95]"
                       :short-name "c")))

  (it "should parse a long option with a default argument"
    (expect (parsec-with-input "--coefficient=K  The K coefficient [default: 2.95]" (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument (docopt-make-argument :name "K" :default "2.95")
                       :description "The K coefficient [default: 2.95]"
                       :long-name "coefficient"))))


(describe "The option lines parser"
  (it "should parse single-line descriptions"
    (expect (parsec-with-input
                (concat "--moored      Moored (anchored) mine.\n"
                        "--drifting    Drifting mine.")
              (docopt--parse-option-lines))
            :to-equal (list (docopt-make-option-line
                             :description "Moored (anchored) mine."
                             :long-name "moored")
                            (docopt-make-option-line
                             :description "Drifting mine."
                             :long-name "drifting"))))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat "--moored      Moored (anchored) mine.\n"
                        "--drifting    Drifting mine.\n"
                        "--version     Show version."
                        "                More version help.")
              (docopt--parse-option-lines))
            :to-equal (list (docopt-make-option-line
                             :description "Moored (anchored) mine."
                             :long-name "moored")
                            (docopt-make-option-line
                             :description "Drifting mine."
                             :long-name "drifting")
                            (docopt-make-option-line
                             :description "Show version.                More version help."
                             :long-name "version")))))


(describe "Parsing stacked short options"
  (it "should return a list of short options"
    (expect (parsec-with-input "-abc" (docopt--parse-short-options-stacked))
            :to-equal (list (docopt-make-short-option :name "a")
                            (docopt-make-short-option :name "b")
                            (docopt-make-short-option :name "c")))))

(describe "The usage pattern expression parser"

  (it "should parse standard input"
    (expect (parsec-with-input "[-]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-standard-input))))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "ARG" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-argument :name "ARG"))))

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<ARG>" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-argument :name "ARG"))))

  (it "should parse a spaceship argument with double colon"
    (expect (parsec-with-input "<host:port>" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-argument :name "host:port"))))

  (it "should parse a spaceship argument with whitespace"
    (expect (parsec-with-input "<input file>" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-argument :name "input file"))))

  (it "should parse a repeatable argument"
    (expect (parsec-with-input "ARG..." (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-repeated (docopt-make-argument :name "ARG")))))

  (it "should parse a repeatable argument with whitespace"
    (expect (parsec-with-input "ARG ..." (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-repeated (docopt-make-argument :name "ARG")))))

  (it "should parse an optional spaceship argument"
    (expect (parsec-with-input "[<ARG>]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-optional-group (docopt-make-argument :name "ARG")))))

  (it "should parse a required spaceship argument"
    (expect (parsec-with-input "(<ARG>)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-make-argument :name "ARG")))))

  (it "should parse an optional upper case argument"
    (expect (parsec-with-input "[ARG]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-optional-group (docopt-make-argument :name "ARG")))))

  (it "should parse an optional with multiple members"
    (expect (parsec-with-input "[ARG-1 ARG-2]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-make-argument :name "ARG-1")
                             (docopt-make-argument :name "ARG-2")))))

  (it "should parse a required group with upper case argument"
    (expect (parsec-with-input "(ARG)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-make-argument :name "ARG")))))

  (it "should parse a required group with upper case argument and whitespace"
    (expect (parsec-with-input "(  ARG  )" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-make-argument :name "ARG")))))

  (it "should parse a required group with multiple members"
    (expect (parsec-with-input "(ARG-1 ARG-2)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-make-argument :name "ARG-1")
                             (docopt-make-argument :name "ARG-2")))))

  (it "should parse stacked short options"
    (expect (parsec-with-input "-abc" (docopt--parse-usage-expr))
            :to-equal (parsec-with-input "-a -b -c" (docopt--parse-usage-expr))))

  (it "should parse optional stacked short options"
    (expect (parsec-with-input "[-abc]" (docopt--parse-usage-expr))
            :to-equal  (parsec-with-input "[-a -b -c]" (docopt--parse-usage-expr))))

  (it "should parse required stacked short options"
    (expect (parsec-with-input "(-abc)" (docopt--parse-usage-expr))
            :to-equal (parsec-with-input "(-a -b -c)" (docopt--parse-usage-expr))))

  (it "should parse an optional long option"
    (expect (parsec-with-input "[--help TOPIC]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-make-long-option
                              :argument (docopt-make-argument :name "TOPIC")
                              :name "help"
                              )))))

  (it "should parse a required long option"
    (expect (parsec-with-input "(--help TOPIC)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-make-long-option
                              :argument (docopt-make-argument :name "TOPIC")
                              :name "help")))))

  (it "should parse a repeatable long option"
    (expect (parsec-with-input "--help TOPIC..." (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-repeated
                             (docopt-make-long-option
                              :argument (docopt-make-argument :name "TOPIC")
                              :name "help")))))

  (it "should parse an optional short option"
    (expect (parsec-with-input "[-h]" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-make-short-option :name "h")))))

  (it "should parse a required short option"
    (expect (parsec-with-input "(-h)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-make-short-option :name "h")))))

  (it "should parse mutually exclusive options"
    (expect (parsec-with-input "-h | --help" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-either
                             (list (docopt-make-short-option :name "h"))
                             (list (docopt-make-long-option :name "help"))))))

  (it "should parse nested expressions"
    (expect (parsec-with-input "(N [M | (K | L)] | O P)" (docopt--parse-usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-make-either
                              (list (docopt-make-argument :name "N")
                                    (docopt-make-optional-group
                                     (docopt-make-either
                                      (list (docopt-make-argument :name "M"))
                                      (list (docopt-make-required-group
                                             (docopt-make-either
                                              (list (docopt-make-argument :name "K"))
                                              (list (docopt-make-argument :name "L"))))))))
                              (list (docopt-make-argument :name "O")
                                    (docopt-make-argument :name "P"))))))))

(describe "The usage pattern parser"

  (it "should parse program only"
    (expect (parsec-with-input "Usage: prog"
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "prog")))))

  (it "should parse the options shortcut"
    (expect (parsec-with-input "Usage: prog [options]"
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "prog")
                             (docopt-make-options-shortcut)))))

  (it "should parse a spaceship argument"
    (expect (parsec-with-input
                (concat "Usage: naval_fate ship new <name>...\n"
                        "       naval_fate ship <name> move <x> <y> [--speed=<kn>]")
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "naval_fate")
                             (docopt-make-command :name "ship")
                             (docopt-make-command :name "new")
                             (docopt-make-repeated (docopt-make-argument :name "name")))
                            (docopt-make-usage-pattern
                             (docopt-make-command :name "naval_fate")
                             (docopt-make-command :name "ship")
                             (docopt-make-argument :name "name")
                             (docopt-make-command :name "move")
                             (docopt-make-argument :name "x")
                             (docopt-make-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-make-long-option
                               :argument (docopt-make-argument :name "kn")
                               :name "speed"
                               ))))))

  (it "should parse \"Usage: naval_fate -h | --help\""
    (expect (parsec-with-input "Usage: naval_fate -h | --help"
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "naval_fate")
                             (docopt-make-either
                              (list (docopt-short-option :name "h"))
                              (list (docopt-long-option :name "help")))))))

  (it "should parse \"Usage: naval_fate mine (set | remove all) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval_fate mine (set many | remove all) <x> <y> [--moored|--drifting]"
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "naval_fate")
                             (docopt-make-command :name "mine")
                             (docopt-make-required-group
                              (docopt-make-either
                               (list (docopt-make-command :name "set")
                                     (docopt-make-command :name "many"))
                               (list (docopt-make-command :name "remove")
                                     (docopt-make-command :name "all"))))
                             (docopt-make-argument :name "x")
                             (docopt-make-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-make-either
                               (list (docopt-make-long-option :name "moored"))
                               (list (docopt-make-long-option :name "drifting"))))))))

  (it "should parse \"Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
              (docopt--parse-usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-make-command :name "naval_fate")
                             (docopt-make-command :name "mine")
                             (docopt-make-required-group
                              (docopt-make-either
                               (list (docopt-make-command :name "set"))
                               (list (docopt-make-command :name "remove"))))
                             (docopt-make-argument :name "x")
                             (docopt-make-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-make-either
                               (list (docopt-make-long-option :name "moored"))
                               (list (docopt-make-long-option :name "drifting")))))))))

(describe "The program parser"

  (it "should parse \"PROGRAM Usage: prog --foo\""
    (expect (parsec-with-input "PROGRAM Usage: prog --foo"
              (docopt--parse-program))
            :to-equal (docopt-make-program
                       :header "PROGRAM"
                       :usage (list (docopt-make-usage-pattern
                                     (docopt-make-command :name "prog")
                                     (docopt-make-long-option :name "foo"))))))

  (it "should parse \"Usage: prog [options]\n\nOptions: -a,--all  All.\""
    (expect (parsec-with-input "Usage: prog [options]\n\nOptions: -a,--all  All."
              (docopt--parse-program))
            :to-equal (docopt-make-program
                       :usage (list (docopt-make-usage-pattern
                                     (docopt-make-command :name "prog")
                                     (docopt-make-options-shortcut
                                      (list (docopt-make-long-option :name "all" :description "All.")
                                            (docopt-make-short-option :name "a" :description "All.")))))
                       :options (list (docopt-make-option-line
                                       :description "All."
                                       :short-name "a"
                                       :long-name "all"))))))

(describe "The docopt--parse-sep-end-by1 combinator"
  :var ((parser (lambda () (docopt--parse-sep-end-by1 (parsec-ch ?a) (parsec-ch ?\|)))))

  (it "should parse \"a\""
    (expect (parsec-with-input "a" (funcall parser)) :to-equal (list "a")))

  (it "should parse \"aa\""
    (expect (parsec-with-input "aa" (funcall parser)) :to-equal (list "a")))

  (it "should parse \"a|\""
    (expect (parsec-with-input "a|" (funcall parser))
            :to-equal (list "a")))

  (it "should parse \"a|a\""
    (expect (parsec-with-input "a|a" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should parse a|a|"
    (expect (parsec-with-input "a|a|" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should parse a|a||"
    (expect (parsec-with-input "a|a||" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should fail parsing \"\""
    (expect (parsec-with-input "" (funcall parser))
            :to-equal '(parsec-error . "Found \"`EOF'\" -> Expected \"a\"")))

  (it "should fail parsing \"b\""
    (expect (parsec-with-input "" (funcall parser))
            :to-equal '(parsec-error . "Found \"`EOF'\" -> Expected \"a\""))))

(describe "The docopt--parse-sep-end-by combinator"
  :var ((parser (lambda () (docopt--parse-sep-end-by (parsec-ch ?a) (parsec-ch ?\|)))))

  (it "should parse \"a\""
    (expect (parsec-with-input "a" (funcall parser)) :to-equal (list "a")))

  (it "should parse \"aa\""
    (expect (parsec-with-input "aa" (funcall parser)) :to-equal (list "a")))

  (it "should parse \"a|\""
    (expect (parsec-with-input "a|" (funcall parser))
            :to-equal (list "a")))

  (it "should parse \"a|a\""
    (expect (parsec-with-input "a|a" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should parse a|a|"
    (expect (parsec-with-input "a|a|" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should parse a|a||"
    (expect (parsec-with-input "a|a||" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should parse \"\""
    (expect (parsec-with-input "" (funcall parser)) :to-be nil))

  (it "should fail parsing \"b\""
    (expect (parsec-with-input "" (funcall parser)) :to-be nil)))

(describe "The docopt--parse-sep-by1 combinator"
  :var ((parser (lambda () (docopt--parse-sep-by1 (parsec-ch ?a) (parsec-ch ?\|)))))

  (it "should parse \"a\""
    (expect (parsec-with-input "a" (funcall parser)) :to-equal (list "a")))

  (it "should parse \"a|a\""
    (expect (parsec-with-input "a|a" (funcall parser))
            :to-equal (list "a" "a")))

  (it "should not parse \"\""
    (expect (parsec-with-input "" (funcall parser))
            :to-equal '(parsec-error . "Found \"`EOF'\" -> Expected \"a\"")))

  (it "should not parse \"a|\""
    (expect (parsec-with-input "a|" (funcall parser))
            :to-equal '(parsec-error . "Found \"`EOF'\" -> Expected \"a\""))))

;;; docopt-parser-test.el ends here
