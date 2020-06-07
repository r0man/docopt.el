;;; docopt-parser-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

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

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt-parser)
(require 'test-helper)

(describe "Parsing tokens"

  (it "should parse a command name"
    (expect (parsec-with-input "naval-fate.py" (docopt-parser-command-name))
            :to-equal "naval-fate.py"))

  (it "should parse a space"
    (expect (parsec-with-input " " (docopt-parser-space)) :to-equal " "))

  (it "should parse spaces"
    (expect (parsec-with-input "  " (docopt-parser-spaces)) :to-equal "  "))

  (it "should parse a space character as white space"
    (expect (parsec-with-input " " (docopt-parser-whitespace)) :to-equal " "))

  (it "should parse a newline as white space"
    (expect (parsec-with-input "\n" (docopt-parser-whitespace)) :to-equal "\n"))

  (it "should parse CRLF as white space and return newline"
    (expect (parsec-with-input "\r\n" (docopt-parser-whitespace)) :to-equal "\n"))

  (it "should parse \"[options]\""
    (expect (parsec-with-input "[options]" (docopt-parser--options-shortcut))
            :to-equal (docopt-make-options-shortcut)))

  (it "should parse \"Examples:\""
    (expect (parsec-with-input "Examples:" (docopt-parser--examples-str))
            :to-equal "Examples:"))

  (it "should parse \"Usage:\""
    (expect (parsec-with-input "Usage:" (docopt-parser--usage-header))
            :to-equal "Usage:"))

  (it "should parse \"Options:\""
    (expect (parsec-with-input "Options:" (docopt-parser--options-str))
            :to-equal "Options:"))

  (it "should parse the \"-h\" short option name"
    (expect (parsec-with-input "-h" (docopt-parser--short-option-name))
            :to-equal "h"))

  (it "should parse the \"--help\" long option name"
    (expect (parsec-with-input "--help" (docopt-parser--long-option-name))
            :to-equal "help"))

  (it "should parse the \"--help-me\" long option name"
    (expect (parsec-with-input "--help-me" (docopt-parser--long-option-name))
            :to-equal "help-me"))

  (it "should parse a section header"
    (expect (parsec-with-input "Examples:" (docopt-parser--section-header))
            :to-equal "Examples")))

(describe "The argument parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<host>" (docopt-parser--argument))
            :to-equal (docopt-argument :name "host")))

  (it "should parse a spaceship argument containing a slash"
    (expect (parsec-with-input "<km/h>" (docopt-parser--argument))
            :to-equal (docopt-argument :name "km/h")))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "HOST" (docopt-parser--argument))
            :to-equal (docopt-argument :name "HOST")))

  (it "should parse a upper case argument containing a slash"
    (expect (parsec-with-input "KM/H" (docopt-parser--argument))
            :to-equal (docopt-argument :name "KM/H"))))

(describe "The default parser"

  (it "should handle nil"
    (expect (docopt-parser--default nil) :to-be nil))

  (it "should parse a decimal as a default"
    (expect (docopt-parser--default "[default: 2.95]") :to-equal "2.95"))

  (it "should parse a default without spaces"
    (expect (docopt-parser--default "[default:2.95]") :to-equal "2.95"))

  (it "should parse a default with spaces"
    (expect (docopt-parser--default "[default:  2.95  ]") :to-equal "2.95"))

  (it "should parse a filename as a default"
    (expect (docopt-parser--default "[default: test.txt]") :to-equal "test.txt"))

  (it "should parse the current directory as a default"
    (expect (docopt-parser--default "[default: ./]") :to-equal "./"))

  (it "should parse multiple defaults"
    (expect (docopt-parser--default "[default: x y]") :to-equal ["x" "y"])))


(describe "The long option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "--help" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "help")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "--path PATH" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "path" :argument (docopt-argument :name "PATH"))))

  (it "should parse an option with a space separated spaceship argument"
    (expect (parsec-with-input "--path <path>" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "path" :argument (docopt-argument :name "path"))))

  (it "should parse an option with a \"=\" separated upper case argument"
    (expect (parsec-with-input "--path=PATH" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "path" :argument (docopt-argument :name "PATH"))))

  (it "should parse an option with a \"=\" separated lower case argument"
    (expect (parsec-with-input "--path=path" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "path" :argument (docopt-argument :name "path"))))

  (it "should parse an option with a \"=\" separated spaceship argument"
    (expect (parsec-with-input "--path=<path>" (docopt-parser--long-option))
            :to-equal (docopt-long-option :name "path" :argument (docopt-argument :name "path")))))


(describe "The short option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "-h" (docopt-parser--short-option))
            :to-equal (docopt-short-option :name "h")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "-p PATH" (docopt-parser--short-option))
            :to-equal (docopt-short-option :name "p" :argument (docopt-argument :name "PATH"))))

  (it "should parse an option with a equal sign separated argument"
    (expect (parsec-with-input "-p=PATH" (docopt-parser--short-option))
            :to-equal (docopt-short-option :name "p" :argument (docopt-argument :name "PATH"))))

  (it "should parse an option with a not separated argument"
    (expect (parsec-with-input "-pPATH" (docopt-parser--short-option))
            :to-equal (docopt-short-option :name "p" :argument (docopt-argument :name "PATH")))))

(describe "The option line description parser"
  (it "should parse single-line descriptions"
    (expect (parsec-with-input "Show version.\n  More version help."
              (docopt-parser--option-line-description))
            :to-equal "Show version.\n  More version help."))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat "Show version.\n"
                        "  More version help.\n"
                        "  --moored      Moored (anchored) mine.\n")
              (docopt-parser--option-line-description))
            :to-equal "Show version.\n  More version help.")))

(describe "The option line options parser"

  (it "should parse short option only"
    (expect (parsec-with-input "-h  Show this help." (docopt-parser--option-line-options))
            :to-equal (list nil (docopt-short-option :name "h"))))

  (it "should parse long option only"
    (expect (parsec-with-input "--help  Show this help." (docopt-parser--option-line-options))
            :to-equal (list (docopt-long-option :name "help") nil)))

  (it "should parse short options first"
    (expect (parsec-with-input "-h, --help  Show this help." (docopt-parser--option-line-options))
            :to-equal (list (docopt-long-option :name "help")
                            (docopt-short-option :name "h"))))

  (it "should parse short options first with minimal spacing"
    (expect (parsec-with-input "-h,--help Show this help." (docopt-parser--option-line-options))
            :to-equal (parsec-with-input "-h,--help Show this help." (docopt-parser--option-line-options))))

  (it "should parse long options first"
    (expect (parsec-with-input "--help, -h  Show this help." (docopt-parser--option-line-options))
            :to-equal (list (docopt-long-option :name "help")
                            (docopt-short-option :name "h"))))

  (it "should parse long options first with minimal spacing"
    (expect (parsec-with-input "--help,-h Show this help." (docopt-parser--option-line-options))
            :to-equal (list (docopt-long-option :name "help")
                            (docopt-short-option :name "h")))))

(describe "The option line parser"

  (it "should parse a short option without an argument"
    (expect (parsec-with-input " -h  Show this help." (docopt-parser--option-line))
            :to-equal (docopt-make-options :description "Show this help." :short-name "h")))

  (it "should parse a short and long option without an argument"
    (expect (parsec-with-input " -h, --help  Show this help." (docopt-parser--option-line))
            :to-equal (docopt-make-options :description "Show this help." :long-name "help" :short-name "h")))

  (it "should parse a short option with a space separated argument"
    (expect (parsec-with-input " -p PATH  Path to files." (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :argument-name "PATH"
                       :description "Path to files."
                       :short-name "p")))

  (it "should parse a short option with a not separated argument"
    (expect (parsec-with-input " -pPATH  Path to files." (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :argument-name "PATH"
                       :description "Path to files."
                       :short-name "p")))

  (it "should parse a long option without an argument"
    (expect (parsec-with-input " --help  Show this help." (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :description "Show this help."
                       :long-name "help")))

  (it "should parse a long and shortcut option without an argument"
    (expect (parsec-with-input " --help, -h  Show this help." (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :description "Show this help."
                       :long-name "help"
                       :short-name "h")))

  (it "should parse a long option with an argument"
    (expect (parsec-with-input " --path PATH  Path to files." (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :argument-name "PATH"
                       :description "Path to files."
                       :long-name "path")))

  (it "should parse a short option with a default argument"
    (expect (parsec-with-input " -c K  The K coefficient [default: 2.95]" (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :argument (docopt-argument :name "K")
                       :description "The K coefficient [default: 2.95]"
                       :default "2.95"
                       :short-name "c")))

  (it "should parse a long option with a default argument"
    (expect (parsec-with-input " --coefficient=K  The K coefficient [default: 2.95]" (docopt-parser--option-line))
            :to-equal (docopt-make-options
                       :argument (docopt-argument :name "K")
                       :description "The K coefficient [default: 2.95]"
                       :default "2.95"
                       :long-name "coefficient"))))

(describe "The option lines parser"

  (it "should parse single-line descriptions"
    (expect (parsec-with-input
                (concat " --moored      Moored (anchored) mine.\n"
                        " --drifting    Drifting mine.")
              (docopt-parser--option-lines))
            :to-equal (list (docopt-long-option
                             :description "Moored (anchored) mine."
                             :name "moored")
                            (docopt-long-option
                             :description "Drifting mine."
                             :name "drifting"))))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat " --moored      Moored (anchored) mine.\n"
                        " --drifting    Drifting mine.\n"
                        " --version     Show version."
                        "                More version help.")
              (docopt-parser--option-lines))
            :to-equal (list (docopt-long-option
                             :description "Moored (anchored) mine."
                             :name "moored")
                            (docopt-long-option
                             :description "Drifting mine."
                             :name "drifting")
                            (docopt-long-option
                             :description "Show version.                More version help."
                             :name "version")))))


(describe "Parsing stacked short options"
  (it "should return a list of short options"
    (expect (parsec-with-input "-abc" (docopt-parser--short-options-stacked))
            :to-equal (list (docopt-short-option :name "a")
                            (docopt-short-option :name "b")
                            (docopt-short-option :name "c")))))

(describe "The usage pattern expression parser"

  (it "should parse standard input"
    (expect (parsec-with-input "[-]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-standard-input))))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "ARG" (docopt-parser--usage-expr))
            :to-equal (list (docopt-argument :name "ARG"))))

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<ARG>" (docopt-parser--usage-expr))
            :to-equal (list (docopt-argument :name "ARG"))))

  (it "should parse a spaceship argument with double colon"
    (expect (parsec-with-input "<host:port>" (docopt-parser--usage-expr))
            :to-equal (list (docopt-argument :name "host:port"))))

  (it "should parse a spaceship argument with whitespace"
    (expect (parsec-with-input "<input file>" (docopt-parser--usage-expr))
            :to-equal (list (docopt-argument :name "input file"))))

  (it "should parse a repeatable argument"
    (expect (parsec-with-input "ARG..." (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-repeated (docopt-argument :name "ARG")))))

  (it "should parse a repeatable argument with whitespace"
    (expect (parsec-with-input "ARG ..." (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-repeated (docopt-argument :name "ARG")))))

  (it "should parse an optional spaceship argument"
    (expect (parsec-with-input "[<ARG>]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-optional-group (docopt-argument :name "ARG")))))

  (it "should parse a required spaceship argument"
    (expect (parsec-with-input "(<ARG>)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-argument :name "ARG")))))

  (it "should parse an optional upper case argument"
    (expect (parsec-with-input "[ARG]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-optional-group (docopt-argument :name "ARG")))))

  (it "should parse an optional with multiple members"
    (expect (parsec-with-input "[ARG-1 ARG-2]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-argument :name "ARG-1")
                             (docopt-argument :name "ARG-2")))))

  (it "should parse a required group with upper case argument"
    (expect (parsec-with-input "(ARG)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-argument :name "ARG")))))

  (it "should parse a required group with upper case argument and whitespace"
    (expect (parsec-with-input "(  ARG  )" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group (docopt-argument :name "ARG")))))

  (it "should parse a required group with multiple members"
    (expect (parsec-with-input "(ARG-1 ARG-2)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-argument :name "ARG-1")
                             (docopt-argument :name "ARG-2")))))

  (it "should parse stacked short options"
    (expect (parsec-with-input "-abc" (docopt-parser--usage-expr))
            :to-equal (parsec-with-input "-a -b -c" (docopt-parser--usage-expr))))

  (it "should parse optional stacked short options"
    (expect (parsec-with-input "[-abc]" (docopt-parser--usage-expr))
            :to-equal  (parsec-with-input "[-a -b -c]" (docopt-parser--usage-expr))))

  (it "should parse required stacked short options"
    (expect (parsec-with-input "(-abc)" (docopt-parser--usage-expr))
            :to-equal (parsec-with-input "(-a -b -c)" (docopt-parser--usage-expr))))

  (it "should parse an optional long option"
    (expect (parsec-with-input "[--help TOPIC]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-long-option :name "help" :argument (docopt-argument :name "TOPIC"))))))

  (it "should parse a required long option"
    (expect (parsec-with-input "(--help TOPIC)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-long-option :name "help" :argument (docopt-argument :name "TOPIC"))))))

  (it "should parse a repeatable long option"
    (expect (parsec-with-input "--help TOPIC..." (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-repeated
                             (docopt-long-option :name "help" :argument (docopt-argument :name "TOPIC"))))))

  (it "should parse an optional short option"
    (expect (parsec-with-input "[-h]" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-optional-group
                             (docopt-short-option :name "h")))))

  (it "should parse a required short option"
    (expect (parsec-with-input "(-h)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-short-option :name "h")))))

  (it "should parse mutually exclusive options"
    (expect (parsec-with-input "-h | --help" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-either
                             (list (docopt-short-option :name "h"))
                             (list (docopt-long-option :name "help"))))))

  (it "should parse nested expressions"
    (expect (parsec-with-input "(N [M | (K | L)] | O P)" (docopt-parser--usage-expr))
            :to-equal (list (docopt-make-required-group
                             (docopt-make-either
                              (list (docopt-argument :name "N")
                                    (docopt-make-optional-group
                                     (docopt-make-either
                                      (list (docopt-argument :name "M"))
                                      (list (docopt-make-required-group
                                             (docopt-make-either
                                              (list (docopt-argument :name "K"))
                                              (list (docopt-argument :name "L"))))))))
                              (list (docopt-argument :name "O")
                                    (docopt-argument :name "P"))))))))

(describe "The usage pattern parser"

  (it "should parse program only"
    (expect (parsec-with-input "Usage: prog"
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "prog")))))

  (it "should parse the options shortcut"
    (expect (parsec-with-input "Usage: prog [options]"
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "prog")
                             (docopt-make-options-shortcut)))))

  (it "should parse a spaceship argument"
    (expect (parsec-with-input
                (concat "Usage: naval-fate ship new <name>...\n"
                        "       naval-fate ship <name> move <x> <y> [--speed=<kn>]")
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "naval-fate")
                             (docopt-command :name "ship")
                             (docopt-command :name "new")
                             (docopt-make-repeated (docopt-argument :name "name")))
                            (docopt-make-usage-pattern
                             (docopt-command :name "naval-fate")
                             (docopt-command :name "ship")
                             (docopt-argument :name "name")
                             (docopt-command :name "move")
                             (docopt-argument :name "x")
                             (docopt-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-long-option :name "speed" :argument (docopt-argument :name "kn")))))))

  (it "should parse \"Usage: naval-fate -h | --help\""
    (expect (parsec-with-input "Usage: naval-fate -h | --help"
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "naval-fate")
                             (docopt-make-either
                              (list (docopt-short-option :name "h"))
                              (list (docopt-long-option :name "help")))))))

  (it "should parse \"Usage: naval-fate mine (set | remove all) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval-fate mine (set many | remove all) <x> <y> [--moored|--drifting]"
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "naval-fate")
                             (docopt-command :name "mine")
                             (docopt-make-required-group
                              (docopt-make-either
                               (list (docopt-command :name "set")
                                     (docopt-command :name "many"))
                               (list (docopt-command :name "remove")
                                     (docopt-command :name "all"))))
                             (docopt-argument :name "x")
                             (docopt-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-make-either
                               (list (docopt-long-option :name "moored"))
                               (list (docopt-long-option :name "drifting"))))))))

  (it "should parse \"Usage: naval-fate mine (set|remove) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval-fate mine (set|remove) <x> <y> [--moored|--drifting]"
              (docopt-parser--usage))
            :to-equal (list (docopt-make-usage-pattern
                             (docopt-command :name "naval-fate")
                             (docopt-command :name "mine")
                             (docopt-make-required-group
                              (docopt-make-either
                               (list (docopt-command :name "set"))
                               (list (docopt-command :name "remove"))))
                             (docopt-argument :name "x")
                             (docopt-argument :name "y")
                             (docopt-make-optional-group
                              (docopt-make-either
                               (list (docopt-long-option :name "moored"))
                               (list (docopt-long-option :name "drifting")))))))))

(describe "The program parser"

  (it "should parse \"PROGRAM Usage: prog --foo\""
    (expect (parsec-with-input "PROGRAM Usage: prog --foo"
              (docopt-parser-program))
            :to-equal (docopt-program
                       :header "PROGRAM"
                       :name "prog"
                       :usage (list (docopt-make-usage-pattern
                                     (docopt-command :name "prog")
                                     (docopt-long-option :name "foo" :prefixes '("fo" "f"))))
                       :options (list (docopt-long-option :name "foo" :prefixes '("fo" "f"))))))

  (it "should parse \"Usage: prog [options]\n\nOptions: -a,--all  All.\""
    (expect (parsec-with-input "Usage: prog [options]\n\nOptions: -a,--all  All."
              (docopt-parser-program))
            :to-equal (docopt-program
                       :name "prog"
                       :usage (list (docopt-make-usage-pattern
                                     (docopt-command :name "prog")
                                     (docopt-make-options-shortcut
                                      (docopt-long-option :name "all" :description "All." :synonym "a" :prefixes '("al" "a"))
                                      (docopt-short-option :name "a" :description "All." :synonym "all"))))
                       :options (list (docopt-short-option :name "a" :description "All." :synonym "all")
                                      (docopt-long-option :name "all" :description "All." :synonym "a" :prefixes '("al" "a")))))))

(describe "Parsing naval fate"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "should parse the name"
    (expect (docopt-program-name program) :to-equal "naval-fate"))

  (it "should parse the header"
    (expect (docopt-program-header program) :to-equal "Naval Fate."))

  (it "should parse the usage"
    (expect (docopt-program-usage program)
            :to-equal (list docopt-naval-fate-pattern-ship-new
                            docopt-naval-fate-pattern-ship-name
                            docopt-naval-fate-pattern-ship-shoot
                            docopt-naval-fate-pattern-mine
                            docopt-naval-fate-pattern-help
                            docopt-naval-fate-pattern-version)))

  (it "should parse the options"
    (expect (docopt-program-options program)
            :to-equal (list docopt-naval-fate-option-drifting
                            docopt-naval-fate-option-h
                            docopt-naval-fate-option-help
                            docopt-naval-fate-option-moored
                            docopt-naval-fate-option-speed
                            docopt-naval-fate-option-version)))

  (it "should parse the examples"
    (expect (docopt-program-examples program)
            :to-equal '(("naval-fate" "ship" "new" "SHIP-123")
                        ("naval-fate" "ship" "SHIP-123" "move" "1" "2" "--speed=10")))))

(describe "Parsing naval fate with different section order"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "should parse program sections: examples, options, usage"
    (expect (docopt-equal
             program (docopt-parse
                      (concat docopt-naval-fate-header-str
                              docopt-naval-fate-examples-str "\n"
                              docopt-naval-fate-options-str "\n"
                              docopt-naval-fate-usage-str "\n")))
            :to-equal t))

  (it "should parse program sections: options, examples, usage"
    (expect (docopt-equal
             program (docopt-parse
                      (concat docopt-naval-fate-header-str
                              docopt-naval-fate-options-str "\n"
                              docopt-naval-fate-examples-str "\n"
                              docopt-naval-fate-usage-str "\n")))
            :to-equal t))

  (it "should parse program sections: options, usage, examples"
    (expect (docopt-equal
             program (docopt-parse
                      (concat docopt-naval-fate-header-str
                              docopt-naval-fate-options-str "\n"
                              docopt-naval-fate-usage-str "\n"
                              docopt-naval-fate-examples-str "\n")))
            :to-equal t))

  (it "should parse program sections: usage, options, examples"
    (expect (docopt-equal
             program (docopt-parse
                      (concat docopt-naval-fate-header-str
                              docopt-naval-fate-usage-str "\n"
                              docopt-naval-fate-options-str "\n"
                              docopt-naval-fate-examples-str "\n")))
            :to-equal t)))

(describe "The docopt-parser--sep-end-by1 combinator"
  :var ((parser (lambda () (docopt-parser--sep-end-by1 (parsec-ch ?a) (parsec-ch ?\|)))))

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

(describe "The docopt-parser--sep-end-by combinator"
  :var ((parser (lambda () (docopt-parser--sep-end-by (parsec-ch ?a) (parsec-ch ?\|)))))

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

(describe "The docopt-parser--sep-by1 combinator"
  :var ((parser (lambda () (docopt-parser--sep-by1 (parsec-ch ?a) (parsec-ch ?\|)))))

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
