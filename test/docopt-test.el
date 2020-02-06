;;; docopt-test.el --- The docopt tests -*- lexical-binding: t -*-

;;; Commentary:

;;  The docopt tests.

;;; Code:

(require 'buttercup)
(require 'docopt)

(defvar docopt-example "
Naval Fate.

Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.
")

(describe "Parsing tokens"

  (it "should parse \"[options]\""
    (expect (parsec-with-input "[options]" (docopt--parse-options-shortcut))
            :to-equal "[options]"))

  (it "should parse \"Examples:\""
    (expect (parsec-with-input "Examples:" (docopt--parse-examples-str))
            :to-equal "Examples:"))

  (it "should parse \"Usage:\""
    (expect (parsec-with-input "Usage:" (docopt--parse-usage-str))
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

  (it "should parse the \"my_program\" program name"
    (expect (parsec-with-input "my_program" (docopt--parse-program-name))
            :to-equal "my_program")))


(describe "The blank line parser"
  (it "should parse lines with spaces"
    (expect (parsec-with-input " \n" (docopt--parse-blank-line))
            :to-equal '(" " "\n")))

  (it "should parse lines without any spaces"
    (expect (parsec-with-input "\n" (docopt--parse-blank-line))
            :to-equal '("" "\n"))))


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


(describe "The argument parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<host>" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host")))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "HOST" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST")))

  ;; Optional

  (it "should parse an optional spaceship argument"
    (expect (parsec-with-input "[<host>]" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host" :optional t)))

  (it "should parse an optional upper case argument"
    (expect (parsec-with-input "[HOST]" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST" :optional t)))

  ;; Repeated

  (it "should parse a repeated spaceship argument"
    (expect (parsec-with-input "<host>..." (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host" :repeated t)))

  (it "should parse a repeated upper case argument"
    (expect (parsec-with-input "HOST..." (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST" :repeated t))))


(describe "The long option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "--help" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "help")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "--path PATH" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "PATH"))))

  (it "should parse an option with a space separated spaceship argument"
    (expect (parsec-with-input "--path <path>" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "path"))))

  (it "should parse an option with a \"=\" separated argument"
    (expect (parsec-with-input "--path=PATH" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "PATH"))))

  (it "should parse an option with a \"=\" separated spaceship argument"
    (expect (parsec-with-input "--path=<path>" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "path"))))

  ;; Same as above, but optional

  (it "should parse an optional option without an argument"
    (expect (parsec-with-input "[--help]" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "help" :optional t)))

  (it "should parse an optional option with a space separated argument"
    (expect (parsec-with-input "[--path PATH]" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "PATH") :optional t)))

  (it "should parse an optional option with a space separated spaceship argument"
    (expect (parsec-with-input "[--path <path>]" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "path") :optional t)))

  (it "should parse an optional option with a \"=\" separated argument"
    (expect (parsec-with-input "[--path=PATH]" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "PATH") :optional t)))

  (it "should parse an optional option with a \"=\" separated spaceship argument"
    (expect (parsec-with-input "[--path=<path>]" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option :name "path" :argument (docopt-make-argument :name "path") :optional t))))


(describe "The short option parser"

  (it "should parse an option without an argument"
    (expect (parsec-with-input "-h" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "h")))

  (it "should parse an option with a space separated argument"
    (expect (parsec-with-input "-p PATH" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "p" :argument (docopt-make-argument :name "PATH"))))

  (it "should parse an option with a not separated argument"
    (expect (parsec-with-input "-pPATH" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "p" :argument (docopt-make-argument :name "PATH"))))

  ;; Same as above, but optional

  (it "should parse an optional option without an argument"
    (expect (parsec-with-input "[-h]" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "h" :optional t)))

  (it "should parse an optional option with a space separated argument"
    (expect (parsec-with-input "[-p PATH]" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "p" :argument (docopt-make-argument :name "PATH") :optional t)))

  (it "should parse an optional option with a not separated argument"
    (expect (parsec-with-input "[-pPATH]" (docopt--parse-short-option))
            :to-equal (docopt-make-short-option :name "p" :argument (docopt-make-argument :name "PATH") :optional t))))


(describe "The option line parser"

  (it "should parse a short option without an argument"
    (expect (parsec-with-input "-h  Show this help." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line :description "Show this help." :short-name "h")))

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

  (it "should parse a long option with an argument"
    (expect (parsec-with-input "--path PATH  Path to files." (docopt--parse-option-line))
            :to-equal (docopt-make-option-line
                       :argument-name "PATH"
                       :description "Path to files."
                       :long-name "path"))))


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

;;; docopt-test.el ends here
