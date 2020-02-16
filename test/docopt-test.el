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
            :to-equal "[options]"))

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
            :to-equal "help-me")))

(describe "The argument parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<host>" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host")))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "HOST" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST"))))


(describe "The mutually exclusive parser"

  (it "should parse empty lines"
    (expect (parsec-with-input "" (docopt--parse-mutually-exclusive))
            :to-be nil))

  (it "should parse a single short option"
    (expect (parsec-with-input "-h" (docopt--parse-mutually-exclusive))
            :to-equal (list (docopt-make-short-option :name "h"))))

  (it "should parse a single long option"
    (expect (parsec-with-input "--speed=<kn>" (docopt--parse-mutually-exclusive))
            :to-equal (list (docopt-make-long-option
                             :argument (docopt-make-argument :name "kn")
                             :name "speed"))))

  (it "should parse multiple options"
    (expect (parsec-with-input "-h | --speed=<kn>" (docopt--parse-mutually-exclusive))
            :to-equal (list (docopt-make-short-option :name "h")
                            (docopt-make-long-option
                             :argument (docopt-make-argument :name "kn")
                             :name "speed")))))


(describe "The command parser"
  (it "should parse multiple arguments"
    (expect (parsec-with-input "my_program <host> <port>" (docopt--parse-command))
            :to-equal (docopt-make-command
                       :name "my_program"
                       :arguments (list (docopt-make-argument :name "host")
                                        (docopt-make-argument :name "port"))))))

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

  (it "should parse an option with a \"=\" separated argument"
    (expect (parsec-with-input "--path=PATH" (docopt--parse-long-option))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "PATH")
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

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "ARG" (docopt--parse-expr))
            :to-equal (docopt-make-argument :name "ARG")))

  (it "should parse a repeatable argument"
    (expect (parsec-with-input "ARG..." (docopt--parse-expr))
            :to-equal (docopt-make-argument :name "ARG" :repeated t)))

  (it "should parse an optional spaceship argument"
    (expect (parsec-with-input "[<ARG>]" (docopt--parse-expr))
            :to-equal (list (docopt-make-argument :name "ARG" :optional t))))

  (it "should parse a required spaceship argument"
    (expect (parsec-with-input "(<ARG>)" (docopt--parse-expr))
            :to-equal (list (docopt-make-argument :name "ARG"))))

  (it "should parse an optional upper case argument"
    (expect (parsec-with-input "[ARG]" (docopt--parse-expr))
            :to-equal (list (docopt-make-argument :name "ARG" :optional t))))

  (it "should parse a required upper case argument"
    (expect (parsec-with-input "(ARG)" (docopt--parse-expr))
            :to-equal (list (docopt-make-argument :name "ARG"))))

  (it "should parse stacked short options"
    (expect (parsec-with-input "-abc" (docopt--parse-expr))
            :to-equal (parsec-with-input "-abc" (docopt--parse-short-options-stacked))))

  (it "should parse optional stacked short options"
    (expect (parsec-with-input "[-abc]" (docopt--parse-expr))
            :to-equal (list (docopt-make-short-option :name "a" :optional t)
                            (docopt-make-short-option :name "b" :optional t)
                            (docopt-make-short-option :name "c" :optional t))))

  (it "should parse required stacked short options"
    (expect (parsec-with-input "(-abc)" (docopt--parse-expr))
            :to-equal (parsec-with-input "-abc" (docopt--parse-expr))))

  (it "should parse an required group"
    (expect (parsec-with-input "(-abc)" (docopt--parse-expr))
            :to-equal (parsec-with-input "-abc" (docopt--parse-short-options-stacked))))

  (it "should parse an optional long option"
    (expect (parsec-with-input "[--help TOPIC]" (docopt--parse-expr))
            :to-equal (list (docopt-make-long-option
                             :argument (docopt-make-argument :name "TOPIC")
                             :name "help"
                             :optional t))))

  (it "should parse a required long option"
    (expect (parsec-with-input "(--help TOPIC)" (docopt--parse-expr))
            :to-equal (list (docopt-make-long-option
                             :argument (docopt-make-argument :name "TOPIC")
                             :name "help"))))

  (it "should parse a repeatable long option"
    (expect (parsec-with-input "--help TOPIC..." (docopt--parse-expr))
            :to-equal (docopt-make-long-option
                       :argument (docopt-make-argument :name "TOPIC")
                       :name "help"
                       :repeated t)))

  (it "should parse an optional short option"
    (expect (parsec-with-input "[-h]" (docopt--parse-expr))
            :to-equal (list (docopt-make-short-option :name "h" :optional t))))

  (it "should parse a required short option"
    (expect (parsec-with-input "(-h)" (docopt--parse-expr))
            :to-equal (list (docopt-make-short-option :name "h"))))

  (it "should parse mutually exclusive options"
    (expect (parsec-with-input "-h | --help" (docopt--parse-expr))
            :to-equal (list (docopt-make-short-option :name "h")
                            (docopt-make-long-option :name "help")))))

(describe "The usage pattern parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input
                (concat "Usage: naval_fate ship new <name>...\n"
                        "       naval_fate ship <name> move <x> <y> [--speed=<kn>]")
              (docopt--parse-usage-patterns))
            :to-equal (list (list (docopt-make-command :name "naval_fate")
                                  (docopt-make-command :name "ship")
                                  (docopt-make-command :name "new")
                                  (docopt-make-argument :name "name" :repeated t))
                            (list (docopt-make-command :name "naval_fate")
                                  (docopt-make-command :name "ship")
                                  (docopt-make-argument :name "name")
                                  (docopt-make-command :name "move")
                                  (docopt-make-argument :name "x")
                                  (docopt-make-argument :name "y")
                                  (list (docopt-make-long-option
                                         :argument (docopt-make-argument :name "kn")
                                         :name "speed"
                                         :optional t))))))

  (it "should parse \"Usage: naval_fate -h | --help\""
    (expect (parsec-with-input "Usage: naval_fate -h | --help"
              (docopt--parse-usage-patterns))
            :to-equal (list (list (docopt-make-command :name "naval_fate")
                                  (list (docopt-short-option :name "h")
                                        (docopt-long-option :name "help"))))))

  (it "should parse \"Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
              (docopt--parse-usage-patterns))
            :to-equal (list (list (docopt-make-command :name "naval_fate")
                                  (docopt-make-command :name "mine")
                                  (list (docopt-make-command :name "set")
                                        (docopt-make-command :name "remove"))
                                  (docopt-make-argument :name "x")
                                  (docopt-make-argument :name "y")
                                  (list (docopt-make-long-option :name "moored" :optional t)
                                        (docopt-make-long-option :name "drifting" :optional t)))))))

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

;;; docopt-test.el ends here
