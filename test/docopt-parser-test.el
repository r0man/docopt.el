;;; docopt-parser-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

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
            :to-equal "help-me"))

  (it "should parse a section header"
    (expect (parsec-with-input "Examples:" (docopt--parse-section-header))
            :to-equal "Examples")))

(describe "The argument parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<host>" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "host")))

  (it "should parse an upper case argument"
    (expect (parsec-with-input "HOST" (docopt--parse-argument))
            :to-equal (docopt-make-argument :name "HOST"))))

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

  (it "should parse an upper case argument"
    (expect (parsec-with-input "ARG" (docopt--parse-usage-expr))
            :to-equal (list (list (docopt-make-argument :name "ARG")))))

  (it "should parse a spaceship argument"
    (expect (parsec-with-input "<ARG>" (docopt--parse-usage-expr))
            :to-equal (list (list (docopt-make-argument :name "ARG")))))

  (it "should parse a repeatable argument"
    (expect (parsec-with-input "ARG..." (docopt--parse-usage-expr))
            :to-equal (list (list (docopt-make-argument :name "ARG" :repeated t)))))

  (it "should parse an optional spaceship argument"
    (expect (parsec-with-input "[<ARG>]" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-argument :name "ARG" :optional t)))))))

  (it "should parse a required spaceship argument"
    (expect (parsec-with-input "(<ARG>)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-argument :name "ARG")))))))

  (it "should parse an optional upper case argument"
    (expect (parsec-with-input "[ARG]" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-argument :name "ARG" :optional t)))))))

  (it "should parse a required upper case argument"
    (expect (parsec-with-input "(ARG)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-argument :name "ARG")))))))

  (it "should parse stacked short options"
    (expect (parsec-with-input "-abc" (docopt--parse-usage-expr))
            :to-equal (list (list (list (docopt-make-short-option :name "a")
                                        (docopt-make-short-option :name "b")
                                        (docopt-make-short-option :name "c"))))))

  (it "should parse optional stacked short options"
    (expect (parsec-with-input "[-abc]" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (list (docopt-make-short-option :name "a" :optional t)
                                                    (docopt-make-short-option :name "b" :optional t)
                                                    (docopt-make-short-option :name "c" :optional t))))))))

  (it "should parse required stacked short options"
    (expect (parsec-with-input "(-abc)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (list (docopt-make-short-option :name "a")
                                                    (docopt-make-short-option :name "b")
                                                    (docopt-make-short-option :name "c"))))))))

  (it "should parse an optional long option"
    (expect (parsec-with-input "[--help TOPIC]" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-long-option
                                               :argument (docopt-make-argument :name "TOPIC")
                                               :name "help"
                                               :optional t)))))))

  (it "should parse a required long option"
    (expect (parsec-with-input "(--help TOPIC)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-long-option
                                               :argument (docopt-make-argument :name "TOPIC")
                                               :name "help")))))))

  (it "should parse a repeatable long option"
    (expect (parsec-with-input "--help TOPIC..." (docopt--parse-usage-expr))
            :to-equal (list (list (docopt-make-long-option
                                   :argument (docopt-make-argument :name "TOPIC")
                                   :name "help"
                                   :repeated t)))))

  (it "should parse an optional short option"
    (expect (parsec-with-input "[-h]" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-short-option :name "h" :optional t)))))))

  (it "should parse a required short option"
    (expect (parsec-with-input "(-h)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-short-option :name "h")))))))

  (it "should parse mutually exclusive options"
    (expect (parsec-with-input "-h | --help" (docopt--parse-usage-expr))
            :to-equal (list (list (docopt-make-short-option :name "h"))
                            (list (docopt-make-long-option :name "help")))))

  (it "should parse nested expressions"
    (expect (parsec-with-input "(N [M | (K | L)] | O P)" (docopt--parse-usage-expr))
            :to-equal (list (list (list (list (docopt-make-argument :name "N")
                                              (list (list (docopt-make-argument :name "M"))
                                                    (list (list (list (docopt-make-argument :name "K"))
                                                                (list (docopt-make-argument :name "L"))))))
                                        (list (docopt-make-argument :name "O")
                                              (docopt-make-argument :name "P"))))))))

(describe "The usage pattern parser"

  (it "should parse a spaceship argument"
    (expect (parsec-with-input
                (concat "Usage: naval_fate ship new <name>...\n"
                        "       naval_fate ship <name> move <x> <y> [--speed=<kn>]")
              (docopt--parse-usage))
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
                                  (list (list (docopt-make-long-option
                                               :argument (docopt-make-argument :name "kn")
                                               :name "speed"
                                               :optional t)))))))

  (it "should parse \"Usage: naval_fate -h | --help\""
    (expect (parsec-with-input "Usage: naval_fate -h | --help"
              (docopt--parse-usage))
            :to-equal (list (list (docopt-make-command :name "naval_fate")
                                  (docopt-short-option :name "h")
                                  (docopt-long-option :name "help")))))

  (it "should parse \"Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
    (expect (parsec-with-input "Usage: naval_fate mine (set|remove) <x> <y> [--moored|--drifting]"
              (docopt--parse-usage))
            :to-equal (list (list (docopt-make-command :name "naval_fate")
                                  (docopt-command :name "mine")
                                  (list (list (docopt-make-command :name "set"))
                                        (list (docopt-make-command :name "remove")))
                                  (docopt-make-argument :name "x")
                                  (docopt-make-argument :name "y")
                                  (list (list (docopt-make-long-option :name "moored" :optional t))
                                        (list (docopt-make-long-option :name "drifting" :optional t))))))))

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
