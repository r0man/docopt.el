;;; docopt-test.el --- The docopt tests -*- lexical-binding: t -*-

;;; Commentary:

;;  The docopt tests.

;;; Code:

(require 'buttercup)
(require 'docopt)

(describe "Parsing"
  (it "should parse \"Examples\""
    (expect (parsec-with-input "Examples:" (docopt-examples-str))
            :to-equal "Examples:"))

  (it "should parse \"Usage\""
    (expect (parsec-with-input "Usage:" (docopt-usage-str))
            :to-equal "Usage:"))

  (it "should parse \"Options\""
    (expect (parsec-with-input "Options:" (docopt-options-str))
            :to-equal "Options:"))

  (it "should parse the \"-h\" short option name"
    (expect (parsec-with-input "-h" (docopt-short-option-name))
            :to-equal "-h"))

  (it "should parse the \"--help\" long option name"
    (expect (parsec-with-input "--help" (docopt-long-option-name))
            :to-equal "help"))

  (it "should parse the \"<name>\" argument"
    (expect (parsec-with-input "<name>" (docopt-argument))
            :to-equal "name"))

  (it "should parse the \"my_program\" program name"
    (expect (parsec-with-input "my_program" (docopt-program-name))
            :to-equal "my_program")))

(describe "The short option parser"
  (it "should parse \"-f FILE\""
    (expect (parsec-with-input "-f FILE" (docopt-short-option))
            :to-equal '("-f" " " "FILE")))

  (it "should parse \"-fFILE\""
    (expect (parsec-with-input "-fFILE" (docopt-short-option))
            :to-equal '("-f" nil "FILE"))))

(describe "The long option parser"
  (it "should parse \"--input\""
    (expect (parsec-with-input "--input" (docopt-long-option))
            :to-equal "input"))

  (it "should parse \"--input=ARG\""
    (expect (parsec-with-input "--input=ARG" (docopt-long-option))
            :to-equal '("input" "=" "ARG"))))

(describe "The blank line parser"
  (it "should parse lines with spaces"
    (expect (parsec-with-input " \n" (docopt-blank-line))
            :to-equal '(" " "\n")))

  (it "should parse lines without any spaces"
    (expect (parsec-with-input "\n" (docopt-blank-line))
            :to-equal '("" "\n"))))

(describe "The option description parser"
  (it "should parse single-line descriptions"
    (expect (parsec-with-input "Show version.\n  More version help."
              (docopt--parse-option-description))
            :to-equal "Show version.\n  More version help."))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat "Show version.\n"
                        "  More version help.\n"
                        "  --moored      Moored (anchored) mine.\n")
              (docopt--parse-option-description))
            :to-equal "Show version.\n  More version help.")))

(describe "The option lines parser"
  (it "should parse single-line descriptions"
    (expect (parsec-with-input
                (concat "  --moored      Moored (anchored) mine.\n"
                        "  --drifting    Drifting mine.")
              (docopt-option-lines))
            :to-equal (list (docopt-make-option "Moored (anchored) mine." "moored")
                            (docopt-make-option "Drifting mine." "drifting"))))

  (it "should parse multi-line descriptions"
    (expect (parsec-with-input
                (concat "  --moored      Moored (anchored) mine.\n"
                        "  --drifting    Drifting mine.\n"
                        "  --version     Show version."
                        "                More version help.")
              (docopt-option-lines))
            :to-equal (list (docopt-make-option "Moored (anchored) mine." "moored")
                            (docopt-make-option "Drifting mine." "drifting")
                            (docopt-make-option (concat "Show version.                More version help.")
                                                "version")))))

;;; docopt-test.el ends here
