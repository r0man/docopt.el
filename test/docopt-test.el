;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 's)

(defvar docopt-naval-fate-usage-ast
  (parsec-with-input "Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version"
    (docopt--parse-usage)))

(defvar docopt-naval-fate-options-ast
  (parsec-with-input "Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine."
    (docopt--parse-options)))

(defvar docopt-example-naval-fate
  (s-trim "
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
"))

(describe "Parsing naval fate"
  :var ((program (docopt-parse-program docopt-example-naval-fate)))
  (it "should parse the title"
    (expect (docopt-program-title program) :to-equal "Naval Fate"))
  (it "should parse the description"
    (expect (docopt-program-description program) :to-be nil))
  (it "should parse the usage"
    (expect (docopt-program-usage program)
            :to-equal docopt-naval-fate-usage-ast))
  (it "should parse the options"
    (expect (docopt-program-options program)
            :to-equal docopt-naval-fate-options-ast))
  (it "should parse the examples"
    (expect (docopt-program-examples program)
            :to-equal nil)))

;;; docopt-test.el ends here
