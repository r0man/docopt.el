;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 's)

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
  :var ((program (doctopt-parse-program docopt-example-naval-fate)))
  (it "should parse the title"
    (expect (docopt-program-title program) :to-equal "Naval Fate"))
  (it "should parse the description"
    (expect (docopt-program-description program) :to-be nil)))

;;; docopt-test.el ends here
