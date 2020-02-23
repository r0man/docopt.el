;;; test-helper.el --- The docopt test helpers -*- lexical-binding: t -*-

;;; Commentary:

;;  The docopt test helpers.

;;; Code:

(require 'parsec)
(require 's)

(defvar docopt-naval-fate-header
  "Naval Fate.\n\n")

(defvar docopt-naval-fate-usage
  "Usage:
  naval_fate ship new <name>...
  naval_fate ship <name> move <x> <y> [--speed=<kn>]
  naval_fate ship shoot <x> <y>
  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]
  naval_fate -h | --help
  naval_fate --version")

(defvar docopt-naval-fate-options
  "Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.")

(defvar docopt-naval-fate-examples
  "Examples:
  naval_fate ship new SHIP-123
  naval_fate ship SHIP-123 move 1 2 --speed=10")

(defvar docopt-naval-fate-usage-ast
  (parsec-with-input docopt-naval-fate-usage
    (docopt--parse-usage)))

(defvar docopt-naval-fate-options-ast
  (parsec-with-input docopt-naval-fate-options
    (docopt--parse-options)))

(defvar docopt-naval-fate-examples-ast
  (parsec-with-input docopt-naval-fate-examples
    (docopt--parse-examples)))

(defvar docopt-naval-fate
  (concat docopt-naval-fate-header
          docopt-naval-fate-usage "\n\n"
          docopt-naval-fate-options "\n\n"
          docopt-naval-fate-examples "\n"))

(provide 'test-helper)

;;; test-helper.el ends here
