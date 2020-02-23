;;; test-helper.el --- The docopt test helpers -*- lexical-binding: t -*-

;;; Commentary:

;;  The docopt test helpers.

;;; Code:

(require 'parsec)
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

(provide 'test-helper)

;;; test-helper.el ends here
