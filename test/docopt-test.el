;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

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
(require 'docopt)
(require 'test-helper)

(describe "Parsing naval fate"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "should parse the header"
    (expect (docopt-program-header program) :to-equal "Naval Fate."))

  (it "should parse the usage"
    (expect (docopt-program-usage program)
            :to-equal docopt-naval-fate-usage-ast))

  (it "should parse the options"
    (expect (docopt-program-options program)
            :to-equal docopt-naval-fate-options-ast))

  (it "should parse the examples"
    (expect (docopt-program-examples program)
            :to-equal '(("naval_fate" "ship" "new" "SHIP-123")
                        ("naval_fate" "ship" "SHIP-123" "move" "1" "2" "--speed=10"))))

  (it "should parse program sections: examples, options, usage"
    (expect (docopt-parse
             (concat docopt-naval-fate-header-str
                     docopt-naval-fate-examples-str "\n"
                     docopt-naval-fate-options-str "\n"
                     docopt-naval-fate-usage-str "\n"))
            :to-equal program))

  (it "should parse program sections: options, examples, usage"
    (expect (docopt-parse
             (concat docopt-naval-fate-header-str
                     docopt-naval-fate-options-str "\n"
                     docopt-naval-fate-examples-str "\n"
                     docopt-naval-fate-usage-str "\n"))
            :to-equal program))

  (it "should parse program sections: options, usage, examples"
    (expect (docopt-parse
             (concat docopt-naval-fate-header-str
                     docopt-naval-fate-options-str "\n"
                     docopt-naval-fate-usage-str "\n"
                     docopt-naval-fate-examples-str "\n"))
            :to-equal program))

  (it "should parse program sections: usage, options, examples"
    (expect (docopt-parse
             (concat docopt-naval-fate-header-str
                     docopt-naval-fate-usage-str "\n"
                     docopt-naval-fate-options-str "\n"
                     docopt-naval-fate-examples-str "\n"))
            :to-equal program)))

(describe "Parsing naval fate argument vectors"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "should parse \"naval_fate --help\""
    (expect (docopt-eval-ast program "naval_fate --help")
            :to-equal (list (docopt-long-option :object-name "help"))))

  (it "should parse \"naval_fate ship SHIP-123 move 1 2 --speed=10\""
    (expect (docopt-eval-ast program "naval_fate ship SHIP-123 move 1 2 --speed=10")
            :to-equal (list (docopt-command :object-name "ship")
                            (docopt-argument :object-name "name" :value "SHIP-123")
                            (docopt-command :object-name "move")
                            (docopt-argument :object-name "x" :value "1")
                            (docopt-argument :object-name "y" :value "2")
                            (docopt-long-option :object-name "speed" :argument (docopt-argument :object-name "kn" :value "10"))))))

;;; docopt-test.el ends here
