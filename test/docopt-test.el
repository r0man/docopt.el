;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 'test-helper)

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
