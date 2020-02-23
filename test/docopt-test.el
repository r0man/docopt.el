;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 'test-helper)

(describe "Parsing naval fate"
  :var ((program (docopt-parse-program docopt-naval-fate)))

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
            :to-equal '(("naval_fate" "ship" "new" "SHIP-123")
                        ("naval_fate" "ship" "SHIP-123" "move" "1" "2" "--speed=10"))))

  (it "should parse program sections: examples, options, usage"
    (expect (docopt-parse-program
             (concat docopt-naval-fate-header
                     docopt-naval-fate-examples "\n"
                     docopt-naval-fate-options "\n"
                     docopt-naval-fate-usage "\n"))
            :to-equal program))

  (it "should parse program sections: options, examples, usage"
    (expect (docopt-parse-program
             (concat docopt-naval-fate-header
                     docopt-naval-fate-options "\n"
                     docopt-naval-fate-examples "\n"
                     docopt-naval-fate-usage "\n"))
            :to-equal program))

  (it "should parse program sections: options, usage, examples"
    (expect (docopt-parse-program
             (concat docopt-naval-fate-header
                     docopt-naval-fate-options "\n"
                     docopt-naval-fate-usage "\n"
                     docopt-naval-fate-examples "\n"))
            :to-equal program))

  (it "should parse program sections: usage, options, examples"
    (expect (docopt-parse-program
             (concat docopt-naval-fate-header
                     docopt-naval-fate-usage "\n"
                     docopt-naval-fate-options "\n"
                     docopt-naval-fate-examples "\n"))
            :to-equal program)))

;;; docopt-test.el ends here
