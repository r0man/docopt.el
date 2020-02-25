;;; docopt-test.el --- The Docopt parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt parser tests

;;; Code:

(require 'buttercup)
(require 'docopt)
(require 'test-helper)

(describe "Parsing naval fate"
  :var ((program (docopt-parse-program docopt-naval-fate)))

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

(describe "Parsing naval fate argument vectors"
  :var ((program (docopt-parse-program docopt-naval-fate)))

  (it "should parse \"naval_fate --help\""
    (expect (doctopt-parse-argv program "naval_fate --help")
            :to-equal (list (docopt-make-command :name "naval_fate")
                            (docopt-make-long-option :name "help"))))

  (it "should parse \"naval_fate ship SHIP-123 move 1 2 --speed=10\""
    (expect (doctopt-parse-argv program "naval_fate ship SHIP-123 move 1 2 --speed=10")
            :to-equal (list (docopt-make-command :name "naval_fate")
                            (list (docopt-make-command :name "ship")
                                  (docopt-make-argument :name "name" :value "SHIP-123")
                                  (docopt-make-command :name "move")
                                  (docopt-make-argument :name "x" :value "1")
                                  (docopt-make-argument :name "y" :value "2")
                                  (docopt-make-long-option
                                   :name "speed"
                                   :argument (docopt-make-argument :name "kn" :value "10")
                                   :optional t))))))

;;; docopt-test.el ends here
