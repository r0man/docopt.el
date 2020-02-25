;;; docopt-testcase-test.el --- The Docopt testcase tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt testcase tests

;;; Code:

(require 'buttercup)
(require 'docopt-testcase)
(require 'f)

(describe "The `docopt--parse-testcase-example` parser"

  (it "should parse a single line JSON result"
    (expect (parsec-with-input "$ prog\n{\"-a\": false}\n"
              (docopt--parse-testcase-example))
            :to-equal '("prog" ((-a . :json-false)))))

  (it "should parse a multi line JSON result"
    (expect (parsec-with-input "$ prog\n{\"-a\": false,\n \"-b\": true}\n"
              (docopt--parse-testcase-example))
            :to-equal '("prog" ((-a . :json-false) (-b . t)))))

  (it "should parse a multi line nested JSON result"
    (expect (parsec-with-input "$ prog\n{\"-a\": false,\n \"-b\": {\"c\": 1}}\n"
              (docopt--parse-testcase-example))
            :to-equal '("prog" ((-a . :json-false) (-b (c . 1))))))

  (it "should parse a multi line nested JSON result with spaces"
    (expect (parsec-with-input "$ prog\n {\"-a\": false,\n \"-b\": {\"c\": 1 } }\n"
              (docopt--parse-testcase-example))
            :to-equal '("prog" ((-a . :json-false) (-b (c . 1))))))

  (it "should parse a user error"
    (expect (parsec-with-input "$ prog --xxx\n\"user-error\""
              (docopt--parse-testcase-example))
            :to-equal (list "prog --xxx" 'user-error))))

(describe "The `docopt-testcase-program` parser"

  (it "should parse a single line string"
    (expect (parsec-with-input "r\"\"\"Usage: prog [<arg>]\n\n\"\"\""
              (docopt--parse-testcase-program))
            :to-equal (docopt-parse-program "Usage: prog [<arg>]\n\n")))

  (it "should parse a multi line string"
    (expect (parsec-with-input "r\"\"\"Usage: prog [options]\n\nOptions: -a  All.\n\n\"\"\""
              (docopt--parse-testcase-program))
            :to-equal (docopt-parse-program "Usage: prog [options]\n\nOptions: -a  All.\n\n"))))

(describe "Parsing the Docopt test cases"
  :var ((test-cases (docopt-parse-testcases (f-read-text "test/testcases.docopt"))))

  (it "should return 81 test cases"
    (expect (length test-cases)
            :to-equal 81)))

;;; docopt-testcase-test.el ends here
