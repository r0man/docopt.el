;;; docopt-argument-parser-test.el --- The Docopt argument parser tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt argument parser tests

;;; Code:

(require 'buttercup)
(require 'docopt-argument-parser)

(describe "The `docopt-argument-parser` for an argument"

  (it "should parse a required argument"
    (expect (parsec-with-input "my-arg"
              (docopt-argument-parser (docopt-make-argument :name "ARG")))
            :to-equal "my-arg"))

  (it "should parse an optional argument"
    (expect (parsec-with-input ""
              (docopt-argument-parser (docopt-make-argument :name "ARG" :optional t)))
            :to-equal nil)

    (expect (parsec-with-input "my-arg"
              (docopt-argument-parser (docopt-make-argument :name "ARG" :optional t)))
            :to-equal "my-arg")))

;;; docopt-argument-parser-test.el ends here
