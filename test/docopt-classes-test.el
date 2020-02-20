;;; docopt-classes-test.el --- The Docopt class tests -*- lexical-binding: t -*-

;;; Commentary:

;; The Docopt class tests

;;; Code:

(require 'buttercup)
(require 'docopt-classes)

(describe "Concatenate the members of eithers"
  (it "should concatenate the members"
    (expect (docopt-either-concat
             (docopt-make-either (docopt-make-argument "A")
                                 (docopt-make-argument "B"))
             (docopt-make-either (docopt-make-argument "C"))
             (docopt-make-either (docopt-make-argument "D")))
            :to-equal
            (docopt-make-either
             (docopt-make-argument "A")
             (docopt-make-argument "B")
             (docopt-make-argument "C")
             (docopt-make-argument "D")))))

;;; docopt-classes-test.el ends here
