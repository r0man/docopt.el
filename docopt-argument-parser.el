;;; docopt-argument-parser.el --- The Docopt argument-parser -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The Docopt argument parser

;;; Code:


(require 'cl-lib)
(require 'parsec)

(cl-defgeneric docopt-argument-parser (object))

(cl-defmethod docopt-argument-parser ((argument docopt-argument))
  (cond
   ((docopt-optional argument)
    (parsec-optional (parsec-re "[^ ]+")))
   (t (parsec-re "[^ ]+"))))

(parsec-with-input "" (docopt-argument-parser (docopt-make-argument :name "ARG" :optional nil)))

;;; docopt-argument parser.el ends here
