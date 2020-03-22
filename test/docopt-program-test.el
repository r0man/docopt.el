;;; docopt-program-test.el --- The Docopt program tests -*- lexical-binding: t -*-

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

;; The Docopt program tests

;;; Code:

(require 'buttercup)
(require 'docopt-program)
(require 'test-helper)

(describe "Find an option by"
  :var ((program (docopt-parse docopt-naval-fate-str)))

  (it "long option should return nil when not found"
    (expect (docopt-program-option program "UNKNOWN")
            :to-equal nil))

  (it "short option should return nil when not found"
    (expect (docopt-program-option program "U")
            :to-equal nil))

  (it "long option should return the long option"
    (expect (docopt-program-option program "help")
            :to-equal docopt-naval-fate-option-help))

  (it "short option should return the short option"
    (expect (docopt-program-option program "h")
            :to-equal docopt-naval-fate-option-h)))

(describe "The copy of a program"
  :var ((program (docopt-parse docopt-naval-fate-str)))
  (it "should be equal to the original"
    (expect (docopt-copy program) :to-equal program)))

;;; docopt-program-test.el ends here
