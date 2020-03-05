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

(describe "Find an option line by"
  :var ((option-line (docopt-make-option-line :short-name "h" :long-name "help" :description "Show this screen.")))

  (it "description should return nil when not found"
    (expect (docopt-program-find-option-line docopt-naval-fate "UNKNOWN")
            :to-equal nil))

  (it "long option should return nil when not found"
    (expect (docopt-program-find-option-line docopt-naval-fate (docopt-long-option :name "UNKNOWN"))
            :to-equal nil))

  (it "short option should return nil when not found"
    (expect (docopt-program-find-option-line docopt-naval-fate (docopt-short-option :name "UNKNOWN"))
            :to-equal nil))

  (it "description should return the option line when found by long option"
    (expect (docopt-program-find-option-line docopt-naval-fate (docopt-option-line-description option-line))
            :to-equal option-line))

  (it "long option should return the option line when found by long option"
    (expect (docopt-program-find-option-line docopt-naval-fate (docopt-option-line-long-option option-line))
            :to-equal option-line))

  (it "short option should return the option line when found by short option"
    (expect (docopt-program-find-option-line docopt-naval-fate (docopt-option-line-short-option option-line))
            :to-equal option-line)))

;;; docopt-program-test.el ends here
