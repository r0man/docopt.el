;;; docopt-util-test.el --- The Docopt util tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Keywords: docopt, tools, processes
;; Homepage: https://github.com/r0man/docopt.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; util version 3, or (at
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

;; The Docopt util tests

;;; Code:

(require 'buttercup)
(require 'docopt-util)

(describe "Converting a string to a keyword"

  (it "should handle nil"
    (expect (docopt-keyword nil) :to-equal nil))

  (it "should handle the empty string string"
    (expect (docopt-keyword "") :to-equal nil))

  (it "should handle the string \"x\""
    (expect (docopt-keyword "x") :to-equal :x))

  (it "should handle the string \"x y\""
    (expect (docopt-keyword "x y") :to-equal :x-y)))

;;; docopt-util-test.el ends here
