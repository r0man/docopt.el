;;; docopt-abbrev-test.el --- Docopt abbreviation tests -*- lexical-binding: t -*-

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

;; The Docopt abbreviation tests.

;;; Code:

(require 'buttercup)
(require 'docopt-abbrev)

(describe "The `docopt-abbrev-list` function"
  :var ((words '("hello" "world" "data")))

  (it "should calculate 1 letter abbreviations"
    (expect (docopt-abbrev-list words 1)
            :to-equal '("h" "w" "d")))

  (it "should calculate 2 letter abbreviations"
    (expect (docopt-abbrev-list words 2)
            :to-equal '("he" "wo" "da")))

  (it "should calculate 3 letter abbreviations"
    (expect (docopt-abbrev-list words 3)
            :to-equal '("hel" "wor" "dat"))))

;;; docopt-abbrev-test.el ends here
