;;; docopt-classes-test.el --- The Docopt class tests -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 29 Feb 2020
;; Keywords: docopt, command line argument
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
