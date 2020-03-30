;;; docopt-testcase-test.el --- The Docopt testcase tests -*- lexical-binding: t -*-

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

;; The Docopt testcase tests

;;; Code:

(require 'ert)
(require 'f)
(require 'docopt-testcase)

(defvar docopt-testcase-filename
  "test/testcases.docopt")

(ert-deftest docopt-test-testcase-parse-num-tests ()
  (should (equal 81 (length (docopt--testcase-parse (f-read-text docopt-testcase-filename))))))

(ert-deftest docopt-test-testcase-parse ()
  (let ((testcase (nth 1 (docopt--testcase-parse (f-read-text docopt-testcase-filename)))))
    (should (equal (docopt-parse-program "Usage: prog [options]\n\nOptions: -a  All.\n\n")
                   (docopt-testcase-program testcase)))
    (should (equal (list (docopt-testcase-example :argv "prog"  :expected '((-a . :json-false)))
                         (docopt-testcase-example :argv "prog -a" :expected '((-a . t)))
                         (docopt-testcase-example :argv "prog -x" :expected "user-error"))
                   (docopt-testcase-examples testcase)))))

;;; docopt-testcase-test.el ends here
