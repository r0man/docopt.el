;;; docopt-abbrev.el --- Docopt abbreviations -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
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

;; The Docopt abbreviations.

;;; Code:

(require 'cl-lib)
(require 'docopt-util)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(defvar docopt-abbrev-lower-chars
  (cl-loop for char from ?a to ?z collect char)
  "The list of lower case abbreviations chars.")

(defvar docopt-abbrev-upper-chars
  (cl-loop for char from ?A to ?Z collect char)
  "The list of upper case abbreviations chars.")

(defvar docopt-abbrev-numeric-chars
  (cl-loop for char from ?0 to ?9 collect char)
  "The list of numeric abbreviations chars.")

(defvar docopt-abbrev-chars
  (append docopt-abbrev-lower-chars
          docopt-abbrev-upper-chars
          docopt-abbrev-numeric-chars)
  "The list of abbreviations chars.")

(defun docopt-abbrev-sanitize (s)
  "Remove characters from S that should not be used in abbreviations."
  (s-replace-regexp "[-_]" "" s))

(defun docopt-abbrev-candidates (s)
  "Return the abbreviation candidates for S in it's preferred order."
  (delete-dups (append (seq-map #'identity (docopt-abbrev-sanitize s))
                       docopt-abbrev-chars)))

(defun docopt-abbrev-next-char (char &optional index candidates)
  "Return the next abbreviation char for CHAR from CANDIDATES at INDEX."
  (let* ((candidates (or candidates docopt-abbrev-chars))
         (relative-index (or (cl-position char candidates) -1)))
    (nth (mod (+ relative-index (or index 0))
              (length candidates))
         candidates)))

(defun docopt-abbrev-next-string (s &optional index candidates)
  "Return the next abbreviation string for S using CANDIDATES at INDEX."
  (let* ((candidates (or candidates docopt-abbrev-chars))
         (num-candidates (length candidates))
         (len (length s)))
    (when (< index (* len num-candidates))
      (thread-last (reverse s)
        (seq-map-indexed (lambda (current-char char-index)
                           (let* ((next-index (mod (/ index num-candidates) len)))
                             (if (= char-index next-index)
                                 (docopt-abbrev-next-char current-char index candidates)
                               current-char))))
        (reverse)
        (seq-map #'char-to-string)
        (s-join "")))))

(cl-defun docopt-abbrev-list (n lst &key actions)
  "Return the unique abbreviations of length N for each element in LST taking ACTIONS into account."
  (nreverse (seq-reduce
             (lambda (taken-abbrevs next-abbrev)
               (pcase-let ((`(,_ ,preferred-abbrev ,abbrev-candidates) next-abbrev))
                 (let ((index 0))
                   (while (and preferred-abbrev
                               (or (member preferred-abbrev taken-abbrevs)
                                   (seq-some (lambda (action)
                                               (s-starts-with-p action preferred-abbrev))
                                             actions )))
                     (setq preferred-abbrev (docopt-abbrev-next-string preferred-abbrev index abbrev-candidates)
                           index (+ 1 index)))
                   (if preferred-abbrev
                       (cons preferred-abbrev taken-abbrevs)
                     taken-abbrevs))))
             (seq-map (lambda (element)
                        (list element
                              (docopt-substring (docopt-abbrev-sanitize element) 0 n)
                              (docopt-abbrev-candidates element)))
                      lst)
             nil)))

(provide 'docopt-abbrev)

;;; docopt-abbrev.el ends here
