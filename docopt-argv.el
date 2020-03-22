;;; docopt-argv.el --- The Docopt argument vector parser -*- lexical-binding: t -*-

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

;; The Docopt argument vector parser

;;; Code:

(require 'cl-lib)
(require 'cl-seq)
(require 'dash)
(require 'docopt-parser)
(require 'docopt-util)
(require 'parsec)
(require 's)

(defun docopt--parse-argv-identifier ()
  "Parse a Docopt command line argument identifier."
  (parsec-re "[^ ]+"))

(defun docopt--parse-argv-simple-list* (program lst)
  "Parse the Docopt argument vector LST."
  (let ((num-elements (length lst)))
    (if (equal 1 num-elements)
        `(docopt-argv-parser ,program ,(car lst)))
    `(parsec-collect
      ,@(seq-map-indexed
         (lambda (element index)
           (cond
            ((< index (- num-elements 1))
             `(parsec-return (docopt-argv-parser ,program ,element)
                (parsec-optional
                 (parsec-try
                  (parsec-and
                   (parsec-peek
                    (parsec-and
                     (docopt--parse-spaces1)
                     (docopt-argv-parser ,program ,(nth (+ index 1) lst))))
                   (docopt--parse-spaces1))))))
            ((< index num-elements)
             `(docopt-argv-parser ,program ,element))))
         lst))))

;; (defun docopt--parse-argv-simple-list* (lst)
;;   "Parse the Docopt argument vector LST."
;;   (let ((num-elements (length lst)))
;;     (if (equal 1 num-elements)
;;         `(docopt-argv-parser ,(car lst)))
;;     `(parsec-collect
;;       ,@(seq-map-indexed
;;          (lambda (element index)
;;            (cond
;;             ((zerop index)
;;              `(docopt-argv-parser ,element))
;;             ((docopt-optionable-child-p element)
;;              `(parsec-optional
;;                (parsec-try
;;                 (parsec-and
;;                  (docopt--parse-spaces1)
;;                  (docopt-argv-parser ,element)))))
;;             (t
;;              `(parsec-and
;;                (docopt--parse-spaces1)
;;                (docopt-argv-parser ,element)))))
;;          lst))))

(defun docopt--parse-argv-simple-list (program lst)
  "Parse the Docopt argument vector LST."
  (docopt--flatten (eval (docopt--parse-argv-simple-list* program lst))))

;; (defun docopt-argv--parse-option-list (lst)
;;   "Parse the list LST of Docopt options in any order."
;;   (let ((options (apply #'append (eval `(parsec-sepby
;;                                          (parsec-or ,@(seq-map (lambda (option) `(list (docopt-argv-parser ,option))) lst))
;;                                          (parsec-try (parsec-and (docopt--parse-spaces1)
;;                                                                  (parsec-lookahead (parsec-str "-")))))))))
;;     (let ((expected-options (seq-map #'eieio-object-name-string lst))
;;           (found-options (seq-map #'eieio-object-name-string options)))
;;       (when-let ((difference (cl-set-difference expected-options found-options)))
;;         (parsec-stop
;;          :message (format "Missing options: %s" difference)
;;          :expected expected-options
;;          :found expected-options)))
;;     options))

;; (defun docopt--parse-argv-stacked-list (lst)
;;   "Parse the Docopt argument vector LST."
;;   (parsec-or (docopt--flatten (docopt-argv-parser (docopt-argv--stack-short-options lst)))
;;              (docopt-argv--parse-option-list lst)))

;; (defun docopt-argv--stack-short-options (lst)
;;   "Parse the Docopt argument vector LST."
;;   (thread-last lst
;;     (-partition-by (lambda (element) (not (docopt-short-option-p element))))
;;     (seq-mapcat (lambda (group)
;;                   (thread-last (-partition-after-pred
;;                                 (lambda (element)
;;                                   (and (docopt-short-option-p element)
;;                                        (docopt-option-argument element)))
;;                                 group)
;;                     (seq-map (lambda (group)
;;                                (if (docopt-short-option-p (car group))
;;                                    (make-instance 'docopt-stacked-short-options :members group)
;;                                  group))))))
;;     (docopt--flatten)))

;; (defun docopt-argv-stack-start-regex (option)
;;   "Return the regular expression to parse a stacked short OPTION at the beginning."
;;   (concat "-" (eieio-object-name-string option)))

;; (defun docopt-argv-stack-element-regex (option)
;;   "Return the regular expression to parse a stacked short OPTION."
;;   (let ((name (eieio-object-name-string option)))
;;     (if (docopt-option-argument option)
;;         (concat "\\(\s*-\\)?" name "\\(=\\|[\s]+\\)?\\([^\s]+\\)")
;;       (concat "\\(\s*-\\)?" name))))

;; (defun docopt-argv-stack-regex (options)
;;   "Return the regular expression to parse the stacked short OPTIONS."
;;   (with-slots (members) options
;;     (concat (docopt-argv-stack-start-regex (car members))
;;             (thread-last (seq-drop members 1)
;;               (seq-map (lambda (option) (docopt-argv-stack-element-regex option)))
;;               (s-join "")))))

(defun docopt-argv--parse-long-option-argument (program option)
  "Parse the long OPTION argument of PROGRAM."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (docopt--parse-long-option-separator)
                (docopt-argv-parser program argument))))

(defun docopt-argv--parse-short-option-argument (program option)
  "Parse the short OPTION argument of PROGRAM."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (parsec-optional (docopt--parse-short-option-separator))
                (docopt-argv-parser program argument))))

(defun docopt-argv--parse-short-option-name (program option)
  "Parse the short OPTION name of PROGRAM."
  (let ((option (copy-sequence option)))
    (oset option :argument
          (parsec-and
           (parsec-str (eieio-object-name-string option))
           (docopt-argv--parse-short-option-argument program option)))
    option))

(defun docopt-argv--parse-long-option-name (program option)
  "Parse the long OPTION name of PROGRAM."
  (seq-let [_ argument]
      (parsec-try
       (parsec-collect
        (eval `(parsec-or
                ,@(seq-map
                   (lambda (prefix) `(parsec-str ,prefix))
                   (cons (eieio-object-name-string option)
                         (docopt-long-option-prefixes option)))))
        (docopt-argv--parse-long-option-argument program option)))
    (let ((option (copy-sequence option)))
      (oset option :argument argument)
      option)))

(defun docopt-argv--parse-long-options (program)
  "Parse the long options of PROGRAM."
  `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser ,program ,long-option))
                         (docopt-program-long-options program))))

(defun docopt-argv--parse-long-options (program)
  "Parse the long options of PROGRAM."
  (eval `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser program ,long-option))
                               (docopt-program-long-options program)))))

(defun docopt-argv--parse-long-options (program)
  "Parse the long options of PROGRAM."
  (list (eval `(parsec-or ,@(seq-map (lambda (long-option) `(docopt-argv-parser ,program ,long-option))
                                     (docopt-program-long-options program))))))

;; (parsec-with-input "--version" (docopt-argv--parse-options program))
;; (parsec-with-input "--help" (docopt-argv--parse-long-options program))

;; (parsec-with-input "x" (docopt-argv--parse-options program))

(defun docopt-argv--parse-short-options-stacked-arg-0 (program)
  "Parse the stacked short options of PROGRAM that don't have an argument."
  (parsec-many (eval `(parsec-or ,@(seq-map (lambda (option)
                                              `(docopt-argv--parse-short-option-name ,program ,option))
                                            (seq-remove #'docopt-option-argument (docopt-program-short-options program)))))))

(defun docopt-argv--parse-short-options-stacked-arg-1 (program)
  "Parse the stacked short options of PROGRAM that have an argument."
  (eval `(parsec-or ,@(seq-map (lambda (option)
                                 `(docopt-argv--parse-short-option-name ,program ,option))
                               (seq-filter #'docopt-option-argument (docopt-program-short-options program))))))

(defun docopt-argv--parse-short-options (program)
  "Parse the short options of PROGRAM."
  (thread-last (parsec-and
                (parsec-str "-")
                (parsec-or
                 (parsec-collect
                  (docopt-argv--parse-short-options-stacked-arg-0 program)
                  (parsec-optional (docopt-argv--parse-short-options-stacked-arg-1 program)))
                 (parsec-collect
                  (parsec-optional (docopt-argv--parse-short-options-stacked-arg-0 program))
                  (docopt-argv--parse-short-options-stacked-arg-1 program))))
    (seq-remove #'null)
    (docopt--flatten)))

(defun docopt-argv--parse-options (program)
  "Parse the options of PROGRAM."
  (thread-last  (parsec-sepby
                 (parsec-or
                  (docopt-argv--parse-long-options program)
                  (docopt-argv--parse-short-options program))
                 (docopt--parse-whitespaces))
    (apply #'append)
    (seq-remove #'null)))

;; (parsec-with-input "-h" (docopt-argv--parse-options program))

;; (defun docopt--parse-argv-long-option-argument (program option)
;;   "Parse the argument of the long OPTION of PROGRAM."
;;   (when-let ((argument (docopt-option-argument option)))
;;     (parsec-and (docopt--parse-long-option-separator)
;;                 (docopt-argv-parser program argument))))

;; (defun docopt--parse-argv-short-option-argument (program option)
;;   "Parse the argument of the short OPTION of PROGRAM."
;;   (when-let ((argument (docopt-option-argument option)))
;;     (parsec-and (parsec-optional (docopt--parse-short-option-separator))
;;                 (docopt-argv-parser program argument))))

;; (defun docopt-argv--parse-long-option (program option)
;;   "Parse the long OPTION of PROGRAM."
;;   (seq-let [_ argument]
;;       (parsec-collect
;;        (eval `(parsec-or
;;                ,@(seq-map
;;                   (lambda (prefix) `(parsec-str ,prefix))
;;                   (cons (eieio-object-name-string option)
;;                         (docopt-long-option-prefixes option)))))
;;        (docopt--parse-argv-long-option-argument program option))
;;     (let ((option (copy-sequence option)))
;;       (oset option :argument argument)
;;       option)))

;; (defun docopt-argv--parse-short-option (program option)
;;   "Parse the short OPTION of PROGRAM."
;;   (let ((option (copy-sequence option)))
;;     (oset option :argument
;;           (parsec-and
;;            (parsec-str (eieio-object-name-string option))
;;            (docopt--parse-argv-short-option-argument program option)))
;;     option))

;; (defun docopt-argv--parse-long-options (program)
;;   "Parse the long options of PROGRAM."
;;   (list (eval `(parsec-or ,@(seq-map (lambda (long-option) `(parsec-try (docopt-argv-parser (quote ,program) (quote ,long-option))))
;;                                      (docopt-program-long-options program))))))

;; (defun docopt-argv--parse-short-options-stacked-arg-0 (program)
;;   "Parse the stacked short options of PROGRAM that don't have an argument."
;;   (parsec-many1 (eval `(parsec-or ,@(seq-map (lambda (option)
;;                                                `(docopt-argv--parse-short-option ,program ,option))
;;                                              (seq-remove #'docopt-option-argument (docopt-program-short-options program)))))))

;; (defun docopt-argv--parse-short-options-stacked-arg-1 (program)
;;   "Parse the stacked short options of PROGRAM that have an argument."
;;   (eval `(parsec-or ,@(seq-map (lambda (option)
;;                                  `(docopt-argv--parse-short-option ,program ,option))
;;                                (seq-filter #'docopt-option-argument (docopt-program-short-options program))))))

;; (defun docopt-argv--parse-short-options (options)
;;   "Parse the short OPTIONS, possibly stacked."
;;   (thread-last (parsec-and
;;                 (parsec-str "-")
;;                 (parsec-or
;;                  (parsec-collect
;;                   (docopt-argv--parse-short-options-stacked-arg-0 program)
;;                   (parsec-optional (docopt-argv--parse-short-options-stacked-arg-1 program)))
;;                  (parsec-collect
;;                   (parsec-optional (docopt-argv--parse-short-options-stacked-arg-0 program))
;;                   (docopt-argv--parse-short-options-stacked-arg-1 program))))
;;     (seq-remove #'null)
;;     (docopt--flatten)))

;; (defun docopt-argv--parse-options (program)
;;   "Parse the argument vector options of PROGRAM."
;;   (thread-last  (parsec-optional
;;                  (parsec-sepby
;;                   (parsec-or
;;                    (docopt-argv--parse-long-options program)
;;                    (docopt-argv--parse-short-options program))
;;                   (parsec-try
;;                    (parsec-and
;;                     (docopt--parse-whitespaces)
;;                     (parsec-lookahead (parsec-str "-"))))))
;;     (apply #'append)
;;     (seq-remove #'null)))

;; (parsec-with-input "-ax -b X"
;;   (parsec-collect
;;    (docopt-argv-parser shortcut)
;;    (parsec-str " X")))

;; (defun docopt-argv--parse-options-1 (options)
;;   "Parse the argument vector for OPTIONS."
;;   (parsec-or (docopt-argv--parse-long-options
;;               (seq-filter #'docopt-long-option-p options))
;;              (docopt-argv--parse-short-options
;;               (seq-filter #'docopt-short-option-p options))))

;; (defun docopt-argv--parse-options (options)
;;   "Parse the argument vector for OPTIONS."
;;   (parsec-collect
;;    (docopt-argv--parse-options-1 options)
;;    (parsec-optional
;;     (parsec-try
;;      (parsec-many1
;;       (parsec-and
;;        (docopt--parse-whitespaces)
;;        (docopt-argv--parse-options-1 options)))))))

;; (parsec-with-input "-x -x  "
;;   (parsec-collect
;;    (docopt-argv-parser
;;     #s(docopt-options-shortcut
;;        (#s(docopt-short-option nil "x" nil "" nil))))
;;    (parsec-many-s (parsec-any-ch))))

;; (parsec-with-input "-x a"
;;   (docopt-argv-parser
;;    (list #s(docopt-options-shortcut
;;             (#s(docopt-short-option nil "x" nil "" nil)))
;;          #s(docopt-command nil "a"))))

;; TODO: Remove
;; (defun docopt-argv--parse-stacked-short-option (shortcut)
;;   "Parse a stacked short option from the Docopt SHORTCUT options."
;;   (eval `(parsec-or ,@(thread-last (docopt-options-shortcut-options shortcut)
;;                         (seq-filter #'docopt-short-option-p)
;;                         (seq-map (lambda (option) `(parsec-try (docopt-argv--parse-short-option ,option))))))))

;; (defun docopt-argv--parse-stacked-options (shortcut)
;;   "Parse stacked short options from the Docopt SHORTCUT options."
;;   (parsec-and (parsec-str "-") (parsec-many1 (docopt-argv--parse-stacked-short-option shortcut))))

(cl-defgeneric docopt-argv--parse-option-name (option)
  "Parse the OPTION name.")

(cl-defmethod docopt-argv--parse-option-name ((option docopt-long-option))
  "Parse the long OPTION name."
  (thread-last (cons (object-name-string option) (docopt-long-option-prefixes option))
    (seq-map (lambda (name) (concat "\\(?:--\\(" name "\\)\\)")))
    (s-join "\\|")
    (parsec-re)))

(cl-defmethod docopt-argv--parse-option-name ((option docopt-short-option))
  "Parse the short OPTION name."
  (parsec-str (concat "-" (object-name-string option))))

(cl-defgeneric docopt-argv--parse-option-separator (option)
  "Parse the OPTION separator.")

(cl-defmethod docopt-argv--parse-option-separator ((option docopt-long-option))
  "Parse the long OPTION separator."
  (docopt--parse-long-option-separator))

(cl-defmethod docopt-argv--parse-option-separator ((option docopt-short-option))
  "Parse the short OPTION separator."
  (parsec-optional (docopt--parse-short-option-separator)))

;; (parsec-with-input "-h"
;;   (docopt-argv--parse-option-name docopt-naval-fate-option-h))

(cl-defgeneric docopt-argv-parser (program object)
  "Return an argument vector parser for PROGRAM and OBJECT.")

(cl-defmethod docopt-argv-parser (program (argument docopt-argument))
  "Return an argument vector parser for PROGRAM and ARGUMENT."
  (when-let ((value (docopt--parse-argv-identifier)))
    (let ((argument (docopt-copy argument)))
      (oset argument :value value)
      argument)))

(cl-defmethod docopt-argv-parser (program (command docopt-command))
  "Return an argument vector parser for the PROGRAM and COMMAND."
  (when (parsec-str (oref command object-name))
    (docopt-copy command)))

(cl-defmethod docopt-argv-parser (program (either docopt-either))
  "Return an argument vector parser for PROGRAM and  EITHER."
  (eval `(parsec-or ,@(seq-map (lambda (member) `(parsec-try (docopt-argv-parser (quote ,program) (quote ,member))))
                               (docopt-either-members either)))))

;; (cl-defmethod docopt-argv-parser ((lst list))
;;   "Return an argument vector parser for the LST."
;;   (cond ((seq-find #'docopt-short-option-p lst)
;;          (docopt--parse-argv-stacked-list (docopt--flatten lst)))
;;         (t (docopt--parse-argv-simple-list (docopt--flatten lst)))))

(cl-defmethod docopt-argv-parser (program (lst list))
  "Return an argument vector parser for PROGRAM and LST."
  (docopt--parse-argv-simple-list program (docopt--flatten lst)))

;; (cl-defmethod docopt-argv-parser (program (option docopt-long-option))
;;   "Return an argument vector parser for PROGRAM and OPTION."
;;   (parsec-try (parsec-and (parsec-str "--")
;;                           (docopt-argv--parse-long-option program option))))

;; (parsec-with-input "--version"
;;   (docopt-argv-parser program #s(docopt-long-option nil "version" nil "Show version." nil ("versio" "versi" "vers" "ver" "ve" "v"))))


;; (cl-defmethod docopt-argv-parser :around (program (object docopt-optionable))
;;   "Parse the optional OBJECT of PROGRAM."
;;   (if (docopt-optional-p object)
;;       (parsec-optional (cl-call-next-method program object))
;;     (cl-call-next-method program object)))

;; (cl-defmethod docopt-argv-parser (program (options docopt-stacked-short-options))
;;   "Return an argument vector parser for PROGRAM and the stacked short OPTIONS."
;;   (with-slots (members) options
;;     (let ((group (+ 1 (length members)))
;;           (members (seq-map #'copy-sequence members)))
;;       (when-let ((value (eval `(parsec-query (parsec-re ,(docopt-argv-stack-regex options)) :group ,group))))
;;         (when-let ((argument (docopt-option-argument (car (last members)))))
;;           (oset argument :value value))))
;;     members))

;; (cl-defmethod docopt-argv-parser (program (option docopt-short-option))
;;   "Return an argument vector parser for PROGRAM and OPTION."
;;   (parsec-try (parsec-and (parsec-str "-") (docopt-argv--parse-short-option program option))))

;; (cl-defmethod docopt-argv-parser (program (option docopt-long-option))
;;   "Return an argument vector parser for PROGRAM and OPTION."
;;   (seq-let [_ argument]
;;       (parsec-collect
;;        (eval `(parsec-or ,@(seq-map (lambda (name) `(parsec-str ,(concat "--" name)))
;;                                     (cons (object-name-string option) (docopt-long-option-prefixes option)))))
;;        (when-let ((argument (docopt-option-argument option)))
;;          (docopt--parse-long-option-separator)
;;          (docopt-argv-parser program argument))
;;        argument)))

(cl-defmethod docopt-argv-parser (program (option docopt-option))
  "Return an argument vector parser for PROGRAM and OPTION."
  (seq-let [name argument]
      (parsec-collect
       (docopt-argv--parse-option-name option)
       (when-let ((argument (docopt-option-argument option)))
         (docopt-argv--parse-option-separator option)
         (docopt-argv-parser program argument)))
    (let ((copy (docopt-copy option)))
      (when (docopt-option-argument option)
        (oset copy :argument argument))
      copy)))

;; (parsec-with-input "--version"
;;   (docopt-argv--parse-option-name
;;    #s(docopt-long-option nil "version" nil "Show version." nil
;;                          ("versio" "versi" "vers" "ver" "ve" "v"))))

;; (parsec-with-input "--speed=20"
;;   (docopt-argv-parser docopt-naval-fate docopt-naval-fate-option-speed))

;; (parsec-with-input "asas"
;;   (docopt-argv-parser docopt-naval-fate docopt-naval-fate-option-speed))

;; (parsec-with-input "--speed=20"
;;   (parsec-re (docopt-option-regex docopt-naval-fate-option-speed)))

;; (parsec-with-input "-s=10"
;;   (docopt-argv-parser program (docopt-short-option :object-name "s" :argument (docopt-argument :object-name "kn" :default "10"))))

;; (length (docopt-long-option-prefixes docopt-naval-fate-option-speed))

(cl-defmethod docopt-argv-parser (program (repeated docopt-repeated))
  "Return an argument vector parser for PROGRAM and REPEATED."
  (parsec-sepby (docopt-argv-parser program (docopt-repeated-object repeated)) (docopt--parse-spaces1)))

(cl-defmethod docopt-argv-parser (program (shortcut docopt-options-shortcut))
  "Return an argument vector parser for PROGRAM and SHORTCUT."
  (parsec-optional (docopt-argv--parse-options program)))

(cl-defmethod docopt-argv-parser (program (group docopt-optional-group))
  "Return an argument vector parser for PROGRAM and GROUP."
  (parsec-optional (docopt-argv-parser program (docopt-group-members group))))

;; (cl-defmethod docopt-argv-parser ((group docopt-optional-group))
;;   "Return an argument vector parser for the GROUP."
;;   (docopt--flatten
;;    (eval `(parsec-optional (parsec-sepby
;;                             (parsec-or ,@(seq-map (lambda (member)
;;                                                     `(docopt-argv-parser (quote ,member)))
;;                                                   (docopt-group-members group)))
;;                             (docopt--parse-spaces))))))

(cl-defmethod docopt-argv-parser (_ (program docopt-program))
  "Return an argument vector parser for the PROGRAM."
  (docopt--flatten (eval `(parsec-or ,@(seq-map (lambda (pattern) `(parsec-try (docopt-argv-parser ,program ,pattern)))
                                                (docopt-program-usage program))))))

(cl-defmethod docopt-argv-parser (program (group docopt-required-group))
  "Return an argument vector parser for PROGRAM and GROUP."
  (docopt-argv-parser program (docopt-group-members group)))

(cl-defmethod docopt-argv-parser (program (standard-input docopt-standard-input))
  "Return an argument vector parser for PROGRAM and STANDARD-INPUT."
  (when (parsec-optional (parsec-str "-")) standard-input))

(cl-defmethod docopt-argv-parser (program (pattern docopt-usage-pattern))
  "Return an argument vector parser for PROGRAM and PATTERN."
  (let* ((expressions (docopt-usage-pattern-expressions pattern))
         (num-expressions (length expressions)))
    (seq-let [command exprs]
        (parsec-collect
         (docopt--parse-command-name)
         (parsec-return (if (zerop num-expressions)
                            (parsec-and (docopt--parse-spaces) nil)
                          (parsec-and
                           (docopt--parse-spaces)
                           (docopt-argv-parser program expressions)))
           (parsec-eof)))
      ;; (pp (docopt-collect-options pattern))
      ;; (pp (seq-filter #'docopt-option-child-p exprs))
      (cons (docopt-command command)
            (if (listp exprs) exprs (list exprs))))))

;; (docopt-eval my-program "prog -b -a")
;; (-permutations (list "-a" "-b" "-c"))

;; alist symbol

(cl-defgeneric docopt--argv-symbol (object)
  "Return the symbol for the OBJECT in an alist.")

(cl-defmethod docopt--argv-symbol ((argument docopt-argument))
  "Return the symbol for the ARGUMENT in an alist."
  (let ((name (eieio-object-name-string argument)))
    (if (s-uppercase? name) (intern name) (intern (concat "<" name ">")))))

(cl-defmethod docopt--argv-symbol ((command docopt-command))
  "Return the symbol for the COMMAND in an alist."
  (intern (oref command object-name)))

(cl-defmethod docopt--argv-symbol ((option docopt-long-option))
  "Return the symbol for the long OPTION in an alist."
  (intern (docopt-long-option-format (oref option object-name))))

(cl-defmethod docopt--argv-symbol ((option docopt-short-option))
  "Return the symbol for the short OPTION in an alist."
  (if (docopt-option-synonym option)
      (intern (docopt-long-option-format (oref option synonym)))
    (intern (docopt-short-option-format (oref option object-name)))))

;; alist element

(cl-defgeneric docopt--argv-alist-element (object default)
  "Return the alist cons for the OBJECT and DEFAULT.")

(cl-defmethod docopt--argv-alist-element ((argument docopt-argument) default)
  "Return the alist cons for the ARGUMENT and DEFAULT."
  (cons (docopt--argv-symbol argument) (docopt-argument-value argument)))

(cl-defmethod docopt--argv-alist-element ((command docopt-command) default)
  "Return the alist cons for the COMMAND and DEFAULT."
  (cons (docopt--argv-symbol command) default))

(cl-defmethod docopt--argv-alist-element ((option docopt-option) default)
  "Return the alist cons for the OPTION and DEFAULT."
  (cons (docopt--argv-symbol option)
        (if-let (argument (docopt-option-argument option))
            (or (docopt-argument-value argument)
                (docopt-argument-default argument))
          default)))

(cl-defmethod docopt--argv-alist-element ((standard-input docopt-standard-input) default)
  "Return the alist cons for the STANDARD-INPUT and DEFAULT."
  (cons '- default))

(defun docopt-program-default-alist (program)
  "Return the default alist of the Docopt PROGRAM."
  (seq-map (lambda (element) (docopt--argv-alist-element element nil))
           (docopt-program-argv-normalize program)))

(defun docopt--argv-to-alist (program exprs)
  "Convert the Docopt EXPRS for PROGRAM to an alist."
  (if (docopt--parsec-error-p exprs)
      exprs
    (let ((result (thread-last (seq-remove #'null exprs)
                    (seq-map (lambda (element) (docopt--argv-alist-element element t)))
                    (seq-group-by #'car)
                    (seq-map #'cdr)
                    (seq-map (lambda (group)
                               (cons (caar group)
                                     (let ((values (seq-map #'cdr group)))
                                       (cond
                                        ((= 1 (length values))
                                         (car values))
                                        ((cl-every (lambda (x) (equal t x)) values)
                                         (length values))
                                        (t (apply #'vector values)))))))
                    (seq-sort-by #'car #'string<))))
      (seq-doseq (element (docopt-program-default-alist program))
        (unless (assoc (car element) result)
          (setq result (cons element result))))
      (seq-sort-by #'car #'string< result))))

(defun docopt--parse-argv (program s)
  "Parse the argument vector S of the Docopt PROGRAM."
  (let* ((program (docopt-copy program))
         (result (parsec-with-input s (docopt-argv-parser program program))))
    (if (docopt--parsec-error-p result)
        result (cdr result))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here

(defun docopt-argv--interpose-around (sep lst)
  (cond
   ((and (equal sep (car lst)) (equal sep (car (last lst))))
    (append (cons (car lst) (-interpose sep (cdr (butlast lst 1))))
            (list (car (last lst)))))

   ((equal sep (car lst))
    (append (cons (car lst) (-interpose sep (cdr lst)))
            (list sep)))

   ((equal sep (car (last lst)))
    (append (cons sep (-interpose sep (butlast lst)))
            (last lst)))

   (t (append (cons sep (-interpose sep lst)) (list sep)))))

;; (docopt-argv--interpose-around "x" (list 1 2 3))
;; (docopt-argv--interpose-around "x" (list 1 2 3 "x"))
;; (docopt-argv--interpose-around "x" (list "x" 1 2 3))
;; (docopt-argv--interpose-around "x" (list "x" 1 2 3 "x"))

;; (parsec-with-input "-x"
;;   (docopt-argv-parser
;;    #s(docopt-options-shortcut
;;       (#s(docopt-short-option nil "x" nil "" nil)))))

(defun docopt-argv--interleave-options (program)
  (let ((options (apply #'docopt-make-options-shortcut (docopt-program-options program))))
    (docopt-walk program (lambda (element)
                           (cond
                            ((docopt-group-child-p element)
                             (with-slots (members) element
                               (setq members (docopt-argv--interpose-around options members))
                               element))

                            ((docopt-either-p element)
                             element)

                            ((docopt-usage-pattern-p element)
                             (with-slots (expressions) element
                               (setq expressions (docopt-argv--interpose-around options expressions))
                               element))

                            (t element))))))

(defun docopt-argv--interleave-options (program)
  (let ((options (apply #'docopt-make-options-shortcut (docopt-program-options program))))
    (docopt-walk program (lambda (element)
                           (cond
                            ((docopt-group-child-p element)
                             (with-slots (members) element
                               (setq members (docopt-argv--interpose-around options (seq-remove #'docopt-option-child-p members)))
                               element))

                            ((docopt-either-p element)
                             element)

                            ((docopt-usage-pattern-p element)
                             (with-slots (expressions) element
                               (setq expressions (docopt-argv--interpose-around options (seq-remove #'docopt-option-child-p expressions)))
                               element))

                            (t element))))))

;; (docopt--parse-argv my-program "prog -a -r -myourass")
;; (docopt--parse-argv (docopt-argv--interleave-options my-program) "prog -armyourass")

;; ;; (equal #s(docopt-options-shortcut
;; ;;           (#s(docopt-short-option nil "x" nil "" nil)))
;; ;;        #s(docopt-options-shortcut
;; ;;           (#s(docopt-short-option nil "x" nil "" nil))))

;; (parsec-with-input ""
;;   (docopt-argv-parser
;;    (docopt-command :object-name "c")))

;; (oref #s(docopt-command nil "c" :optional t) :optional)
;; (oref (docopt-command :object-name "c" :optional t) :optional)

;; ;; (docopt-optional #s(docopt-command nil "c" :optional nil))

;; (parsec-with-input "a c"
;;   (docopt-argv-parser
;;    (list (docopt-command :object-name "a")
;;          (docopt-command :object-name "b" :optional t)
;;          (docopt-command :object-name "c" :optional t))))

;; (docopt--parse-argv-simple-list*
;;  (list (docopt-command :object-name "a")
;;        (docopt-command :object-name "b" :optional t)
;;        (docopt-command :object-name "c" :optional t)))

;; (docopt-optionable-child-p (docopt-command :object-name "c" :optional t))
