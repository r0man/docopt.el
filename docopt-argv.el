;;; docopt-argv.el --- The Docopt argument vector parser -*- lexical-binding: t -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The Docopt argument vector parser

;;; Code:


(require 'cl-lib)
(require 'parsec)
(require 'docopt-classes)
(require 'docopt-parser)

(defun docopt--parsec-error-p (result)
  "Return t if the car of RESULT is a 'parsec-error."
  (and (sequencep result) (equal 'parsec-error (car result))))

(defun docopt--parse-argv-identifier ()
  "Parse a Docopt command line argument identifier."
  (parsec-re "[^ ]+"))

(defun docopt--parse-argv-argument (argument)
  "Parse the Docopt argument vector ARGUMENT."
  (when-let ((value (docopt--parse-argv-identifier)))
    (oset argument :value value)
    argument))

(defun docopt--parse-argv-command (command)
  "Parse the Docopt argument vector COMMAND."
  (when (parsec-str (docopt-command-name command))
    command))

(defun docopt--parse-argv-simple-list (lst)
  "Parse the Docopt argument vector LST."
  (let ((num-elements (length lst)))
    (eval (cond
           ((equal 1 num-elements)
            `(docopt-argument-parser ,(car lst)))
           (t `(parsec-collect
                ,@(seq-map (lambda (element)
                             `(parsec-return (docopt-argument-parser ,element)
                                (docopt--parse-spaces1)))
                           (seq-take lst (- num-elements 1)))
                (docopt-argument-parser ,(car (seq-drop lst (- num-elements 1))))))))))

(defun docopt--parse-argv-option-argument (option)
  "Parse the argument of the OPTION command line argument."
  (when-let ((argument (docopt-option-argument option)))
    (parsec-and (docopt--parse-long-option-separator)
                (docopt--parse-argv-argument argument))
    option))

(defun docopt--parse-argv-long-option (long-option)
  "Parse the Docopt argument vector LONG-OPTION."
  (when (parsec-collect (parsec-str (concat "--" (docopt-option-name long-option)))
                        (docopt--parse-argv-option-argument long-option))
    long-option))

(defun docopt--parse-argv-short-option (short-option)
  "Parse the Docopt argument vector SHORT-OPTION."
  (when (parsec-collect (parsec-str (concat "-" (docopt-option-name short-option)))
                        (docopt--parse-argv-option-argument short-option))
    short-option))

(cl-defgeneric docopt-argument-parser (object))

(cl-defmethod docopt-argument-parser ((argument docopt-argument))
  (cond
   ((docopt-optional argument)
    (parsec-optional (docopt--parse-argv-argument argument)))
   (t (docopt--parse-argv-argument argument))))

(cl-defmethod docopt-argument-parser ((option docopt-long-option))
  (cond
   ((docopt-optional option)
    (parsec-optional (docopt--parse-argv-long-option option)))
   (t (docopt--parse-argv-long-option option))))

(cl-defmethod docopt-argument-parser ((option docopt-short-option))
  (cond
   ((docopt-optional option)
    (parsec-optional (docopt--parse-argv-short-option option)))
   (t (docopt--parse-argv-short-option option))))

(cl-defmethod docopt-argument-parser ((command docopt-command))
  (docopt--parse-argv-command command))

(cl-defmethod docopt-argument-parser ((either docopt-either))
  (thread-last (docopt-either-members either)
    (seq-map (lambda (element)
               (let ((result (parsec-start (parsec-try (docopt-argument-parser element)))))
                 (unless (docopt--parsec-error-p result)
                   element))))
    (seq-remove #'null)
    (car)))

(cl-defmethod docopt-argument-parser ((either docopt-either))
  (eval `(parsec-or
          ,@(seq-map (lambda (element)
                       `(parsec-try (docopt-argument-parser (quote ,element))))
                     (docopt-either-members either)))))

(cl-defmethod docopt-argument-parser ((group docopt-group))
  (let ((members (docopt-group-members group)))
    (docopt-argument-parser members)))

(cl-defmethod docopt-argument-parser ((lst list))
  (cond
   ((cl-every #'listp lst)
    (seq-map #'docopt--parse-argv-simple-list lst))
   (t (docopt--parse-argv-simple-list lst))))

(cl-defmethod docopt-argument-parser ((pattern docopt-usage-pattern))
  (parsec-collect
   (parsec-return (docopt-argument-parser (docopt-usage-pattern-command pattern))
     (docopt--parse-spaces1))
   (docopt-argument-parser (docopt-usage-pattern-expressions pattern))))

(cl-defmethod docopt-argument-parser ((program docopt-program))
  (eval `(parsec-or ,@(seq-map (lambda (pattern) `(parsec-try (docopt-argument-parser (quote ,pattern))))
                               (docopt-program-usage program)))))

(provide 'docopt-argv)

;;; docopt-argv.el ends here
