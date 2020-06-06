;;; docopt-transient.el --- The Docopt transient -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Created: 8 Mar 2020
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

;; The Docopt transient

;;; Code:

(require 'docopt-argument)
(require 'docopt-generic)
(require 'docopt-option)
(require 'docopt-program)
(require 'docopt-usage-pattern)
(require 'docopt-util)
(require 'seq)
(require 'subr-x)
(require 'transient)

(define-error 'docopt-usage-pattern-index
  "Invalid Docopt usage pattern index.")

(cl-defgeneric docopt-transient--suffix-symbol (program object)
  "Return the transient suffix symbol for PROGRAM and OBJECT.")

(defun docopt-transient--program-suffix-name (program)
  "Return the transient suffix symbol for PROGRAM."
  (format "docopt-transient:%s" (s-replace-regexp "[^a-z0-9_]" "-" (docopt-name program))))

(cl-defmethod docopt-transient--suffix-symbol (program (argument docopt-argument))
  "Return the transient suffix symbol for PROGRAM and ARGUMENT."
  (intern (format "%s:argument:%s"
                  (docopt-transient--program-suffix-name program)
                  (docopt-argument-name argument))))

(cl-defmethod docopt-transient--suffix-symbol (program (command docopt-command))
  "Return the transient suffix symbol for PROGRAM and COMMAND."
  (intern (format "%s:command:%s"
                  (docopt-transient--program-suffix-name program)
                  (docopt-command-name command))))

(cl-defmethod docopt-transient--suffix-symbol (program (option docopt-option))
  "Return the transient suffix symbol for PROGRAM and OPTION."
  (intern (format "%s:option:--%s"
                  (docopt-transient--program-suffix-name program)
                  (docopt-option-name option))))

(cl-defmethod docopt-transient--suffix-symbol (program (usage-pattern docopt-usage-pattern))
  "Return the transient suffix symbol for PROGRAM and USAGE-PATTERN."
  (if-let ((index (cl-position usage-pattern (docopt-program-usage program) :test #'equal)))
      (intern (format "%s:usage-pattern:%s" (docopt-transient--program-suffix-name program) (+ 1 index)))
    (signal 'docopt-usage-pattern-index usage-pattern)))

(defun docopt-transient--program-doc (program)
  "Return the doc sting of the transient for PROGRAM."
  (format "Docopt transient command for %s." (docopt-name program)))

(defun docopt-transient--program-symbol (program)
  "Return the symbol of the transient for PROGRAM."
  (intern (format "docopt-transient-%s" (docopt-name program))))

(defun docopt-transient--program-fn-symbol (program)
  "Return the symbol of the transient function for PROGRAM."
  (intern (format "docopt-transient-%s-fn" (docopt-name program))))

(defun docopt-transient--argument-argument (argument)
  "Return the transient argument for ARGUMENT."
  (format "%s=" (docopt-argument-name argument)))

(defun docopt-transient--argument-key (argument)
  "Return the transient key for ARGUMENT."
  (format "%s" (s-upcase (substring (docopt-argument-name argument) 0 1))))

(defun docopt-transient--command-argument (command)
  "Return the transient command for COMMAND."
  (format "%s" (docopt-command-name command)))

(defun docopt-transient--command-key (command)
  "Return the transient key for COMMAND."
  (format "%s" (substring (docopt-command-name command) 0 2)))

(defun docopt-transient--option-argument (option)
  "Return the transient argument for OPTION."
  (concat "--" (docopt-option-name option)
          (when (docopt-option-argument option) "=")))

(defun docopt-transient--option-class (option)
  "Return the transient class for OPTION."
  (if (docopt-option-argument option)
      'docopt-transient--option
    'docopt-transient--switch))

(defun docopt-transient--option-key (option)
  "Return the transient shortarg for OPTION."
  (format "-%s" (substring (docopt-option-name option) 0 1)))

;; Argument

(defclass docopt-transient--argument (transient-option)
  ((docopt :initarg :docopt :type docopt-argument)))

(cl-defmethod transient-init-value ((argument docopt-transient--argument))
  "Set the initial value of the ARGUMENT."
  (prog1 (cl-call-next-method argument)
    (docopt-transient--set-docopt-value argument (oref argument value))))

(cl-defmethod transient-infix-set ((argument docopt-transient--argument) value)
  "Set the value of the Docopt transient ARGUMENT to VALUE."
  (docopt-transient--set-docopt-value argument value)
  (cl-call-next-method argument value))

;; Command

(defclass docopt-transient--command (transient-switch)
  ((docopt :initarg :docopt :type docopt-command)))

(cl-defmethod transient-init-value ((command docopt-transient--command))
  "Set the initial value of the COMMAND."
  (prog1 (cl-call-next-method command)
    (docopt-transient--set-docopt-value command (oref command value))))

(cl-defmethod transient-infix-set ((command docopt-transient--command) value)
  "Set the value of the Docopt transient COMMAND to VALUE."
  (with-slots (docopt) command
    (docopt-transient--set-docopt-value command value)
    (cl-call-next-method command value)))

;; Option

(defclass docopt-transient--option (transient-option)
  ((docopt :initarg :docopt :type docopt-option)))

(cl-defmethod transient-init-value ((option docopt-transient--option))
  "Set the initial value of the OPTION."
  (prog1 (cl-call-next-method option)
    (with-slots (docopt value) option
      (with-slots (argument) docopt
        (setf (oref argument value) value)))))

(cl-defmethod transient-infix-set ((option docopt-transient--option) value)
  "Set the value of the Docopt transient OPTION to VALUE."
  (with-slots (docopt) option
    (with-slots (argument) docopt
      (setf (oref argument value) value)
      (cl-call-next-method option value))))

;; Switch

(defclass docopt-transient--switch (transient-switch)
  ((docopt :initarg :docopt :type docopt-option)))

(cl-defmethod transient-init-value ((option docopt-transient--switch))
  "Set the initial value of the OPTION."
  (prog1 (cl-call-next-method option)
    (docopt-transient--set-docopt-value option (oref option value))))

(cl-defmethod transient-infix-set ((option docopt-transient--switch) value)
  "Set the value of the Docopt transient OPTION to VALUE."
  (docopt-transient--set-docopt-value option value)
  (cl-call-next-method option value))

;; Usage Pattern

(defclass docopt-transient--usage-pattern (transient-switch)
  ((docopt :initarg :docopt :type docopt-usage-pattern)))

(cl-defmethod transient-format-value ((usage-pattern docopt-transient--usage-pattern))
  "Format USAGE-PATTERN for display and return the result."
  (with-slots (docopt value) usage-pattern
    (let ((face (if value 'transient-value 'transient-inactive-value))
          (formatted (docopt-format docopt)))
      (add-face-text-property 0 (length formatted) face t formatted)
      formatted)))

(defun docopt-transient--usage-pattern-toggle ()
  "Toggle the usage pattern of the program."
  (let ((obj (transient-suffix-object)))
    (seq-doseq (suffix  transient--suffixes)
      (when (cl-typep suffix 'docopt-transient--usage-pattern)
        (setf (oref suffix value) nil)))
    (transient-infix-set obj (transient-infix-read obj))
    (transient--show)))

(cl-defgeneric docopt-transient--define-suffix-form (program object)
  "Return the transient suffix definition form for PROGRAM and OBJECT.")

(cl-defmethod docopt-transient--define-suffix-form (program (argument docopt-argument))
  "Return the transient suffix definition form for PROGRAM and ARGUMENT."
  `(define-infix-argument ,(docopt-transient--suffix-symbol program argument) ()
     :argument ,(docopt-transient--argument-argument argument)
     :class 'docopt-transient--argument
     :description ,(docopt-argument-name argument)
     :docopt ,argument
     :key ,(docopt-key argument)))

(cl-defmethod docopt-transient--define-suffix-form (program (command docopt-command))
  "Return the transient suffix definition form for PROGRAM and COMMAND."
  `(define-infix-argument ,(docopt-transient--suffix-symbol program command) ()
     :argument ,(docopt-transient--command-argument command)
     :class 'docopt-transient--command
     :description ,(docopt-command-name command)
     :docopt ,command
     :key ,(docopt-key command)))

(cl-defmethod docopt-transient--define-suffix-form (program (option docopt-option))
  "Return the transient suffix definition form for PROGRAM and OPTION."
  `(define-infix-argument ,(docopt-transient--suffix-symbol program option) ()
     :argument ,(docopt-transient--option-argument option)
     :class ,(docopt-transient--option-class option)
     :description ,(or (docopt-option-description option) (docopt-option-name option))
     :docopt ,option
     :key ,(docopt-key option)))

(cl-defmethod docopt-transient--define-suffix-form (program (usage-pattern docopt-usage-pattern))
  "Return the transient suffix definition form for PROGRAM and USAGE-PATTERN."
  (if-let ((index (cl-position usage-pattern (docopt-program-usage program) :test #'equal)))
      `(define-suffix-command ,(docopt-transient--suffix-symbol program usage-pattern) ()
         :argument ,(docopt-format usage-pattern)
         :class 'docopt-transient--usage-pattern
         :description ,(docopt-format usage-pattern)
         :docopt ,usage-pattern
         :format " %k %v"
         :key ,(number-to-string index)
         (interactive)
         (docopt-transient--usage-pattern-toggle))
    (signal 'docopt-usage-pattern-index usage-pattern)))

(defun docopt-transient--program-arguments (program)
  "Return the transient infix argument s-exprs for the arguments PROGRAM."
  (docopt-remove-duplicates (docopt-collect-arguments program)))

(defun docopt-transient--program-commands (program)
  "Return the transient infix commands s-exprs for the arguments PROGRAM."
  (seq-filter #'docopt-command-incompatible (docopt-collect-commands program)))

(defun docopt-transient--program-options (program)
  "Return the transient infix argument s-exprs for the options PROGRAM."
  (thread-last (docopt-collect-options program)
    (docopt-remove-duplicates)
    (seq-remove (lambda (option)
                  (and (docopt-short-option-p option)
                       (docopt-option-synonym option))))))

(defun docopt-transient--define-suffix-argument-forms (program)
  "Return the transient infix argument s-exprs for the arguments PROGRAM."
  (seq-map (lambda (argument) (docopt-transient--define-suffix-form program argument))
           (docopt-transient--program-arguments program)))

(defun docopt-transient--define-suffix-command-forms (program)
  "Return the transient infix command s-exprs for the commands PROGRAM."
  (seq-map (lambda (command) (docopt-transient--define-suffix-form program command))
           (docopt-transient--program-commands program)))

(defun docopt-transient--define-suffix-option-forms (program)
  "Return the transient infix argument s-exprs for the options PROGRAM."
  (seq-map (lambda (option) (docopt-transient--define-suffix-form program option))
           (docopt-transient--program-options program)))

(defun docopt-transient--define-suffix-usage-pattern-forms (program)
  "Return the transient usage pattern s-exprs for the PROGRAM."
  (seq-map (lambda (usage-pattern) (docopt-transient--define-suffix-form program usage-pattern))
           (docopt-program-usage program)))

(defun docopt-transient--program-list (program args)
  "Return the PROGRAM with ARGS as a list."
  (append (s-split " " (docopt-program-name program)) (cdr args)))

(defun docopt-transient--program-string (program args)
  "Return the PROGRAM with ARGS as a string."
  (string-join (docopt-transient--program-list program args) " "))

(defun docopt-transient--program-buffer-name (program)
  "Return the buffer name for the PROGRAM."
  (concat "*Docopt: " (docopt-name program) "*"))

(defun docopt-transient--selected-usage-pattern (suffixes)
  "Return the selected usage pattern from SUFFIXES."
  (thread-last suffixes
    (seq-filter (lambda (suffix) (cl-typep suffix 'docopt-transient--usage-pattern)))
    (seq-filter (lambda (pattern) (oref pattern value)))
    (seq-map (lambda (pattern) (oref pattern docopt)))
    (car)))

(defun docopt-transient--program-execute-term (program args)
  "Execute the PROGRAM using ARGS in plain term-mode."
  (let ((buffer-name (docopt-transient--program-buffer-name program))
        (args (docopt-transient--program-list program args)))
    (pp args)
    (apply #'start-process buffer-name buffer-name (car args) (cdr args))))

(defun docopt-transient--shell-command (program args)
  "Return the shell command for PROGRAM and ARGS."
  (format "%s -c \"%s\"" shell-file-name (docopt-transient--program-string program args)))

(defun docopt-transient--program-execute-vterm (program args)
  "Execute the PROGRAM using ARGS in vterm-mode with color support."
  (require 'vterm nil)
  (let ((vterm-shell (docopt-transient--shell-command program args)))
    (vterm-mode)
    (use-local-map vterm-copy-mode-map)))

(defun docopt-transient--program-execute ()
  "Execute the PROGRAM with SUFFIXES."
  (interactive)
  (let* ((program (oref transient-current-prefix :scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (buffer-name (docopt-transient--program-buffer-name program)))
    (when-let ((buffer (get-buffer buffer-name)))
      (kill-buffer buffer))
    (switch-to-buffer-other-window (get-buffer-create buffer-name))
    (require 'vterm nil t)
    (cond
     ((boundp 'vterm-shell)
      (docopt-transient--program-execute-vterm program args))
     (t (docopt-transient--program-execute-term program args)))
    (message "Executed %s." (docopt-bold (docopt-transient--program-string program args)))))

(defun docopt-transient--program-clipboard-copy ()
  "Copy the PROGRAM using ARGS to the clipboard."
  (interactive)
  (let* ((program (oref transient-current-prefix :scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (buffer-name (docopt-transient--program-buffer-name program)))
    (with-temp-buffer
      (insert (docopt-transient--program-string program args))
      (clipboard-kill-region (point-min) (point-max)))
    (message "Copied %s to clipboard." (docopt-bold (docopt-transient--program-string program args)))))

(defun docopt-transient--program-insert ()
  "Insert the PROGRAM using ARGS into the current buffer."
  (interactive)
  (let* ((program (oref transient-current-prefix :scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (command (docopt-transient--program-string program args)))
    (insert command)
    (message "Inserted %s to current buffer." (docopt-bold command))))

(defun docopt-transient--section-arguments (program)
  "Return the transient arguments section for the PROGRAM."
  (thread-last (docopt-transient--program-arguments program)
    (seq-map (lambda (argument) (list (docopt-transient--suffix-symbol program argument))))
    (append (list "Arguments"))
    (apply #'vector)))

(defun docopt-transient--section-commands (program)
  "Return the transient commands section for the PROGRAM."
  (thread-last (docopt-transient--program-commands program)
    (seq-map (lambda (argument) (list (docopt-transient--suffix-symbol program argument))))
    (append (list "Commands"))
    (apply #'vector)))

(defun docopt-transient--section-options (program)
  "Return the transient options section for the PROGRAM."
  (thread-last (docopt-transient--program-options program)
    (seq-map (lambda (option) (list (docopt-transient--suffix-symbol program option))))
    (cons "Options")
    (apply #'vector)))

(defun docopt-transient--copy-to-clipboard-action (program)
  "Return the transient action list for the copy to clipboard command of PROGRAM."
  (list "c" "Copy command to clipboard" #'docopt-transient--program-clipboard-copy))

(defun docopt-transient--execute-action (program)
  "Return the transient action list for the execute command of PROGRAM."
  (list "x" "Execute command" #'docopt-transient--program-execute))

(defun docopt-transient--insert-action (program)
  "Return the transient action list for the insert command of PROGRAM."
  (list "i" "Insert command to current buffer" #'docopt-transient--program-insert))

(defun docopt-transient--section-actions (program)
  "Return the transient actions for the PROGRAM."
  (vector "Actions"
          (docopt-transient--copy-to-clipboard-action program)
          (docopt-transient--insert-action program)
          (docopt-transient--execute-action program)))

(defun docopt-transient--section-usage-patterns (program)
  "Return the transient usage pattern section for the PROGRAM."
  (thread-last (docopt-program-usage program)
    (seq-map-indexed
     (lambda (usage-pattern index)
       (list (number-to-string (+ 1 index))
             (docopt-format usage-pattern)
             (docopt-transient--suffix-symbol program usage-pattern))))
    (cons "Usage Patterns")
    (apply #'vector)))

(defun docopt-transient--set-docopt-value (object value)
  "Set the value of the Docopt OBJECT to VALUE."
  (with-slots (docopt) object
    (setf (oref docopt value) value)))

(defun docopt-transient--section-header (program)
  "Return the transient header section for PROGRAM."
  (vector "" (docopt-program-header program)))

(defun docopt-transient--incompatible-options (program)
  "Return the list of incompatible options for PROGRAM."
  (thread-last (docopt-collect-options program)
    (seq-filter #'docopt-option-incompatible)
    (seq-map (lambda (option)
               (cons (docopt-transient--option-argument option)
                     (seq-map #'docopt-transient--option-argument
                              (docopt-option-incompatible option)))))
    (seq-uniq)))

(defun docopt-transient--incompatible-commands (program)
  "Return the list of incompatible options for PROGRAM."
  (thread-last (docopt-collect-commands program)
    (seq-filter #'docopt-command-incompatible)
    (seq-map (lambda (command)
               (cons (docopt-transient--command-argument command)
                     (seq-map #'docopt-command-name (docopt-command-incompatible command)))))
    (seq-uniq)))

(defun docopt-transient--incompatible (program)
  "Return the list of incompatible options for PROGRAM."
  (append (docopt-transient--incompatible-commands program)
          (docopt-transient--incompatible-options program)))

(defun docopt-transient--define-program-form (program)
  "Return the transient infix argument s-exprs for the options PROGRAM."
  (let ((program-symbol (docopt-transient--program-symbol program)))
    `(define-transient-command ,program-symbol ()
       ,(docopt-transient--program-doc program)
       :incompatible (quote ,(docopt-transient--incompatible program))
       ,(docopt-transient--section-header program)
       ,(docopt-transient--section-usage-patterns program)
       ,(docopt-transient--section-commands program)
       ,(docopt-transient--section-options program)
       ,(docopt-transient--section-arguments program)
       ,(docopt-transient--section-actions program)
       (interactive)
       (transient-setup (quote ,program-symbol) nil nil :scope ,program))))

(defun docopt-transient--program-form (program)
  "Return the transient infix argument s-exprs for the options PROGRAM."
  `(progn ,@(docopt-transient--define-suffix-argument-forms program)
          ,@(docopt-transient--define-suffix-command-forms program)
          ,@(docopt-transient--define-suffix-option-forms program)
          ,@(docopt-transient--define-suffix-usage-pattern-forms program)
          ,(docopt-transient--define-program-form program)))

(defun docopt-transient-define-command (program)
  "Define the transient command for PROGRAM."
  (docopt-program-assign-keys program)
  (eval (docopt-transient--program-form program)))

(defun docopt-transient-invoke-command (program)
  "Define the transient command for PROGRAM."
  (let ((command (docopt-transient--program-symbol program)))
    (if (functionp command)
        (funcall command)
      (user-error "Docopt command %s not defined" (docopt-bold command)))))

(defun docopt-transient (program)
  "Define the transient command for PROGRAM."
  (docopt-transient-define-command program)
  (docopt-transient-invoke-command program))

;; (require 'docopt-naval-fate)
;; (setq docopt-naval-fate (docopt-parse docopt-naval-fate-str))
;; (docopt-transient--program-form docopt-naval-fate)
;; (docopt-transient-define-command docopt-naval-fate)
;; (docopt-transient-invoke-command docopt-naval-fate)

(provide 'docopt-transient)

;;; docopt-transient.el ends here
