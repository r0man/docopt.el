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

(require 'docopt-analyzer)
(require 'docopt-argument)
(require 'docopt-command)
(require 'docopt-generic)
(require 'docopt-option)
(require 'docopt-program)
(require 'docopt-usage-pattern)
(require 'docopt-util)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defcustom docopt-transient-switch-to-buffer #'switch-to-buffer-other-window
  "The buffer switch function of the transient command."
  :type 'function
  :group 'docopt)

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

(defun docopt-transient--documentation (program)
  "Return the documentation sting of the transient command for PROGRAM."
  (format "Docopt transient command for %s." (docopt-name program)))

(defun docopt-transient--program-symbol (program)
  "Return the symbol of the transient for PROGRAM."
  (intern (format "docopt-transient-%s" (docopt-name program))))

(defun docopt-transient--argument-argument (argument)
  "Return the transient argument for ARGUMENT."
  (format "%s=" (docopt-argument-name argument)))

(defun docopt-transient--argument-key (argument)
  "Return the transient key for ARGUMENT."
  (s-upcase (substring (docopt-argument-name argument) 0 1)))

(defun docopt-transient--command-argument (command)
  "Return the transient command for COMMAND."
  (docopt-command-name command))

(defun docopt-transient--command-key (command)
  "Return the transient key for COMMAND."
  (substring (docopt-command-name command) 0 2))

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
  "Return the transient short argument for OPTION."
  (format "-%s" (substring (docopt-option-name option) 0 1)))

;; Argument

(defclass docopt-transient--argument (transient-option)
  ((docopt :initarg :docopt :type docopt-argument)))

(cl-defmethod transient-init-value ((argument docopt-transient--argument))
  "Set the initial value of the ARGUMENT."
  (prog1 (cl-call-next-method argument)
    (docopt-transient--set-docopt-value argument (oref argument value))))

(cl-defmethod transient-infix-set ((argument docopt-transient--argument) value)
  "Set the value of the transient ARGUMENT to VALUE."
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
  "Set the value of the transient COMMAND to VALUE."
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
  "Set the value of the transient OPTION to VALUE."
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
  "Set the value of the transient OPTION to VALUE."
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
    (seq-doseq (suffix transient--suffixes)
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
         :argument ,(docopt-string usage-pattern)
         :class 'docopt-transient--usage-pattern
         :description ,(docopt-string usage-pattern)
         :docopt ,usage-pattern
         :format " %k %d \n   %v"
         :key ,(number-to-string index)
         (interactive)
         (docopt-transient--usage-pattern-toggle))
    (signal 'docopt-usage-pattern-index usage-pattern)))

(defun docopt-transient--program-arguments (program)
  "Return the transient infix argument s-expressions for the arguments PROGRAM."
  (thread-last (docopt-collect-arguments program)
    (docopt-remove-duplicates)
    (seq-sort-by #'docopt-name #'string<)))

(defun docopt-transient--program-commands (program)
  "Return the transient infix commands s-expressions for the arguments PROGRAM."
  (thread-last (docopt-collect-commands program)
    (seq-filter #'docopt-command-incompatible)
    (docopt-remove-duplicates)
    (seq-sort-by #'docopt-name #'string<)))

(defun docopt-transient--program-options (program)
  "Return the transient infix argument s-expressions for the options PROGRAM."
  (thread-last (docopt-collect-options program)
    (docopt-remove-duplicates)
    (seq-remove (lambda (option)
                  (and (docopt-short-option-p option)
                       (docopt-option-synonym option))))
    (seq-sort-by #'docopt-name #'string<)))

(defun docopt-transient--define-suffix-argument-forms (program)
  "Return the transient infix argument s-expressions for the arguments PROGRAM."
  (seq-map (lambda (argument) (docopt-transient--define-suffix-form program argument))
           (docopt-transient--program-arguments program)))

(defun docopt-transient--define-suffix-command-forms (program)
  "Return the transient infix command s-expressions for the commands PROGRAM."
  (seq-map (lambda (command) (docopt-transient--define-suffix-form program command))
           (docopt-transient--program-commands program)))

(defun docopt-transient--define-suffix-option-forms (program)
  "Return the transient infix argument s-expressions for the options PROGRAM."
  (seq-map (lambda (option) (docopt-transient--define-suffix-form program option))
           (docopt-transient--program-options program)))

(defun docopt-transient--define-suffix-usage-pattern-forms (program)
  "Return the transient usage pattern s-expressions for the PROGRAM."
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

(defun docopt-transient--execute-command-term (program command buffer)
  "Execute the shell COMMAND of PROGRAM in BUFFER using term."
  (ignore program)
  (when-let ((buffer (get-buffer buffer)))
    (kill-buffer buffer))
  (funcall docopt-transient-switch-to-buffer buffer)
  (start-process buffer buffer shell-file-name "-c" command))

(defun docopt-transient--retry-question (command)
  "Return the retry question for COMMAND."
  (format "Failed to execute %s. Do you want to retry?" (docopt-bold command)) )

(defun docopt-transient--execute-sentinel (program command process event)
  "The EVENT handler for PROCESS executing the COMMAND of PROGRAM."
  (ignore process)
  (when (and (s-match "exited abnormally" event)
             (yes-or-no-p (docopt-transient--retry-question command) ))
    (docopt-transient program)))

(defun docopt-transient--execute-command-vterm (program command buffer)
  "Execute the shell COMMAND of PROGRAM in BUFFER with a fully-featured terminal emulator."
  (require 'vterm)
  (when-let ((buffer (get-buffer buffer)))
    (kill-buffer buffer))
  (funcall docopt-transient-switch-to-buffer buffer)
  (let ((vterm-kill-buffer-on-exit nil)
        (vterm-shell (format "%s -c \"%s\"" shell-file-name command)))
    (vterm-mode)
    (set-process-sentinel vterm--process
                          (lambda (process event)
                            (docopt-transient--execute-sentinel program command process event)))
    (use-local-map vterm-copy-mode-map)))

(defun docopt-transient--execute-command (program command buffer)
  "Execute the shell COMMAND of PROGRAM in BUFFER with a terminal emulator."
  (if (require 'vterm nil t)
      (docopt-transient--execute-command-vterm program command buffer)
    (docopt-transient--execute-command-term program command buffer)))

(defun docopt-transient--program-edit ()
  "Edit and execute the current transient command."
  (interactive)
  (let* ((program (oref transient-current-prefix scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (buffer-name (docopt-transient--program-buffer-name program))
         (command (read-from-minibuffer "Execute: " (docopt-transient--program-string program args))))
    (docopt-transient--execute-command program command buffer-name)))

(defun docopt-transient--program-execute ()
  "Execute the current transient command."
  (interactive)
  (let* ((program (oref transient-current-prefix scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (command (docopt-transient--program-string program args))
         (buffer-name (docopt-transient--program-buffer-name program)))
    (docopt-transient--execute-command program command buffer-name)
    (message "Executed %s." (docopt-bold command))))

(defun docopt-transient--program-clipboard-copy ()
  "Copy the current transient command to the clipboard."
  (interactive)
  (let* ((program (oref transient-current-prefix scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern)))
    (with-temp-buffer
      (insert (docopt-transient--program-string program args))
      (clipboard-kill-region (point-min) (point-max)))
    (message "Copied %s to clipboard." (docopt-bold (docopt-transient--program-string program args)))))

(defun docopt-transient--program-insert ()
  "Insert the current transient command into the current buffer."
  (interactive)
  (let* ((program (oref transient-current-prefix scope))
         (usage-pattern (docopt-transient--selected-usage-pattern transient-current-suffixes))
         (args (docopt-shell-arguments usage-pattern))
         (command (docopt-transient--program-string program args)))
    (insert command)
    (message "Inserted %s to current buffer." (docopt-bold command))))

(defun docopt-transient--section-list (program section elements)
  "Return the transient SECTION for the ELEMENTS of the PROGRAM."
  (thread-last elements
    (seq-map (lambda (element) (list (docopt-transient--suffix-symbol program element))))
    (append (list section))
    (apply #'vector)))

(defun docopt-transient--section-arguments (program)
  "Return the transient arguments section for the PROGRAM."
  (docopt-transient--section-list program "Arguments" (docopt-transient--program-arguments program)))

(defun docopt-transient--section-commands (program)
  "Return the transient commands section for the PROGRAM."
  (docopt-transient--section-list program "Commands" (docopt-transient--program-commands program)))

(defun docopt-transient--section-options (program)
  "Return the transient options section for the PROGRAM."
  (docopt-transient--section-list program "Options" (docopt-transient--program-options program)))

(defvar docopt-transient--section-actions
  ["Actions"
   ("c" "Copy command to clipboard" docopt-transient--program-clipboard-copy)
   ("e" "Edit and execute command" docopt-transient--program-edit)
   ("i" "Insert command to current buffer" docopt-transient--program-insert)
   ("x" "Execute command" docopt-transient--program-execute)]
  "Return the transient actions for the PROGRAM.")

(defun docopt-transient--section-usage-patterns (program)
  "Return the transient usage pattern section for the PROGRAM."
  (thread-last (docopt-program-usage program)
    (seq-map-indexed
     (lambda (usage-pattern index)
       (list (number-to-string (+ 1 index))
             (docopt-string usage-pattern)
             (docopt-transient--suffix-symbol program usage-pattern))))
    (cons "Usage Patterns")
    (apply #'vector)))

(defun docopt-transient--set-docopt-value (object value)
  "Set the value of the OBJECT to VALUE."
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

(defun docopt-transient--defaults (program)
  "Return the transient defaults for PROGRAM."
  (with-slots (usage) program
    (when-let ((pattern (car usage)))
      (list (concat (docopt-format pattern))))))

(defun docopt-transient--define-program-form (program)
  "Return the transient infix argument s-expressions for the options PROGRAM."
  (let ((program-symbol (docopt-transient--program-symbol program)))
    `(define-transient-command ,program-symbol ()
       ,(docopt-transient--documentation program)
       :incompatible (quote ,(docopt-transient--incompatible program))
       :value (quote ,(docopt-transient--defaults program))
       ,(docopt-transient--section-header program)
       ,(docopt-transient--section-usage-patterns program)
       ,(docopt-transient--section-commands program)
       ,(docopt-transient--section-options program)
       ,(docopt-transient--section-arguments program)
       ,docopt-transient--section-actions
       (interactive)
       (transient-setup (quote ,program-symbol) nil nil :scope ,program))))

(defun docopt-transient--program-form (program)
  "Return the transient infix argument s-expressions for the options PROGRAM."
  `(progn ,@(docopt-transient--define-suffix-argument-forms program)
          ,@(docopt-transient--define-suffix-command-forms program)
          ,@(docopt-transient--define-suffix-option-forms program)
          ,@(docopt-transient--define-suffix-usage-pattern-forms program)
          ,(docopt-transient--define-program-form program)))

(defun docopt-transient-define-command (program)
  "Define the transient command for PROGRAM."
  (docopt-analyzer-assign-keys program)
  (eval (docopt-transient--program-form program)))

(defun docopt-transient-invoke-command (program)
  "Define the transient command for PROGRAM."
  (let ((command (docopt-transient--program-symbol program)))
    (if (functionp command)
        (funcall command)
      (user-error "Docopt command %s not defined" (docopt-bold command)))))

(defun docopt-transient (program)
  "Define the transient command for PROGRAM and invoke it."
  (docopt-transient-define-command program)
  (docopt-transient-invoke-command program))

;; (require 'docopt-naval-fate)
;; (setq docopt-naval-fate (docopt-parse docopt-naval-fate-str))
;; (docopt-transient--program-form docopt-naval-fate)
;; (docopt-transient-define-command docopt-naval-fate)
;; (docopt-transient-invoke-command docopt-naval-fate)

(provide 'docopt-transient)

;;; docopt-transient.el ends here
