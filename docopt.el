;;; docopt.el --- The Docopt Emacs mode -*- lexical-binding: t -*-

;; URL: https://github.com/r0man/docopt.el
;; Keywords: docopt
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Docopt for Elisp

;;; Code:

(require 'docopt-classes)
(require 'docopt-parser)
(require 'parsec)

;;;###autoload
(defun docopt-parse-program (s)
  "Parse the Docopt program S."
  (parsec-with-input s (docopt--parse-program)))

(provide 'docopt)

;;; docopt.el ends here
