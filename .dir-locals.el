((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root) "make v=vvv test-buttercup" projectile-test-cmd-map)))))
 (emacs-lisp-mode . ((checkdoc-arguments-in-order-flag)
                     (checkdoc-verb-check-experimental-flag)
                     (indent-tabs-mode . nil)
                     (ispell-buffer-session-localwords "docopt" "testcases")
                     (vterm-always-compile-module . t))))
