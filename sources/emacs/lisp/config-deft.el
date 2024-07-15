(require 'org)
(use-package deft
  :config
  (setq deft-directory org-directory
        deft-recursive t
        deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|attic\\)$"
        deft-extensions '("org")
        ;; deft-use-filename-as-title t

        ;; Don't show summary (and make filtering faster)
        deft-strip-summary-regexp ".*"
        ))
