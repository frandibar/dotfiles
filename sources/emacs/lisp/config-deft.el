;;; config-deft.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package deft
  :custom
  (deft-directory org-directory)
  (deft-recursive t)
  (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|attic\\)$")
  (deft-extensions '("org"))
  (deft-strip-summary-regexp ".*"))

(provide 'config-deft)
;;; config-deft.el ends here
