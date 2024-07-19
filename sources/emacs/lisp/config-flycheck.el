;;; config-flycheck.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :functions global-flycheck-mode
  :init
  (global-flycheck-mode 1))

(provide 'config-flycheck)
;;; config-flycheck.el ends here
