;;; config-vertico.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Vertico provides a minimalistic vertical completion UI
(use-package vertico
  :functions vertico-mode

  :custom
  ;; Number of candidates to show (default is 10)
  (vertico-count 20)

  :init
  (vertico-mode 1))

(provide 'config-vertico)
;;; config-vertico.el ends here
