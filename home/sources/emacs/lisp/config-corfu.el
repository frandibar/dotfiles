;;; config-corfu.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Vertico provides a minimalistic vertical completion UI
(use-package corfu
  :functions global-corfu-mode
  :init
  (global-corfu-mode 1))

(provide 'config-corfu)
;;; config-corfu.el ends here
