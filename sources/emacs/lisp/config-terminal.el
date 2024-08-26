;;; config-terminal.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :custom
  ;; By default eshell doesn't scroll to bottom on input.
  ;; Make it scroll for the selected window only.
  (eshell-scroll-to-bottom-on-input 'this))


;; Play nice with nix-shell.
(use-package direnv
  :config
  (direnv-mode 1))


(provide 'config-terminal)
;;; config-terminal.el ends here
