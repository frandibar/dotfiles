;;; config-eshell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eshell
  :custom
  ;; By default eshell doesn't scroll to bottom on input.
  ;; Make it scroll for the selected window only.
  (eshell-scroll-to-bottom-on-input 'this))

(provide 'config-eshell)
;;; config-eshell.el ends here
