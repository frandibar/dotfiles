;;; config-surround.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; A package that helps insert, change and delete surrounding pairs of
;; quotes, braces, etc.
(use-package surround
  :bind-keymap ("C-c v s" . surround-keymap))

(provide 'config-surround)
;;; config-surround.el ends here
