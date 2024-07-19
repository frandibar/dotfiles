;;; config-ligatures.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Ligatures are usually programming operators that use more than one
;; char, replaced with a unicode char.  It's not necessary to use fira
;; code font in order to use ligatures.
(use-package fira-code-mode
  :hook
  (prog-mode . fira-code-mode))

(provide 'config-ligatures)
;;; config-ligatures.el ends here
