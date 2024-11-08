;;; config-undo-tree.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; vundo is a newer replacement for undo-tree, it uses less space,
;; provides a diff and looks prettier.
(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))


;; (use-package undo-tree
;;   :functions global-undo-tree-mode
;;   :custom
;;   ;; Avoid cluttering the filesystem with ~undo-tree~ files.
;;   (undo-tree-auto-save-history nil)

;;   :config
;;   (global-undo-tree-mode 1))


(provide 'undo-tree)
;;; config-undo-tree.el ends here
