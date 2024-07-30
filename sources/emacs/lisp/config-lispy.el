;;; config-lispy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Cool lisp editing.
(use-package lispy
  :functions lispy-mode
  :hook
  (emacs-lisp-mode . lispy-mode))

(provide 'config-lispy)
;;; config-lispy.el ends here
