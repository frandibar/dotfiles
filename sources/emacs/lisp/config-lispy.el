;;; config-lispy.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Cool lisp editing
(use-package lispy
  :functions lispy-mode
  :hook
  (emacs-lisp-mode . (lambda () (lispy-mode 1))))

(provide 'config-lispy)
;;; config-lispy.el ends here
