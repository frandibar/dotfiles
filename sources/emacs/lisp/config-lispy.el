;; https://github.com/abo-abo/lispy
(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))
