;; For js code formatting
(use-package prettier
  :config
  (add-hook 'after-init-hook #'global-prettier-mode))
