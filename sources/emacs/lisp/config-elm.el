(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode)
  ;; (add-hook 'elm-mode-hook 'eglot-ensure)
  )
