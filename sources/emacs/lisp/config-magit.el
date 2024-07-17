(use-package magit
  :config
  ;; Make magit save files when appropriate.
  ;; Default is t, doom's is nil
  (setq magit-save-repository-buffers 'dontask)
  :bind
  (("C-c v g" . magit-status)))

