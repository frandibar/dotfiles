(use-package expand-region
  :config
  (evil-global-set-key 'visual (kbd "C-=") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "C--") 'er/contract-region)
  )
