;;; config-theme.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :functions doom-themes-treemacs-config

  :custom
  (doom-themes-treemacs-theme "doom-atom")

  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config))

(provide 'config-theme)
;;; config-theme.el ends here
