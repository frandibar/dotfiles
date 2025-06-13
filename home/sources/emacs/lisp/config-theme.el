;;; config-theme.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Nice themes:
;; doom-atom
;; doom-one
;; doom-nord
;; doom-zenburn

(use-package all-the-icons)

(use-package doom-themes
  :functions doom-themes-treemacs-config
  :config
  (load-theme 'doom-nord t)
  (doom-themes-treemacs-config))

(provide 'config-theme)
;;; config-theme.el ends here
