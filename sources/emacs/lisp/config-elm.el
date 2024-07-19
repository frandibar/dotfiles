;;; config-elm.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

(provide 'config-elm)
;;; config-elm.el ends here
