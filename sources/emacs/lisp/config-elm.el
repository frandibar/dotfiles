;;; config-elm.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elm-mode
  :hook
  (elm-mode . elm-format-on-save-mode))

(provide 'config-elm)
;;; config-elm.el ends here
