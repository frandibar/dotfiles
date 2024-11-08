;;; config-prettier.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; For js code formatting.
(use-package prettier
  :hook
  (after-init . global-prettier-mode))

(provide 'config-prettier)
;;; config-prettier.el ends here
