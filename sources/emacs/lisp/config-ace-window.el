;;; config-ace-window.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This package is part of Emacs. It allows switching between windows.
(use-package ace-window
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	([remap other-window] . ace-window)))

(provide 'config-ace-window)
;;; config-ace-window.el ends here
