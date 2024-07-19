;;; config-ibuffer.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ibuffer is part of Emacs
;; Use ibuffer instead of list-buffers.
(use-package ibuffer
  :bind
  ([remap list-buffers] . ibuffer))

(provide 'config-ibuffer)
;;; config-ibuffer.el ends here
