;;; config-magit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :custom
  ;; Save files without confirmation.
  (magit-save-repository-buffers 'dontask)
  :bind
  (("C-c v g" . magit-status)))


(provide 'config-magit)
;;; config-magit.el ends here
