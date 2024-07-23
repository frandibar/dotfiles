;;; config-magit.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defines fjd_custom-bindings-map
  :custom
  ;; Save files without confirmation.
  (magit-save-repository-buffers 'dontask)
  ;; Use full buffer for magit status instead of splitting.
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :bind
  (:map fjd_custom-bindings-map
	(("C-c v g" . magit-status))))

(provide 'config-magit)
;;; config-magit.el ends here
