;;; config-diff-hl.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; diff-hl-mode highlights uncommitted changes on the left side of the
;; window (area also known as the "gutter"), allows you to jump
;; between and revert them selectively.
(use-package diff-hl
  :functions global-diff-hl-mode
  :config
  (global-diff-hl-mode 1)

  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; Show fringe changes in dired
  (dired-mode . diff-hl-dired-mode))

(provide 'config-diff-hl)
;;; config-diff-hl.el ends here
