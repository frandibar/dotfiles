;;; config-casual.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package casual

  ;; casual-dired
  :functions (casual-dired-tmenu
	      casual-dired-sort-by-tmenu)
  :defines dired-mode-map
  :bind (:map dired-mode-map
              ("C-c v c" . 'casual-dired-tmenu)
              ("s" . 'casual-dired-sort-by-tmenu))

  ;; casual-calc
  :functions casual-calc-tmenu
  :defines calc-mode-map
  :bind (:map calc-mode-map
              ("C-c v c" . 'casual-calc-tmenu))

  ;; casual-info
  :functions casual-info-tmenu
  :defines Info-mode-map
  :bind (:map Info-mode-map
              ("C-c v c" . 'casual-info-tmenu))

  ;; casual-isearch
  :functions casual-isearch-tmenu
  :defines isearch-mode-map
  :bind (:map isearch-mode-map
              ("C-c v c" . 'casual-isearch-tmenu))

  ;; casual-re-builder
  :functions casual-re-builder-tmenu
  :defines reb-mode-map
  :bind (:map reb-mode-map
              ("C-c v c" . casual-re-builder-tmenu))

  ;; casual-bookmarks
  :defines bookmark-bmenu-mode-map
  :bind (:map bookmark-bmenu-mode-map
              ("C-c v c" . casual-bookmarks-tmenu)))

(provide 'config-casual)
;;; config-casual.el ends here.
