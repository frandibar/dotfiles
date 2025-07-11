;;; config-dired.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dired)

;; Make dired emulate midnight commander to having two buffers side by
;; side and moving files between both.
(setopt dired-dwim-target 'dired-dwim-target-next)

;; Use xdg-open to resolve application to use when opening these files
;; types.
(setopt dired-guess-shell-alist-user
	'(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" "xdg-open")
	  ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "xdg-open")
	  ("\\.\\(?:xcf\\)\\'" "xdg-open")
	  ("\\.csv\\'" "xdg-open")
	  ("\\.tex\\'" "xdg-open")
	  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "xdg-open")
	  ("\\.\\(?:mp3\\|flac\\)\\'" "xdg-open")
	  ("\\.html?\\'" "xdg-open")
	  ("\\.md\\'" "xdg-open")))

;; Revert without asking.
(setopt dired-auto-revert-buffer #'dired-buffer-stale-p)

;; Hide details.
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;; Show human readable files sizes.
(setopt dired-listing-switches "-l --all --human-readable --group-directories-first")

;; Add nice colors to dired.
(use-package diredfl
  :functions diredfl-global-mode
  :config
  (diredfl-global-mode 1))

(provide 'config-dired)
;;; config-dired.el ends here
