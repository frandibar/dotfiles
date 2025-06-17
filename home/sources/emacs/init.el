;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;; My personal Emacs configuration.  I took inspiration from many
;; sources, among others:
;; - https://github.com/SophieBosio

;;; Code:

(message "Loading my custom init.el...")


(if (eq system-type 'windows-nt)
    (progn
      (add-to-list 'load-path "~/.emacs.d/lisp/")
      (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/"))
  (add-to-list 'load-path "~/.config/emacs/lisp/"))

;; Ask for confirmation when closing Emacs.
(setopt confirm-kill-emacs 'y-or-n-p)

(setopt user-full-name "Francisco Dibar")
(setopt user-mail-address "frandibar@gmail.com")

;; Show column position in modeline
(column-number-mode 1)

;; Show line numbers
;; (global-display-line-numbers-mode 1)

;; Add closing pair for parenthesis, etc.
(electric-pair-mode 1)

;; Prevent creation of lockfiles to avoid editing collisions.
(setopt create-lockfiles nil)

;; Mute annoying bell
(setopt ring-bell-function 'ignore)

;; Disable startup screen
(setopt inhibit-startup-screen t)

;; Setup package manager straight.el as replacement for default
;; package.el
;; https://github.com/radian-software/straight.el
;; Bootstrap straight.el.
(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Prevent org version mismatch error
(straight-use-package 'org)

(setopt straight-use-package-by-default t)

(load "fjd_vars.el")

;; Disable backup files
(setopt make-backup-files nil)

;; By default, emacs saves bookmarks to file when exiting
;; emacs. Instead I want to save them immediately when they are added,
;; to avoid loosing them if emacs crashes.
(setopt bookmark-save-flag 1)

;; By default, buffers are not updated when changed on disk.
;; Make auto revert automatic.
(setopt global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Make minibuffer history persistent.
(savehist-mode t)

;; Use trash can when deleting
(setopt delete-by-moving-to-trash t)

;; Enable separation of camel case words.
(add-hook 'prog-mode-hook 'subword-mode)

;; Follow symlinks, avoid confirmation when opening links to version
;; controlled files.
(setopt vc-follow-symlinks t)

;; Delete trialing whitespaces before saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable winner mode to be able to restore window configurations with
;; C-<left> and C-<right>.
(winner-mode 1)

;; When opening a file, jump to location when last visited.
(save-place-mode 1)

;; Emulate vim's Y command.
(define-key fjd_custom-bindings-map
	    (kbd "C-c v Y")
	    'fjd_current-line-to-kill-ring)

;; Switch to previous buffer
(define-key fjd_custom-bindings-map
	    (kbd "C-c v j o")
	    'mode-line-other-buffer)
;; And also provide a shorter key combination as it's frequently used.
(define-key fjd_custom-bindings-map
	    (kbd "C-`")
	    'mode-line-other-buffer)

;; Enable recent files mode.
(use-package recentf
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)

  :config
  (recentf-mode 1))

;; Ask to save desktop when exiting
(use-package emacs
  :hook
  (kill-emacs . (lambda () (if (y-or-n-p "Save desktop?")
                          (desktop-save (expand-file-name user-emacs-directory) t)))))

;; Don't show the tab bar when using tab-bar-mode.
(setopt tab-bar-show nil)

(when (eq system-type 'windows-nt)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (setq default-file-name-coding-system 'utf-8))

;; Load additional settings

(load "fjd.el")
(load "stolen.el")
(load "ert-helpers.el")

;; app
(load "config-calendar.el")
(load "config-deft.el")
(load "config-elfeed.el")
(load "config-mail.el")
(load "config-ledger.el")
(load "config-org.el")
(load "config-pdf.el")

;; emacs
(load "config-ace-window.el")
(load "config-dired.el")
(load "config-avy.el")
(load "config-ligatures.el")
(load "config-which-key.el")

;; editing
(load "config-multiple-cursors.el")
(load "config-expand-region.el")
(load "config-zop-to-char.el")
(load "config-surround.el")

;; tools
(load "config-magit.el")
(load "config-prettier.el")
(load "config-treemacs.el")
(load "config-diff-hl.el")
(load "config-flycheck.el")
(load "config-undo-tree.el")
(load "config-helpful.el")
(load "config-casual.el")

;; Only enable when doing an emacs screencast.
;; (use-package keycast
;;   :config
;;   (keycast-log-mode 1))

;; term
(load "config-terminal.el")

;; completion
(load "config-vertico.el")
(load "config-consult.el")
(load "config-orderless.el")
(load "config-corfu.el")

;; languages
(load "config-lilypond.el")
(load "config-nix.el")
(load "config-elm.el")
(load "config-lisp.el")
(load "config-markdown.el")

;; ui
(load "config-theme.el")
(load "config-modeline.el")
(load "config-org-appearance.el")

;; Set font
(set-face-attribute 'default nil
		    :font "Fira Code"
		    ;; :font "Maple Mono"
		    ;; :font "Borg Sans Mono"
		    ;; :font "NanumGothicCoding"
		    ;; :font "D2Coding ligature"
		    ;; :font "Cascadia Code"
		    :height 140)

;; Start server so as to be able to use emacsclient
(unless
    (and (boundp 'server-process)
	 (memq (process-status server-process) '(connect listen open run)))
  (server-start))

;; Use password manager pass from emacs.
;; This must go after starting the server or it gives error:
;; "*scratch* buffer has no process"
(use-package pass)

;; Add keybinding to toggle vertical/horizontal window split
(define-key fjd_custom-bindings-map
	    (kbd "C-c v w t")
	    'stolen-toggle-window-split)

(define-minor-mode fjd_custom-bindings-mode
  "A mode that activates my custom keybindings."
  :init-value t
  :keymap fjd_custom-bindings-map)

(message "Done loading init.el.")

;; Disable flycheck warnings for assignment to free variables
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

(provide 'init)
;;; init.el ends here
