;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:

;; My personal Emacs configuration.  I took inspiration from many
;; sources, among others:
;; - https://github.com/SophieBosio

;;; Code:

(message "Loading my custom init.el...")

(add-to-list 'load-path "~/.config/emacs/lisp/")

;; Setup package manager straight.el as replacement for default
;; package.el
;; https://github.com/radian-software/straight.el
;; Bootstrap straight.el.
(setq straight-use-package-by-default t)
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

(load "fjd_vars.el")

;; Ask for confirmation when closing Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

(setq user-full-name "Francisco Dibar")
(setq user-mail-address "frandibar@gmail.com")

;; Show column position in modeline
(column-number-mode 1)

;; Show line numbers
;; (global-display-line-numbers-mode 1)

;; Add closing pair for parenthesis, etc.
(electric-pair-mode 1)

;; Prevent creation of lockfiles to avoid editing collisions.
(setq create-lockfiles nil)

;; Mute annoying bell
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable backup files
(setq make-backup-files nil)

;; By default, emacs saves bookmarks to file when exiting
;; emacs. Instead I want to save them immediately when they are added,
;; to avoid loosing them if emacs crashes.
(setq bookmark-save-flag 1)

;; By default, buffers are not updated when changed on disk.
;; Make auto revert automatic.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Make minibuffer history persistent.
(savehist-mode t)

;; Use trash can when deleting
(setq delete-by-moving-to-trash t)

;; Enable separation of camel case words.
(add-hook 'prog-mode-hook 'subword-mode)

;; Follow symlinks, avoid confirmation when opening links to version
;; controlled files.
(setq vc-follow-symlinks t)
;; But...I'm not even using vc at all.
(setq vc-handled-backends nil)

;; Delete trialing whitespaces before saving a file.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable winner mode to be able to restore window configurations with
;; C-<left> and C-<right>.
(winner-mode 1)

;; When opening a file, jump to location when last visited.
(save-place-mode 1)

;; Emulate vim's Y command.
(define-key fjd_custom-bindings-map (kbd "C-c v Y") 'fjd_current-line-to-kill-ring)

;; Switch to previous buffer
(define-key fjd_custom-bindings-map (kbd "C-c v j o") 'mode-line-other-buffer)
;; And also provide a shorter key combination as it's frequently used.
(define-key fjd_custom-bindings-map (kbd "C-`") 'mode-line-other-buffer)

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


;; Load additional settings

(load "fjd.el")
(load "ert-helpers.el")

;; app
(load "config-calendar.el")
(load "config-deft.el")

;; emacs

(load "config-elfeed.el")
(load "config-ace-window.el")
(load "config-dired.el")
(load "config-avy.el")
(load "config-which-key.el")
(load "config-expand-region.el")
(load "config-multiple-cursors.el")
(load "config-zop-to-char.el")
(load "config-ligatures.el")

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
(load "config-org.el")
(load "config-lilypond.el")
(load "config-nix.el")
(load "config-elm.el")
(load "config-lisp.el")
(load "config-markdown.el")
(load "config-ledger.el")

;; ui
(load "config-theme.el")
(load "config-modeline.el")
(load "config-org-appearance.el")

;; Set font
(set-face-attribute 'default nil
		    :font "Fira Code"
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
