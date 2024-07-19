;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(message "Loading my custom init.el...")

(add-to-list 'load-path "~/.config/emacs/lisp/")

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

;; Prevent creation of lockfiles to avoid editing collisions
(setq create-lockfiles nil)

;; Mute annoying bell
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Setup package manager straight.el as replacement for default package.el
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

(setq straight-use-package-by-default t)

;; Disable backup files
(setq make-backup-files nil)

;; By default, emacs saves bookmarks to file when exiting
;; emacs. Instead I want to save them immediately when they are added,
;; to avoid loosing them if emacs crashes.
(setq bookmark-save-flag 1)

;; By default, buffers are not updated when changed on disk.
;; Make auto revert automatic.
(setq global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; Make minibuffer history persistent.
(savehist-mode t)

;; Enable recent files mode.
(recentf-mode 1)

;; Use trash can when deleting
(setq delete-by-moving-to-trash t)

;; Enable separation of camel case words.
(add-hook 'prog-mode-hook 'subword-mode)

;; Follow symlinks, avoid confirmation when opening links to version
;; controlled files.
(setq vc-follow-symlinks t)

;; Enable winner mode to be able to restore window configuratinos with
;; C-<left> and C-<right>.
(winner-mode 1)

(global-set-key (kbd "C-c v <") 'join-line)

;; Load additional settings

;; emacs
(load "config-ace-window.el")
(load "config-dired.el")
(load "config-ace-jump.el")
(load "config-which-key.el")
(load "config-expand-region.el")
(load "config-multiple-cursors.el")
(load "config-zop-to-char.el")

;; tools
(load "config-magit.el")
(load "config-prettier.el")
(load "config-treemacs.el")
(load "config-diff-hl.el")
(load "config-flycheck.el")

;; term
(load "config-eshell.el")

;; completion
(load "config-vertico.el")
(load "config-consult.el")
(load "config-orderless.el")

;; languages
(load "config-org.el")
(load "config-lilypond.el")
(load "config-nix.el")
(load "config-elm.el")
(load "config-sbcl.el")
(load "config-markdown.el")
(load "config-ledger.el")

;; ui
(load "config-theme.el")
(load "config-modeline.el")

;; app
(load "config-calendar.el")
(load "config-deft.el")     ;; must go after config-org

(load "config-lispy.el")    ;; must go after config-org

;; Set font
(set-face-attribute 'default nil
		    :font "Fira Code"
		    :height 140)

;; Start server so as to be able to use emacsclient
(unless
    (and (boundp 'server-process)
	 (memq (process-status server-process) '(connect listen open run)))
  (server-start))

(message "Done loading init.el.")

(provide 'init)
;;; init.el ends here
