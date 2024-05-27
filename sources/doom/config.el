;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Francisco Dibar"
      user-mail-address "frandibar@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Fira Code has ligatures, I want this for prog mode only but this way sets it globally
;; (setq doom-font "Fira Code-14")
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14.0))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;
;; MY CUSTOM SETTINGS
;;

;; By default, emacs saves bookmarks to file when exiting emacs. Instead I want
;; to save them immediately when they are added, to avoid loosing them if emacs
;; crashes.
(setq bookmark-save-flag 1)

;; By default, outdated dired buffers are not auto refreshed. Doom emacs set's
;; this value to dired-buffer-stale-p, which may be ok, just in case I need to
;; revisit this later I'll leave this commented here.
;; (setq dired-auto-revert-buffer t)
;; Prevent output truncation
;; (setq comint-buffer-maximum-size 100000)

;; Maximize screen
;; Commented out as it's no longer necessary since using i3wm
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; By default, buffers are not updated when changed on disk.
;; Make auto revert automatic.
(setq global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)

;; Make dired emulate midnight commander to having two buffers side by side and
;; moving files between both.
;; Emacs default is nil, doom's is t.
(setq dired-dwim-target 'dired-dwim-target-next)

;; By default eshell doesn't scroll to bottom on input
;; Let's change this.
(setq eshell-scroll-to-bottom-on-input 'this)

;; Read code with two panes side by side as if reading a book.
(add-hook 'prog-mode-hook 'follow-mode)

;; The default behaviour when switching to most recent buffer (SPC `) omits
;; buffers that don't have an associated file name.
;; Override that by removing the default value doom-non-file-visiting-buffer-p
(setq doom-unreal-buffer-functions '(minibufferp doom-special-buffer-p))

;; Prevent warning message when using this function
(put 'narrow-to-region 'disabled nil)


(add-to-list 'load-path "~/.config/doom")
(load "config-lilypond.el")
(load "config-org.el")
(load "config-expand-region.el")
(load "config-elm.el")
(load "config-magit.el")
(load "config-deft.el")
(load "config-ledger.el")
(load "config-frandibar.el")
;; (load "config-prettier.el")
;; (load "config-copilot.el")
(load "config-lsp.el")
(load "config-sbcl.el")
(load "config-calendar.el")
