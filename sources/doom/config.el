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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/docs/")

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

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
;; FIXME: fix path for NixOS
(setq load-path (append (list "/usr/share/emacs/site-lisp") load-path))
(setq LilyPond-pdf-command "zathura")

;; Fira Code has ligatures, I want this for prog mode only but this way sets it globally
;; (setq doom-font "Fira Code-14")
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 16))

;; The default behaviour when switching to most recent buffer (SPC `) omits
;; buffers that don't have an associated file name.
;; Override that by removing the default value doom-non-file-visiting-buffer-p
(setq doom-unreal-buffer-functions '(minibufferp doom-special-buffer-p))

(setq inferior-lisp-program "sbcl")


(use-package org
  :config
  (setq org-agenda-files (quote ("cumples.org"
                                 "todo.org"
                                 "agenda-personal.org"
                                 )))

  (setq org-capture-templates
        '(("t" "work todo" entry
           (file "todo.org")
           "* TODO %?\n%i\n%u\n%a" :prepend t)
          ("n" "work note" entry
           (file "notes.org")
           "* %?\n%u" :prepend t)
          ("a" "agenda" entry
           (file "agenda-personal.org")
           "* %?" :prepend t)
          ("b" "bookmark" entry
           (file "bookmarks.org")
           "* %?\n%u" :prepend t)
          ("l" "elm note" entry
           (file "elm.org")
           "* %?\n%u" :prepend t)
          ("e" "emacs")
          ("et" "emacs tips" entry
           (file+headline "emacs.org" "tip")
           "* %?\n%u" :prepend t)
          ("eq" "emacs questions" entry
           (file+headline "emacs.org" "question")
           "* %?\n%u" :prepend t)
          ("g" "git notes" entry
           (file "git.org")
           "* %?\n%u" :prepend t)
          ("y" "nyxt notes" entry
           (file "nyxt.org")
           "* %?\n%u" :prepend t)
          ("h" "teatro" entry
           (file "teatro.org")
           "* %?\n:PROPERTIES:\n:fecha:\n:teatro:\n:con:\n:END:")
          ("p" "peliculas" entry
           (file+headline "peliculas.org" "Peliculas Vistas")
           "* %?\n:PROPERTIES:\n:Director:\n:Elenco:\n:Año:\n:Genero:\n:Fecha: %u\n:imdb:\n:Origen:\n:END:")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "HOLD(h)" "IDEA(i)" "READ(r)" "|" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "|" "[X](D)")))

  ;; System locale to use for formatting time values. Make sure that the
  ;; weekdays in the time stamps of your Org mode files and in the agenda appear
  ;; in English.
  (setq system-time-locale "C")

  ;; I want the org clock report expressed in hours, not days
  (setq org-duration-format 'h:mm)

  ;; Active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (emacs-lisp . t)))

  ;; Prevent automatic indentation in org mode code blocks
  ;; https://stackoverflow.com/questions/53469017/org-mode-source-editing-indents-code-after-exiting-source-code-block-editor
  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-preserve-indentation t)

  )

(use-package expand-region
  :config
  (evil-global-set-key 'visual (kbd "C-=") 'er/expand-region)
  (evil-global-set-key 'visual (kbd "C--") 'er/contract-region)
  )

(use-package elm-mode
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode)
  ;; (add-hook 'elm-mode-hook 'eglot-ensure)
  )

;; For js code formatting
;; (use-package prettier
;;   :config
;;   (add-hook 'after-init-hook #'global-prettier-mode))

;; (use-package copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (:map copilot-completion-map
;;               ("<tab>" . 'copilot-accept-completion)
;;               ("TAB" . 'copilot-accept-completion)
;;               ("C-TAB" . 'copilot-accept-completion-by-word)
;;               ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package magit
  :config
  ;; Make magit save files when appropriate.
  ;; Default is t, doom's is nil
  (setq magit-save-repository-buffers 'dontask))

(use-package deft
  :config (setq deft-directory org-directory
                deft-recursive t
                deft-extensions '("org")
                ;; deft-use-filename-as-title t

                ;; Don't show summary (and make filtering faster)
                deft-strip-summary-regexp ".*"
                ))

(use-package ledger-mode
  :config
  (setq ledger-reports
        '(("assets-in-usd" "%(binary) bal --price-db prices.db asset --current --exchange usd")
          ("income-vs-expenses" "%(binary) bal expense income --depth 1 --current --price-db prices.db --exchange usd --period %(month)")
          ("expenses-this-month" "%(binary) bal expense --period %(month) --current --sort amount")
          ("expenses-with-tag" "%(binary) reg expense --current --limit 'has_tag(/mytag/)'")
          ("balance-stablecoins" "%(binary) bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/'")
          ("balance-stablecoins-in-usd" "%(binary) bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/' --price-db prices.db --exchange usd")
          ("expenses-pending-this-month" "%(binary) bal expense --period %(month) --uncleared --sort amount")
          ("unbudgeted-expenses-this-month" "%(binary) bal expense --unbudgeted --monthly --sort amount --period %(month)")
          ("prices-ccy" "%(binary) prices btc")
          ("bal" "%(binary) bal")
          ("reg" "%(binary) reg")
          ("payee" "%(binary) reg @%(payee)")
          ("account" "%(binary) reg %(account)"))))

(use-package frandibar
  :load-path "~/.config/doom/modules/frandibar"
  :bind (:map evil-normal-state-map
              ("Y" . 'frandibar/yank-whole-line)))
;; Remap Y from evil-yank-line since it doesn't behave as I would expect
;; I would have expected the following to work, but instead I rely on frandibar/yank-whole-line
;;
;; (define-key evil-normal-state-map (kbd "Y") 'evil-collection-magit-yank-whole-line)


;; https://github.com/elm-tooling/elm-language-server
(after! lsp-ui
  (setq lsp-ui-doc-max-width 100)
  (setq lsp-ui-doc-max-height 30)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-symbol-highlighting nil)
  )

