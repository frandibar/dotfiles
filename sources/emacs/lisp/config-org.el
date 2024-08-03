;;; config-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'fjd)
(require 'org)

(defun fjd_org-which-function ()
  "Return concatenated string with path to current header.

Each header is truncated to prevent the length from growing too
big, and concatenated with a separator.

i.e. For the following headlines:
* One level
** Level 2|

| represents the cursor => \"One L... > Level 2\""
  (interactive)
  (when (eq major-mode 'org-mode)
    (let ((header-length 8)
	  (separator " > "))
      (mapconcat #'(lambda (header) (fjd_truncate-string header header-length))
		 (org-get-outline-path t)
		 separator))))


(use-package which-func
  :config
  ;; Add current heading to modeline.
  (add-to-list 'which-func-functions 'fjd_org-which-function))


(use-package org
  :defines
  (org-duration-format
   org-agenda-todo-ignore-scheduled
   org-capture-templates
   fjd_custom-bindings-map)

  :hook
  (org-mode . which-function-mode)

  :bind
  (:map fjd_custom-bindings-map
	("C-c v o a" . org-agenda)
	("C-c v o c" . org-capture))

  :custom

  ;; If you use `org' and don't want your org files in the default
  ;; location below, change `org-directory'. It must be set before org
  ;; loads!
  (org-directory "~/Sync/docs/")

  ;; Don't show entries scheduled in the future in my todo list.
  ;; The idea behind this is that by scheduling it, I don't want to
  ;; think about it until the scheduled date.
  (org-agenda-todo-ignore-scheduled 'future)

  (org-agenda-files '("cumples.org"
		      "todo.org"
		      "agenda-personal.org"))

  (org-capture-templates '(("t" "work todo" entry
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

  (org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "HOLD(h)" "IDEA(i)" "READ(r)" "|" "DONE(d)" "KILL(k)")
		       (sequence "[ ](T)" "|" "[X](D)")))

  ;; System locale to use for formatting time values. Make sure that
  ;; the weekdays in the time stamps of your Org mode files and in the
  ;; agenda appear in English.
  (system-time-locale "C")

  ;; Clock report expressed in hours, not days.
  (org-duration-format 'h:mm)

  ;; Prevent automatic indentation in org mode code blocks
  ;; https://stackoverflow.com/questions/53469017/org-mode-source-editing-indents-code-after-exiting-source-code-block-editor
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)

  ;; On headlines, jump to start/end of headline text
  (org-special-ctrl-a/e t)

  ;; Don't kill tags on headlines
  (org-special-ctrl-k t)

  ;; Leave current line intact when adding a heading with C-RET.
  (org-insert-heading-respect-content t)

  ;; Re-align tags when promoting/demoting
  (org-auto-align-tags t)

  ;; Add a date when a heading is done.
  (org-log-done t)

  ;; Don't ask confirmation when running blocks of code.
  (org-confirm-babel-evaluate nil)

  :config
  ;; Active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((haskell . t)
     (python . t)
     (emacs-lisp . t)))
  )


(provide 'config-org)
;;; config-org.el ends here
