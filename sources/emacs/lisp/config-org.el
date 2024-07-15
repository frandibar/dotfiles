;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/docs/")

;; Don't show entries scheduled in the future in my todo list.
;; The idea behind this is that by scheduling it, I don't want to
;; think about it until the scheduled date.
(setq org-agenda-todo-ignore-scheduled 'future)

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

  ;; Hide * / _ + ~ markers for emphasis such as *bold*. To edit them simply backspace over them.
  (setq org-hide-emphasis-markers t)

  )
