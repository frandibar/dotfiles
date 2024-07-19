;;; config-lilypond.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))
;; FIXME: fix path for NixOS
(setq load-path (append (list "/usr/share/emacs/site-lisp") load-path))
(setq LilyPond-pdf-command "zathura")

(provide 'config-lilypond)
;;; config-lilypond.el ends here
