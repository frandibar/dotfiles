;;; config-ibuffer.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ibuffer is part of Emacs
;; Use ibuffer instead of list-buffers.
(use-package ibuffer
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	([remap list-buffers] . ibuffer))

  :hook
  (ibuffer-mode . (lambda () (ibuffer-switch-to-saved-filter-groups "Main")))

  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("Main"
      ("Directories" (mode . dired-mode))
      ("Ledger" (mode . ledger-mode))
      ("Org" (mode . org-mode))
      ("Programming" (mode . prog-mode))
      ("Emacs" (or
                (name . "^\\*Help\\*$")
                (name . "^\\*Custom.*")
                (name . "^\\*Org Agenda\\*$")
                (name . "^\\*info\\*$")
                (name . "^\\*scratch\\*$")
                (name . "^\\*Backtrace\\*$")
                (name . "^\\*Messages\\*$")))))))

(provide 'config-ibuffer)
;;; config-ibuffer.el ends here
