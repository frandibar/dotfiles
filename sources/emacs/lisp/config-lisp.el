;;; config-lisp.el --- -*- lexical-binding: t -*-
;;; Commentary:

;; My settings for Lisp programming.

;;; Code:

;; Cool lisp editing.
(use-package lispy
  :functions lispy-mode
  :hook
  (lisp-mode . lispy-mode)
  (emacs-lisp-mode . lispy-mode))


;; Steel Bank Common Lisp.
(use-package inf-lisp
  :custom
  (inferior-lisp-program "sbcl"))


;; Lisp REPL.
;; Settings for nyxt development.
;; https://github.com/atlas-engineer/nyxt/blob/master/documents/README.org
(use-package sly
  :init
  (load "~/common-lisp/nyxt/build-scripts/nyxt-guix.el" :noerror)

  (setq sly-lisp-implementations
   '((nyxt-sbcl
      (lambda () (nyxt-make-guix-cl-for-nyxt
	     "~/common-lisp/nyxt"
	     ;; You may set it to t, if you experience odd behavior.
	     :force nil
	     :cl-implementation "sbcl"
	     :cl-system "nyxt/gi-gtk"
	     :no-grafts t
	     :ad-hoc '("emacs" "xdg-utils" "git"))))))
  )


(provide 'config-sbcl)
;;; config-lisp.el ends here
