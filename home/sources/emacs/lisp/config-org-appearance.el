;;; config-org-appearance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :after config-theme
  :functions org-indent-mode
  :hook
  (org-mode . org-indent-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . org-appear-mode)

  ;; Wrap lines and edit commands act on visual lines.
  (org-mode . visual-line-mode)

  :custom
  ;; Hide * / _ + ~ markers for emphasis such as *bold*. To edit them
  ;; simply backspace over them.
  (org-hide-emphasis-markers t)

  ;; Use 0 to leave one space after heading for tags.
  ;; This settings works better when using different monitor sizes.
  (org-tags-column 0)

  ;; Display UTF characters for entries such as \alpha. Use M-TAB to
  ;; autocomplete.
  (org-pretty-entities t)

  :init

  ;; Used monospaced fonts for tables and checkboxes.
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  ;; FIXME: the color of the checkbox is same as text after this.
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-block nil
		      :foreground 'unspecified
		      :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil
		      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil
		      :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil
		      :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
		      :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil
		      :inherit '(font-lock-comment-face fixed-pitch))

  (let* ((variable-tuple
	  (cond ((x-list-fonts "ETBembo") '(:font "ETBembo"))
		((x-list-fonts "Fira Code") '(:font "Fira Code"))
		((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
		(nil (warn "Cannot find a Sans Serif Font."))))
	 (headline `(:inherit default :weight bold)))
    (custom-theme-set-faces
     'user
     ;; Make title bigger, and headlines smaller and smaller.
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))))

  :custom-face

  (variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
  (fixed-pitch ((t (:family "Fira Code" :height 160))))

  ;; Avoid line spacing issues when a line of text contains both
  ;; variable and fixed-pitch text.
  (org-indent ((t (:inherit (org-hide fixed-pitch))))))

;; (use-package org-superstar
;;   :functions org-superstar-mode
;;   :after org
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "•" "◦"))

;;   :hook
;;   (org-mode . org-superstar-mode))

(use-package org-bullets
  :functions org-bullets-mode
  :after config-theme
  :custom
  (org-bullets-bullet-list '("⋄" "○" "◦" "•"))

  :hook
  (org-mode . org-bullets-mode))

;; Make it easier to edit hidden chars such as links.
(use-package org-appear
  :after config-theme
  :custom
  ;; Toggle `[[https://myurl.org][My Url]]` with `My Url`.
  (org-appear-autolinks t)

  ;; Toggle `*bold*` with `bold`.
  (org-appear-autoemphasis t))

(provide 'config-org-appearance)
;;; config-org-appearance.el ends here
