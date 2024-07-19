;;; config-orderless.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; This package provides an orderless completion style that divides
;; the pattern into space-separated components, and matches candidates
;; that match all of the components in any order.
(use-package orderless
  :custom
  ;; The basic completion style is specified as fallback in addition
  ;; to orderless in order to ensure that completion commands which
  ;; rely on dynamic completion tables, e.g., completion-table-dynamic
  ;; or completion-table-in-turn, work correctly. Furthermore the
  ;; basic completion style needs to be tried first (not as a
  ;; fallback) for TRAMP hostname completion to work. In order to
  ;; achieve that, we add an entry for the file completion category in
  ;; the completion-category-overrides variable. In addition, the
  ;; partial-completion style allows you to use wildcards for file
  ;; completion and partial paths, e.g., /u/s/l for /usr/share/local.

  (completion-styles '(orderless partial-completion basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'config-orderless)
;;; config-orderless.el ends here
