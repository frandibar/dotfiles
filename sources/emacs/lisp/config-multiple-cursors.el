;;; config-multiple-cursors.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package multiple-cursors
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	("C-c v m" . mc/edit-lines)))

(use-package region-bindings-mode
  :defines (region-bindings-mode-map
	    region-bindings-mode-enable)
  :bind
  (:map region-bindings-mode-map
	("a" . mc/mark-all-like-this)
	("p" . mc/mark-previous-like-this)
	("n" . mc/mark-next-like-this))

  :custom
  ;; Don't set these bindings for read only buffers
  (region-bindings-mode-disable-predicates (lambda () buffer-read-only))

  :config
  (region-bindings-mode-enable))


(provide 'config-multiple-cursors)
;;; config-multiple-cursors.el ends here
