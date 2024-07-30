;;; config-expand-region.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Expand region increases the selected region by semantic units. Just
;; keep pressing the key until it selects what you want.
(use-package expand-region
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	("C-=" . er/expand-region)
	("C-+" . er/contract-region)))

(provide 'config-expand-region)
;;; config-expand-region.el ends here
