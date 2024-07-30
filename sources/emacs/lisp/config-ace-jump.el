;;; config-ace-jump.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This package allows jumping the cursor around with very few
;; keypresses.
(use-package ace-jump-mode
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	(("C-c v j" . ace-jump-mode))))

(provide 'config-ace-jump)
;;; config-ace-jump.el ends here
