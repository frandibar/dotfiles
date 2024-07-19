;;; config-ace-jump.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; This package allows jumping the cursor around with very few
;; keypresses.
(use-package ace-jump-mode
  :bind
  (("C-c v j" . ace-jump-mode)))

(provide 'config-ace-jump)
;;; config-ace-jump.el ends here
