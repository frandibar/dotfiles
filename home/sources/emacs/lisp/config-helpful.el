;;; config-helpful.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; This package provides much better contextual information when
;; looking for help.
(use-package helpful
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	([remap describe-function] . helpful-callable)
	([remap describe-variable] . helpful-variable)
	([remap describe-key] . helpful-key)
	([remap describe-command] . helpful-command)))

(provide 'config-helpful)
;;; config-helpful.el ends here
