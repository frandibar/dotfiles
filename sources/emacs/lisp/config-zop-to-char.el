;;; config-zop-to-char.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package zop-to-char
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	(([remap zap-to-char] . zop-to-char))))

(provide 'config-zop-to-char)
;;; config-zop-to-char.el ends here
