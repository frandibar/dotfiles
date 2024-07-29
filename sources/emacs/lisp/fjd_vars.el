;;; fjd_vars.el --- -*- lexical-binding: t; -*_
;;; Commentary:
;;; Code:

;; All my bindings are kept in this map and I activate it at the end
;; of the configuration.
;; Some benefits of doing this:
;; - It keeps my custom bindings from being overwritten by extensions’
;; own bindings.
;; - Help docs for minor mode show all bindings in a single place,
;;   although `describe-personal-keybindings` helps too.
;; - It gives visibility on how far away I am from the defaults. After
;; - a long time using them, I may forget if they are my own or not.
(defvar fjd_custom-bindings-map (make-keymap)
  "A keymap for my custom keybindings.")

(provide 'fjd_vars)
;;; fjd_vars.el ends here
