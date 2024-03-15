(use-package frandibar
  :load-path "~/.config/doom/modules/frandibar"
  :bind (:map evil-normal-state-map
              ("Y" . 'frandibar/yank-whole-line)))
;; Remap Y from evil-yank-line since it doesn't behave as I would expect
;; I would have expected the following to work, but instead I rely on frandibar/yank-whole-line
;;
;; (define-key evil-normal-state-map (kbd "Y") 'evil-collection-magit-yank-whole-line)
