;; https://github.com/magnars/multiple-cursors.el?tab=readme-ov-file
(use-package multiple-cursors
  :bind
  ("C-c v m" . mc/edit-lines))

;; https://github.com/fgallina/region-bindings-mode
(use-package region-bindings-mode
  :bind
  (:map region-bindings-mode-map
   ("a" . mc/mark-all-like-this)
   ("p" . mc/mark-previous-like-this)
   ("n" . mc/mark-next-like-this))
  :custom
  ;; Don't set these bindings for read only buffers
  (region-bindings-mode-disable-predicates (lambda () buffer-read-only))
  )
