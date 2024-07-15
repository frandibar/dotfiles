;; Expand region increases the selected region by semantic units. Just
;; keep pressing the key until it selects what you want.
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))
