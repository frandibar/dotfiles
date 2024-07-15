;; Vertico provides a minimalistic vertical completion UI
(use-package vertico
  :custom
  ;; Number of candidates to show (default is 10)
  (vertico-count 20)
  :init
  (vertico-mode))
