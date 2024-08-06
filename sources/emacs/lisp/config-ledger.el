;;; config-ledger.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun fjd_ledger-move-xact (direction)
  "Move current xact upwards or downwards.
`DIRECTION' can be `up' or `down'."
  (when (derived-mode-p 'ledger-mode)
    (let ((begin (car (ledger-navigate-find-element-extents (point))))
          (end (save-excursion
                 (ledger-navigate-next-xact-or-directive)
                 (point))))
      (progn
        (kill-region begin end)
        (cond ((eq direction 'up)
               (ledger-navigate-prev-xact-or-directive))
              ((eq direction 'down)
               (ledger-navigate-next-xact-or-directive))
              (t (error "Invalid argument value for direction: %s" direction)))
        (yank)
        (ledger-navigate-prev-xact-or-directive)))))


(defun fjd_ledger-move-xact-down ()
  "Move current xact downwards."
  (interactive)
  (fjd_ledger-move-xact 'down))


(defun fjd_ledger-move-xact-up ()
  "Move current xact upwards."
  (interactive)
  (fjd_ledger-move-xact 'up))


(use-package ledger-mode
  :after fjd
  :defines fjd_update-cash fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
        (("C-c v l c" . ledger-mode-clean-buffer))
        (("C-M-p" . fjd_ledger-move-xact-up))
        (("C-M-n" . fjd_ledger-move-xact-down)))
  :hook
  (ledger-mode . (lambda () (add-hook 'after-save-hook 'fjd_update-cash nil t)))
  (ledger-report-mode . hl-line-mode)

  :custom
  ;; Add empty line after copying a transaction
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-schedule-file "~/Sync/ledger/main-schedule.ledger")

  (ledger-reports
   '(("assets-in-usd" "%(binary) bal --price-db prices.db asset --current --exchange usd")
     ("income-vs-expenses" "%(binary) bal expense income --depth 1 --current --price-db prices.db --exchange usd --period %(month)")
     ("expenses-this-month" "%(binary) bal expense --period %(month) --current --sort amount")
     ("expenses-with-tag" "%(binary) reg expense --current --limit 'has_tag(/%(tagvalue)/)'")
     ("balance-stablecoins" "%(binary) bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/'")
     ("balance-stablecoins-in-usd" "%(binary) bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/' --price-db prices.db --exchange usd")
     ("expenses-pending-this-month" "%(binary) bal expense --period %(month) --uncleared --sort amount")
     ("unbudgeted-expenses-this-month" "%(binary) bal expense --unbudgeted --monthly --sort amount --period %(month)")
     ("budgeted-ars-next-month" "%(binary) reg assets:cash:ars --budget --period 'next month'")
     ("servicios-pagados-mes-actual" "%(binary) reg expenses:servicio --cleared --current --period 'this month'")
     ("prices-ccy" "%(binary) prices btc")
     ("bal" "%(binary) bal")
     ("reg" "%(binary) reg")
     ("payee" "%(binary) reg @%(payee)")
     ("account" "%(binary) reg %(account)"))))

(provide 'config-ledger)
;;; config-ledger.el ends here
