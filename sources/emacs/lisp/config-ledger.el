;;; config-ledger.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ledger-mode
  :after fjd
  :defines fjd_update-cash
  :hook
  (ledger-mode . (lambda () (add-hook 'after-save-hook 'fjd_update-cash nil t)))

  :custom
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
