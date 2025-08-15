;;; config-ledger.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)


(defun fjd_ledger-move-xact (direction)
  "Move current xact upwards or downwards.
`DIRECTION' can be `up' or `down'."

  (unless (derived-mode-p 'ledger-mode)
    (user-error "Not in a ledger-mode buffer"))

  (let ((begin (car (ledger-navigate-find-element-extents (point))))
	(end (save-excursion
	       (ledger-navigate-next-xact-or-directive)
	       (point)))
	(cursor-off-bounds
	 (cl-reduce #'eq
		    (ledger-navigate-find-element-extents (point)))))
    (if cursor-off-bounds
	;; This happens when point is on an empty line between posts.
	(message "Nothing selected.")
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


(defun fjd_toggle-decimal-separator ()
  "Toggle between ',' and '.' in NUMBER-AS-STRING.
This function does a dumb char replacement, it does not check
whether the number is valid."
  (interactive)
  (save-excursion
    (let* ((placeholder "<COMMA-WAS-HERE>")
	   (start (pos-bol))
	   (end (pos-eol))
	   (tmp1 (string-replace "," placeholder (buffer-substring start end)))
	   (tmp2 (string-replace "." "," tmp1))
	   (result (string-replace placeholder "." tmp2)))
      (delete-and-extract-region start end)
      (insert result))))


(defun fjd_ledger-post-edit-amount ()
  "This is like `ledger-post-edit-amount' but it works with comma as
decimal separator.  After copying the result in the calc buffer
with `y' (remember to show grouping with `g d', you should call
`fjd_toggle-decimal-separator'."
  (interactive)
  (fjd_toggle-decimal-separator)
  (ledger-post-edit-amount))

(use-package ledger-mode
  :after fjd
  :defines fjd_update-cash fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
        (("C-c v l c" . ledger-mode-clean-buffer))
        (("C-M-p" . fjd_ledger-move-xact-up))
        (("C-M-n" . fjd_ledger-move-xact-down))
	(("C-c C-b" . fjd_ledger-post-edit-amount)))
  :hook
  (ledger-report-mode . hl-line-mode)

  :custom
  ;; Add empty line after copying a transaction
  (ledger-copy-transaction-insert-blank-line-after t)
  (ledger-schedule-file "~/Sync/ledger/main-schedule.ledger")

  (ledger-reports
   (let ((prefix "%(binary) -f %(ledger-file) "))
     (mapcar
      (lambda (pair) (list (car pair) (s-concat prefix (cadr pair))))
      '(("assets-in-usd"
	 "bal --price-db prices.db asset --current --exchange usd")

	("income-vs-expenses"
	 "bal expense income --depth 1 --current --price-db prices.db --exchange usd --period %(month)")

	("expenses-this-month"
	 "bal expense --period %(month) --current --sort amount")

	("expenses-with-tag"
	 "reg expense --current --limit 'has_tag(/%(tagvalue)/)'")

	("balance-stablecoins"
	 "bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/'")

	("balance-stablecoins-in-usd"
	 "bal asset --sort amount --limit 'commodity=~/^busd$/ or commodity=~/^usdc$/ or commodity=~/^usdp$/ or commodity=~/^usdt$/' --price-db prices.db --exchange usd")

	("expenses-pending-this-month"
	 "bal expense --period %(month) --uncleared --sort amount")

	("unbudgeted-expenses-this-month"
	 "bal expense --unbudgeted --monthly --sort amount --period %(month)")

	("budgeted-ars-next-month"
	 "reg assets:cash:ars --budget --period 'next month'")

	("servicios-pagados-mes-actual"
	 "reg expenses:servicio --cleared --current --period 'this month'")

	("prices-ccy" "prices btc")
	("bal" "bal")
	("reg" "reg")
	("payee" "reg @%(payee)")
	("account" "reg %(account)"))))))


(when (eq system-type 'windows-nt)
  ;; Quick & Dirty fix. Don't know why it's not taking `exec-path'
  ;; into account.
  (setq ledger-binary-path "C:\\ProgramData\\chocolatey\\bin\\ledger.exe"))


(provide 'config-ledger)
;;; config-ledger.el ends here
