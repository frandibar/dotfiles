;;; fjd.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun fjd_update-cash ()
  "Update cash.org with the output of cash.sh."
  (interactive)
  (call-process-shell-command "~/Sync/ledger/cash.sh > ~/Sync/docs/cash.org" nil 0))

(provide 'fjd)
;;; fjd.el ends here
