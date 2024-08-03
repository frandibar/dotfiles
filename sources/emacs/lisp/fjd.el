;;; fjd.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun fjd_update-cash ()
  "Update cash.org with the output of cash.sh."
  (interactive)
  (call-process-shell-command "~/Sync/ledger/cash.sh > ~/Sync/docs/cash.org" nil 0))


(defun fjd_truncate-string (string length)
  "Truncate STRING if longer than LENGTH."
  (if (> (length string) length)
      (concat (substring string 0 (- length 1)) "…")
    string))


(defun fjd_current-line-to-kill-ring ()
  "Add the current line to the kill ring."
  (interactive)
  (save-excursion
    (kill-whole-line)
    (yank)))


(provide 'fjd)
;;; fjd.el ends here
