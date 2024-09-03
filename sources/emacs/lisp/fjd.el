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


(defvar fjd_facturas-dir (expand-file-name "~/Sync/keybase/docs/facturas/"))


(defun fjd_mv-facturas ()
  "Move facturas from ~/Downloads to their corresponding directory.

This function could be a shell script but I prefer living in Emacs."
  (interactive)
  (let* ((downloads-dir (expand-file-name "~/Downloads/"))
	 (servicios '("abl"
		      "aysa"
		      "edenor"
		      "naturgy"
		      "osde"
		      "personal"))
	 (downloaded-files (directory-files downloads-dir
					    nil ; relative names
					    ".pdf$")))
    (mapc (lambda (file)
	    (let ((filename (file-name-sans-extension file)))
	      (when (member filename servicios)
		(let ((cmd (concat "mv "
				   downloads-dir
				   file
				   " "
				   fjd_facturas-dir
				   filename
				   "/"
				   (fjd_mv-facturas--next-file-name filename))))
		  (message cmd)
		  (call-process-shell-command cmd)))))
	  downloaded-files)))


(defun fjd_mv-facturas--yy-mm-to-filename (yy-mm)
  "Given YY-MM as (yy mm), return string 'YY-MM.pdf'."
  (apply 'format "%02d-%02d.pdf" yy-mm))


(defun fjd_mv-facturas--filename-to-yy-mm (filename)
  "Parse FILENAME 'YY-MM.pdf' into (YY MM)."
  (mapcar #'string-to-number
	  (s-split "-"
		   (car
		    (s-split "\\."
			     filename)))))


(defun fjd_mv-facturas--next-period (yy-mm)
  "Given YY-MM as (yy mm), add one month and return result."
  (pcase-let ((`(,yy ,mm) yy-mm))
    (if (= mm 12)
	(list (1+ yy) 1)
      (list yy (1+ mm)))))


(defun fjd_mv-facturas--next-file-name (service)
  "Return the filename of the next period for SERVICE."
  (let* ((files (directory-files
		 (concat fjd_facturas-dir service)
		 nil			; relative name
		 "[0-9][0-9]-[0-9][0-9].pdf"))
	 (last-period-filename (car (last files))))
    (-> last-period-filename
	fjd_mv-facturas--filename-to-yy-mm
	fjd_mv-facturas--next-period
	fjd_mv-facturas--yy-mm-to-filename)))



(provide 'fjd)
;;; fjd.el ends here
