;;; fjd.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Prevent compiler warnings
(declare-function lispy-forward "lispy")
(declare-function s-split "s")
(declare-function special-lispy-tab "lispy")


(defun fjd_generate-cash-org-file ()
  "Generate a ledger balance report from `main.ledger' into `cash.org'."
  (interactive)
  (let* ((ledger-file (expand-file-name
		       (file-name-concat "~" "Sync" "ledger" "main.ledger")))
	 (command (concat "ledger bal --file " ledger-file
			  " cash brubank ciudad uala galicia cocos:cocorma --cleared --flat"))
	 (output-file (expand-file-name
		       (file-name-concat "~" "Sync" "docs" "cash.org")))
	 (headers (list "#+title: Caja\n"
			"#+date: "
			;; #+date: lun 02 jun 2025 14:12:49 -03
			(format-time-string "%A %B %d %Y %H:%M:%S %z%n")
			"\nEste archivo se genera automáticamente al grabar main.ledger, NO EDITAR.\n\n"
			"#+begin_example\n\n"))
	 (ledger-output (with-temp-buffer
			  (call-process-shell-command command nil t)
			  (buffer-string)))
	 ;; Remove "assets:" prefix
	 (output-makeup-1 (replace-regexp-in-string "assets:" "" ledger-output))
	 ;; Remove decimals
	 (output-makeup-2 (replace-regexp-in-string ",[0-9][0-9]" "" output-makeup-1))
	 (final-output output-makeup-2)
	 (footers (list "\n#+end_example\n")))

    (with-temp-file output-file
      (mapc #'insert headers)
      (insert final-output)
      (mapc #'insert footers))

    (message "Ledger report saved to %s" output-file)))


(defun fjd_truncate-string (string max-length)
  "Truncate STRING if longer than MAX-LENGTH."
  (if (> (length string) max-length)
      (concat (substring string 0 (- max-length 1)) "…")
    string))


(defun fjd_current-line-to-kill-ring ()
  "Add the current line to the kill ring."
  (interactive)
  (save-excursion
    (kill-whole-line)
    (yank)))


(defvar fjd_facturas-dir (file-name-as-directory
			  (expand-file-name
			   (file-name-concat "~" "Sync" "keybase" "docs" "facturas"))))


(defun fjd_mv-facturas ()
  "Move facturas from ~/Downloads to their corresponding directory."
  (interactive)
  (let* ((downloads-dir (file-name-as-directory
			 (expand-file-name
			  (file-name-concat "~" "Downloads"))))
	 (servicios '("abl"
		      "aysa"
		      "edenor"
		      "naturgy"
		      "osde"
		      "personal"))
	 (downloaded-files (directory-files downloads-dir
					    t ; absolute paths
					    "\\.[pP][dD][fF]$"))
	 (files-to-move (seq-filter (lambda (file)
				      (member (file-name-base file)
					      servicios))
				    downloaded-files)))
    (if files-to-move
	(let ((output-buffer (generate-new-buffer "*facturas*")))
	  (with-output-to-temp-buffer output-buffer
	    (mapc (lambda (file)
		    (let ((new-file-name
			   (file-name-concat fjd_facturas-dir
					     (file-name-base file)
					     (fjd_mv-facturas--next-file-name
					      (file-name-nondirectory file)))))
		      (rename-file file new-file-name)
		      (princ (concat "Moved " file " into " new-file-name "\n"))))
		  files-to-move)))
      (message "Nothing to move."))))


(defun fjd_mv-facturas--yyyy-mm-to-filename (yyyy-mm extension)
  "Given YYYY-MM as (yyyy mm), return string `YYYY-MM.[EXTENSION]'."
  (concat (apply #'format "%02d-%02d" yyyy-mm) "." extension))


(defun fjd_mv-facturas--filename-to-yyyy-mm (filename)
  "Parse FILENAME such as `YYYY-MM.pdf' into (YYYY MM)."
  (mapcar #'string-to-number
	  (s-split "-" (file-name-sans-extension filename))))


(defun fjd_mv-facturas--next-period (yyyy-mm)
  "Given YYYY-MM as (yyyy mm), add one month and return result."
  (pcase-let ((`(,yyyy ,mm) yyyy-mm))
    (if (= mm 12)
	(list (1+ yyyy) 1)
      (list yyyy (1+ mm)))))


(defun fjd_mv-facturas--next-file-name (filename)
  "Return the filename of the next period for FILENAME."
  (let* ((files (directory-files
		 (file-name-concat fjd_facturas-dir
				   (file-name-sans-extension filename))
		 nil			; relative name
		 "[0-9][0-9][0-9][0-9]-[0-9][0-9]\\."))
	 (last-period-filename (car (last files))))

    (fjd_mv-facturas--yyyy-mm-to-filename
     (fjd_mv-facturas--next-period
      (fjd_mv-facturas--filename-to-yyyy-mm last-period-filename))
     (file-name-extension filename))))


(defun fjd_insert-google-maps-link ()
  "Insert a Google Maps link.
Link gets inserted after the marked region."
  (interactive)
  (when (use-region-p)
    (save-excursion
      ;; Make sure the insertion goes after the region.
      (when (< (point) (region-end))
	(exchange-point-and-mark))
      (insert (concat "\n[[https://www.google.com/maps/place/"
		      (string-replace " " "+"
				      (buffer-substring (region-beginning)
							(region-end)))
		      "][mapa]]")))))


(defun fjd_lispy-format-buffer ()
  "Format each sexp in buffer using `special-lispy-tab'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (lispy-forward 1)
      (special-lispy-tab))))


(provide 'fjd)
;;; fjd.el ends here
