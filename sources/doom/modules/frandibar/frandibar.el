;;; package --- frandibar.el
;;; Commentary:
;;; Code:

(require 'evil)

;;;###autoload
(defun frandibar/increment-integer-at-point (amount-to-add)
  "Increment the absolute value of number at point by AMOUNT-TO-ADD.
Default to 1. Minus sign is ignored.
i.e. -n1 turns into -n2 when cursor is over 1."
  (interactive "p")
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (+ amount-to-add
                                        (string-to-number (match-string 0))))))
  )

(defun frandibar/yank-whole-line (number-of-lines)
  "Yank NUMBER-OF-LINES lines."
  (interactive "p")
  (evil-visual-line)
  (save-excursion
    (let ((from (line-beginning-position))
          (to (progn
                (evil-next-line (- number-of-lines 1))
                (line-end-position))))
      (evil-visual-line)
      (evil-yank-line from to)
      (evil-exit-visual-state)
      (message (format "Yanked %s line(s)" number-of-lines))
      )))

(defun frandibar/filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard.
Extracted from URL `http://emacsredux.com'."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; (defun frandibar/elm-sort-record ()
;;   "Sort an Elm record. NOT FINISHED YET."
;;   (interactive)
;;   (evil-visual-char)
;;   ;; (save-excursion
;;   ;; (while (not (string= "{"
;;   ;;                      (buffer-substring-no-properties (- (region-beginning) 1) (+ 0 (region-beginning)))))
;;   ;;   (er/expand-region 1))
;;   ;; (elm-format-region (region-beginning) (region-end))
;;   ;; (let ((region (buffer-substring-no-properties (+ 1 (region-beginning)) (- (region-end) 1))))
;;   ;; ;;   (concat (sort (split-string region ",") 'string< ) ",")
;;   ;;   )
;;   ))

;; """
;; { model
;;     | x : Int
;;     , y : String
;; }

;; { x : Int , y : String }

;; { x = 1 , y = 2 }

;; { model | x = 1 , y = 2 }

;; { x = (2, 3) , y = 2 }

;; """

(provide 'frandibar)
;;; frandibar.el ends here
