;;; package --- frandibar.el

;;; Code:

;;;###autoload

;; Preferably redefine this function without dependency to evil
(defun frandibar/yank-whole-line (number-of-lines)
  "Yank NUMBER-OF-LINES lines."
  (interactive "p")
  (evil-visual-line)
  (save-excursion
    (let ((from (point-at-bol))
          (to (progn
                (evil-next-line (- number-of-lines 1))
                (point-at-eol))))
      (evil-visual-line)
      (evil-yank-line from to)
      (evil-exit-visual-state)
      (message (format "Yanked %s line(s)" number-of-lines))
      )))

(defun frandibar/elm-sort-record ()
  "Sort a record"
  (interactive)
  (evil-visual-char)
  (save-excursion
  (while (not (string= "{" (buffer-substring-no-properties (- (region-beginning) 1) (+ 0 (region-beginning)))))
    (er/expand-region 1))
  ;; (elm-format-region (region-beginning) (region-end))
  ;; (let ((region (buffer-substring-no-properties (+ 1 (region-beginning)) (- (region-end) 1))))
  ;; ;;   (concat (sort (split-string region ",") 'string< ) ",")
  ;;   )
  ))

"""
{ model
    | x : Int
    , y : String
}

{ x : Int , y : String }

{ x = 1 , y = 2 }

{ model | x = 1 , y = 2 }

{ x = (2, 3) , y = 2 }

"""

(provide 'frandibar)
