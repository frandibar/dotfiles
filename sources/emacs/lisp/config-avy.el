;;; config-avy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ring)
(require 'avy)

;; Extracted from https://karthinks.com/software/avy-can-do-anything
(defun fjd_avy-action-kill-whole-line (destination-point)
  "An avy action to kill whole line where DESTINATION-POINT is."
  (save-excursion
    (goto-char destination-point)
    (kill-whole-line))
  (select-window (cdr (ring-ref avy-ring 0)))
  t)


(defun fjd_avy-action-copy-whole-line (destination-point)
  "An avy action to copy whole line where DESTINATION-POINT is."
  (save-excursion
    (goto-char destination-point)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window (cdr (ring-ref avy-ring 0)))
  t)


(defun fjd_avy-action-yank-whole-line (destination-point)
  "An avy action to yank whole line where DESTINATION-POINT is."
  (fjd_avy-action-copy-whole-line point)
  (save-excursion (yank))
  t)


(defun fjd_avy-action-teleport-whole-line (destination-point)
  "An avy action to kill and yank whole line where DESTINATION-POINT is."
  (fjd_avy-action-kill-whole-line destination-point)
  (save-excursion (yank)) t)


(defun fjd_avy-action-yank-stay (destination-point)
  "An avy action to yank at DESTINATION-POINT.  Stay at current point."
  (save-excursion
    (goto-char destination-point)
    (yank)))


;; This package allows jumping the cursor around with very few
;; keypresses.
(use-package avy
  :defines fjd_custom-bindings-map
  :config
  (setf (alist-get ?K avy-dispatch-alist) 'fjd_avy-action-kill-whole-line)
  (setf (alist-get ?W avy-dispatch-alist) 'fjd_avy-action-copy-whole-line)
  (setf (alist-get ?Y avy-dispatch-alist) 'fjd_avy-action-yank-whole-line)
  (setf (alist-get ?T avy-dispatch-alist) 'fjd_avy-action-teleport-whole-line)
  (setf (alist-get ?p avy-dispatch-alist) 'fjd_avy-action-yank-stay)

  :bind
  (:map fjd_custom-bindings-map
	(
	 ("C-:" . avy-goto-char-timer) ; Used so much, An exception to C-c v prefix
	 ("C-c v j t" . avy-goto-char-timer) ; duplicate for discoverability
	 ("C-c v j l" . avy-goto-line)))

  :custom
  ;; Give a chance to select an action before jumping
  (avy-single-candidate-jump nil))

(provide 'config-avy)
;;; config-avy.el ends here
