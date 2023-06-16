;;; package --- mgs.el

;;; Commentary:
;;; Custom library for use at Metronomics

;;; Code:

;;;###autoload

(require 'elm-mode)

(defun mgs/daily-huddle ()
  "Open the daily huddle Zoom link."
  (interactive)
  (browse-url "https://us02web.zoom.us/j/88522231511?pwd=NUNDZmNraGRhRXBiOVcvTDlIOVhIQT09"))

(defun mgs/browse-jira-ticket (ticket-number)
  "Open the Jira ticket with the given TICKET-NUMBER."
  (interactive "n\Ticket number (without prefix): ")
  (browse-url (concat "https://metronomegrowthsystems.atlassian.net/browse/MGS-" (number-to-string ticket-number))))

(defun mgs/browse-pull-requests ()
  "Open the GitHub pull requests page."
  (interactive)
  (browse-url "https://github.com/Metronome-Software/app/pulls"))

(defun mgs/run-tests ()
  "Run elm-test and display results in an org buffer."
  (interactive)
  (shell-command "elm-test" "elm-test")
  (switch-to-buffer "elm-test")
  (delete-other-windows)
  (mgs/test-to-org))

(defun mgs/test-to-org ()
  "Convert the elm-test output to an org-mode buffer."
  (interactive)
  (save-excursion
    (replace-regexp "^↓" "* ↓" nil (point-min) (point-max))
    (replace-regexp "^✗" "* ✗" nil (point-min) (point-max))
    (replace-regexp "^\\(    ▼ Query.fromHtml\\)" "**\\1\n#+BEGIN_SRC html\n" nil (point-min) (point-max))
    (replace-regexp "^\\(    ▼ Query.find\\)" "#+END_SRC\n**\\1" nil (point-min) (point-max))
    (replace-regexp "^\\(    ▼ Query.has\\)" "#+END_SRC\n**\\1" nil (point-min) (point-max))
    (replace-regexp "^\\(TEST RUN FAILED\\)" "* \\1" nil (point-min) (point-max)))
  (org-mode))

(defun mgs/view-org-html-block ()
  "View the current 'org-mode' block as HTML."
  (interactive)
  (org-narrow-to-block)
  (copy-to-buffer "tmp" (point-min) (point-max))
  (widen)
  (switch-to-buffer "tmp")
  (save-excursion
    (goto-char (point-min))
    (kill-whole-line)
    (goto-char (point-max))
    (kill-whole-line))
  (mhtml-mode))


(defun mgs/fold-style-blocks ()
  "Fold all html style blocks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<style>" (point-max) t)
      (+fold/close))))

(defun mgs/format-elm-debug-line ()
  "Format the current line as an Elm Debug.log statement."
  (interactive)
  (copy-to-buffer "tmp" (line-beginning-position) (line-end-position))
  (switch-to-buffer "tmp")
  (save-excursion
    (search-forward ":" nil t)
    (replace-match "_=" nil t)
    (replace-string "," ",\n" nil (point-min) (point-max))
    (replace-string "}" "}\n" nil (point-min) (point-max))
    (replace-string "<function>" "function" nil (point-min) (point-max)))
  (goto-char (point-min))
  (elm-format-buffer)
  (elm-mode))

(provide 'mgs)
;;; mgs.el ends here
