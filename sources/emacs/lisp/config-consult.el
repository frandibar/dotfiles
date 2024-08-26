;;; config-consult.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; Consult provides search and navigation commands based on the Emacs
;; completion function completing-read. Completion allows you to
;; quickly select an item from a list of candidates.
(use-package consult
  :defines fjd_custom-bindings-map
  :bind
  ;; Use consult-buffer for listing buffers with C-x C-b
  ;; consult-buffer is an enhanced version of switch-to-buffer with
  ;; support for virtual buffers. Supports live preview of buffers and
  ;; narrowing to the virtual buffer types. You can type f SPC in
  ;; order to narrow to recent files. Press SPC to show ephemeral
  ;; buffers. Supported narrowing keys:
  ;;   b Buffers
  ;;   SPC Hidden buffers
  ;;   * Modified buffers
  ;;   f Files (Requires recentf-mode)
  ;;   r File registers
  ;;   m Bookmarks
  ;;   p Project
  (:map fjd_custom-bindings-map
	(([remap repeat-complex-command] . consult-complex-command)
	 ([remap switch-to-buffer] . consult-buffer)
	 ([remap switch-to-buffer-other-window] . consult-switch-to-buffer-other-window)
	 ([remap switch-to-buffer-other-frame] . consult-switch-to-buffer-other-frame)
	 ([remap switch-to-buffer-other-tab] . consult-switch-to-buffer-other-tab)
	 ([remap bookmark-jump] . consult-bookmark)
	 ([remap project-switch-to-buffer] . consult-project-switch-to-buffer)
	 ([remap yank-pop] . consult-yank-pop)
	 ([remap goto-line] . consult-goto-line)
	 ([remap occur] . consult-line)
	 ("C-c v b" . consult-global-mark)
	 ("C-c v r" . consult-ripgrep)
	 ("C-c v o d" . consult-org-agenda)
	 ("C-c v o h" . consult-org-heading))

	:map isearch-mode-map
	([remap isearch-edit-string] . consult-isearch-history)

	:map minibuffer-local-map
	([remap next-matching-history-element] . consult-history)
	([remap previous-matching-history-element] . consult-history)
	))

;; Adds annotations to minibuffer completions
(use-package marginalia
  :functions marginalia-mode
  :config
  (marginalia-mode 1))

;; Embark makes it easy to choose a command to run based on what is
;; near point, both during a minibuffer completion session and in
;; normal buffers.
(use-package embark
  :defines fjd_custom-bindings-map
  :bind
  (:map fjd_custom-bindings-map
	(("C-." . embark-act)	 ;; pick some comfortable binding
	 ("C-;" . embark-dwim))) ;; good alternative: M-.

  :custom
  ;; Optionally replace the key help with a completing-read interface
  (prefix-help-command embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Edit Occur buffers in place for editing multiple matches in only
;; one place.
(use-package wgrep)

;; I haven't found a way of filtering by filetype or changing the
;; arguments passed to ripgrep. Until then use this package.
(use-package deadgrep)

(provide 'config-consult)
;;; config-consult.el ends here
