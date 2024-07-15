;; Make dired emulate midnight commander to having two buffers side by side and
;; moving files between both.
;; Emacs default is nil, doom's is t.
(setq dired-dwim-target 'dired-dwim-target-next)

;; Use xdg-open to resolve application to use when opening these files types.
(setq dired-guess-shell-alist-user
      '(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" "xdg-open")
      ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" "xdg-open")
      ("\\.\\(?:xcf\\)\\'" "xdg-open")
      ("\\.csv\\'" "xdg-open")
      ("\\.tex\\'" "xdg-open")
      ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "xdg-open")
      ("\\.\\(?:mp3\\|flac\\)\\'" "xdg-open")
      ("\\.html?\\'" "xdg-open")
      ("\\.md\\'" "xdg-open")))
