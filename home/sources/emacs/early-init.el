;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Turn off unwanted GUI elements.
(menu-bar-mode 0)

;; Disable vertical scroll bars.
(scroll-bar-mode 0)

;; Disable the tool bar in all graphical frames.
(tool-bar-mode 0)

;; When Tooltip mode is disabled, Emacs displays help text in the
;; echo area, instead of making a pop-up window.
(tooltip-mode 0)

;; Disable default package manager in favor of straight.el
;; https://github.com/radian-software/straight.el
(setopt package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
