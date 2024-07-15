;;; early-init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Francisco Dibar
;;
;; Author: Francisco Dibar <frandibar@gmail.com>
;; Maintainer: Francisco Dibar <frandibar@gmail.com>
;; Created: July 13, 2024
;; Modified: July 13, 2024
;; Version: 0.0.1
;; Keywords: 
;; Homepage: https://github.com/frandibar/early-init
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
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
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
