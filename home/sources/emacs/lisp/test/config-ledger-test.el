;;; config-ledger-test.el --- ERT for config-ledger.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "straight/repos/ledger-mode"))


(require 'config-ledger)
(require 'ledger-mode)


(ert-deftest move-xact-down-test ()
  (ert-test-erts-file "config-ledger-move-xact-down.erts"
		      (lambda () (progn
			      (ledger-mode)
			      (fjd_ledger-move-xact-down)))))


(ert-deftest move-xact-up-test ()
  (ert-test-erts-file "config-ledger-move-xact-up.erts"
		      (lambda () (progn
			      (ledger-mode)
			      (fjd_ledger-move-xact-up)))))


(provide 'config-ledger-test)
;;; config-ledger-test.el ends here
