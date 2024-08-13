;;; ert-helpers.el --- ERT helpers -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  ERT is Emacs Regression Testing library.
;;  This module provides helper functions.
;;
;;; Code:

(require 'cl-lib)


(defun fjd__replace-newlines (string)
  "Replace escaped newlines in STRING with actual newlines."
  (s-replace "\\n" "\n" string))


(defun fjd_kill-result-buffers ()
  "Kill all buffers that where created when calling `fjd_ert-results-diff'."
  (interactive)
  (mapcar #'kill-buffer
	  (cl-remove-if-not (lambda (buffer)
			      (or (s-starts-with? "expected-" (buffer-name buffer))
				  (s-starts-with? "actual-" (buffer-name buffer))))
			    (buffer-list))))


(defun fjd_ert-results-diff ()
  "Run ediff on the results of an ert test result.

An ert output may look like:

F my-test
    \(ert-test-failed
      \(\"Mismatch in test \"some-test\", file some-file.erts\"
       \"some output\"
       \"another output\"))

This function must be called with the cursor inside de sexp within
`ert-test-failed' that you want to compare."

  (interactive)
  (let* ((result
	  (mapcar #'fjd__replace-newlines
		  (read (buffer-substring (pos-bol) (pos-eol)))))
	 (buffer-suffix (make-temp-name ""))
	 (buffer-A (get-buffer-create (s-join "-" (list "actual" buffer-suffix))))
	 (buffer-B (get-buffer-create (s-join "-" (list "expected" buffer-suffix)))))

    (with-current-buffer buffer-A
      (insert (elt result 1)))

    (with-current-buffer buffer-B
      (insert (elt result 2)))

    (ediff-buffers buffer-A buffer-B)))


(provide 'ert-helpers)
;;; ert-helpers.el ends here
