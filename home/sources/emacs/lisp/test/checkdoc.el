;;; checkdoc.el --- -*- lexical-binding: t -*-
;;; Commentary:

;; Run this module in batch mode in order to check that all elisp
;; files follow established conventions.

;;; Code:

(require 'cl-lib)

(mapc #'checkdoc-file
      (cl-reduce #'append
		 (mapcar #'(lambda (dir) (directory-files dir t ".el$"))
			 '(".."))))

(provide 'checkdoc)
;;; checkdoc.el ends here
