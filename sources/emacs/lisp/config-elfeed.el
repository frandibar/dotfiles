;;; config-elfeed.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :custom
  (elfeed-feeds '(
		  ("https://planet.emacslife.com/atom.xml" emacs)
		  ("https://news.ycombinator.com/rss" news)
		  ("http://www.marketingyestrategia.com/feed/")
		  ;; ("https://www.infobae.com/feeds/rss/" noticias)
		  )))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
