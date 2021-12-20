;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'markdown-mode)
  (setq markdown-hide-urls t)
  (add-auto-mode 'markdown-mode "\\.md\\.html\\'")
  (with-eval-after-load 'whitespace-cleanup-mode
    (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)))

(maybe-require-package 'pandoc-mode)


(provide 'init-markdown)
;;; init-markdown.el ends here
