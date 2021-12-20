;;; init-snippets.el --- Configuration for snippets -*- lexical-binding: t -*-

;;; Commentary:

;; Description of useful functions:
;;
;;     yas-describe-tables :: list all snippets with expansion
;;
;; TODO Check if there's a consult-yasnippet available
;; NOTE `header' is the is the keyword for Emacs Lisp modules
;;

;;; Code:
(when (maybe-require-package 'yasnippet)

  ;; Install default yasnippet snippets
  (maybe-require-package 'yasnippet-snippets)
  )

(provide 'init-snippets)

;;; init-snippets.el ends here
