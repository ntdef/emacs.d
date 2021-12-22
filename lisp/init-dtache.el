;;; init-dtache.el --- Config for dtache.el       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.local/src/dtache/")

(if (not (executable-find "dtach"))
    (warn "dtach executable not found; skipping init-dtache.el")
  (require 'dtache)
  (require 'marginalia-dtache)
  (require 'embark-dtache)

  (setq dtache-db-directory user-emacs-directory)
  (setq dtache-session-directory (expand-file-name "dtache" (temporary-file-directory)))
  (setq dtache-env "~/.local/scripts/dtache-env.sh")
  (add-to-list 'marginalia-annotator-registry
               '(dtache marginalia-dtache-annotate builtin none))
  (dtache-initialize))

(provide 'init-dtache)
;;; init-dtache.el ends here
