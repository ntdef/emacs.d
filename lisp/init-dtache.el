;;; init-dtache.el --- Config for dtache.el       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dtache
  :demand
  :ensure nil
  :load-path "~/.local/src/dtache/"
  :ensure-system-package dtache
  :config
  (setq dtache-db-directory user-emacs-directory)
  (setq dtache-session-directory (expand-file-name "dtache" (temporary-file-directory)))
  (setq dtache-env "~/.local/scripts/dtache-env.sh")
  (dtache-initialize))

(use-package marginalia-dtache
  :after (marginalia dtache)
  :config
  (add-to-list 'marginalia-annotator-registry
               '(dtache marginalia-dtache-annotate builtin none)))

(use-package embark-dtache
  :after (embark dtache))

(provide 'init-dtache)
;;; init-dtache.el ends here
