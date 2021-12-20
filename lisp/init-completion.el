;;; init-completion.el --- Completion configuration -*- lexical-binding: t -*-

;;; Commentary:

;; See https://github.com/minad/corfu
;;; TODO Setup https://github.com/jdtsmith/kind-icon for completion icons

;;; Code:

(when (maybe-require-package 'corfu)
  (when (featurep 'company)
    (company-mode nil))

  (setq corfu-auto t
        corfu-cycle t
        corfu-commit-predicate nil
        corfu-quit-at-boundary t
        corfu-quit-no-match t
        corfu-preview-current t)
  (corfu-global-mode))

(setq-local completion-at-point-functions
            (list (cape-super-capf #'cape-dabbrev #'cape-dict #'cape-keyword)))

(when (maybe-require-package 'cape)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(maybe-require-package 'eglot)

(provide 'init-completion)

;;; init-completion.el ends here
