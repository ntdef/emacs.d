;;; init-embark.el --- Configuration for embark -*- lexical-binding: t -*-

;;; Commentary:


;;; Code:

(when (maybe-require-package 'embark)

  (global-set-key (kbd "C-;") 'embark-act)

  (defun ntdef/embark-ticket-target ()
    "Idenitfy ticket targets for embark"
    (when-let (bounds (bounds-of-thing-at-point 'symbol))
      (let ((name (buffer-substring (car bounds) (cdr bounds))))
        (when (string-match-p "MS-[[:digit:]]+" name)
          `(ticket ,name . ,bounds)))))

  (defun ntdef/browse-ticket (tix)
    "Browse given ticket in JIRA"
    (browse-url (format "https://anchorfm.atlassian.net/browse/%s" tix)))

  (defun ntdef/view-ticket (tix)
    "Pull up information about ticket"
    (with-current-buffer-window (get-buffer-create (concat "JIRA:" tix)) nil nil
      ;; TODO Make this call-process async
      (call-process "jira" nil t t "view" "-t" "json" tix)
      (if (and (featurep 'json) (fboundp 'json-mode))
          (progn
            (json-mode)
            (read-only-mode))
        (special-mode))))

  ;; TODO Check prot usage of embark-act (using which-key maybe?)
  (with-eval-after-load 'embark
    (define-key embark-region-map (kbd "SPC") #'scratch)
    (embark-define-keymap embark-ticket-actions
      "Keymap for Jira tickets"
      ("o"   ntdef/browse-ticket)
      ("RET" ntdef/view-ticket)
      ("v"   ntdef/view-ticket))
    (add-to-list 'embark-keymap-alist '(ticket . embark-ticket-actions))
    (add-to-list 'embark-target-finders #'ntdef/embark-ticket-target))

  ;; FIXME There's probably a better way to initialize this
  (when (maybe-require-package 'embark-consult)
    (with-eval-after-load 'embark
      (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-c C-o") 'embark-export)
    (define-key vertico-map (kbd "C-c C-c") 'embark-act)))

(provide 'init-embark)

;;; init-embark.el ends here
