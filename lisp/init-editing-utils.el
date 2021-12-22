;;; init-editing-utils.el --- Day-to-day editing helpers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

(maybe-require-package 'list-unicode-display)


;;; Some basic preferences

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 create-lockfiles nil
 auto-save-default nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'delete-selection-mode)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)



;; Huge files

(when (fboundp 'so-long-enable)
  (add-hook 'after-init-hook 'so-long-enable))

(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; A simple visible bell which works in all terminal types
(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)



(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  (add-hook 'after-init-hook 'beacon-mode))



;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)



(with-eval-after-load 'subword
  (diminish 'subword-mode))



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))



(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?\u254e)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))



(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (with-eval-after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))


;;; Zap *up* to char is a handy pair for zap-to-char
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(with-eval-after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(with-eval-after-load 'page-break-lines
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;; Show matching parens
(add-hook 'after-init-hook 'show-paren-mode)


;;; Narrowing
(defun ntdef/narrow-dwim ()
  "Widen region if buffer is narrowed, otherwise call `narrow-to-region'."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (call-interactively #'narrow-to-region)))

(global-set-key (kbd "C-c n") #'ntdef/narrow-dwim)

;;; Handy key bindings

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)



;;; Page break lines

(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (add-hook 'help-mode-hook 'page-break-lines-mode)
  (with-eval-after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))



;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.

(require-package 'move-dup)
(global-set-key [M-up] 'move-dup-move-lines-up)
(global-set-key [M-down] 'move-dup-move-lines-down)
(global-set-key [M-S-up] 'move-dup-move-lines-up)
(global-set-key [M-S-down] 'move-dup-move-lines-down)

(global-set-key (kbd "C-c d") 'move-dup-duplicate-down)
(global-set-key (kbd "C-c u") 'move-dup-duplicate-up)


;;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL

(defun sanityinc/backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'sanityinc/backward-up-sexp) ; C-M-u, C-M-up



;;; Cut/copy the current line if no region is active
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)
(with-eval-after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))



(defun sanityinc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)



;; M-^ is inconvenient, so also bind M-j
(global-set-key (kbd "M-j") 'join-line)


;; upcase, downcase & capitalize configuration

(defun ntdef/upcase-dwim ()
  "Wrapper for upcase dwim"
  (interactive)
  (unless current-prefix-arg
    (setq current-prefix-arg -1))
  (call-interactively #'upcase-dwim))

(defun ntdef/downcase-dwim ()
  "Wrapper for downcase dwim"
  (interactive)
  (unless current-prefix-arg
    (setq current-prefix-arg -1))
  (call-interactively #'downcase-dwim))

(defun ntdef/capitalize-dwim ()
  "Wrapper for capitalize dwim"
  (interactive)
  (unless current-prefix-arg
    (setq current-prefix-arg -1))
  (call-interactively #'capitalize-dwim))

(global-set-key [remap upcase-word     ] #'ntdef/upcase-dwim)
(global-set-key [remap downcase-word   ] #'ntdef/downcase-dwim)
(global-set-key [remap capitalize-word ] #'ntdef/capitalize-dwim)


;; Random line sorting
(defun sanityinc/sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))



(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)


(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)
(setq-default which-key-idle-delay 1.5)
(with-eval-after-load 'which-key
  (diminish 'which-key-mode))


(defun sanityinc/disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'sanityinc/disable-features-during-macro-call)


;; REPEAT-MODE configuration
(repeat-mode 1)

(global-set-key (kbd "M-]") #'forward-paragraph)
(global-set-key (kbd "M-[") #'backward-paragraph)

(with-eval-after-load 'repeat
  (defvar repeat-movement-map
    (define-keymap
      (kbd "M-f") #'forward-word
      (kbd "f")   #'forward-word
      (kbd "M-b") #'backward-word
      (kbd "b")   #'backward-word
      (kbd "M-]") #'forward-paragraph
      (kbd "]")   #'forward-paragraph
      (kbd "M-[") #'backward-paragraph
      (kbd "[")   #'backward-paragraph
      (kbd "n")   #'next-line
      (kbd "C-n") #'next-line
      (kbd "p")   #'previous-line
      (kbd "C-p") #'previous-line)
    "My keymap.")
  (dolist (cmd '(forward-word
                 backward-word
                 next-line
                 previous-line
                 backward-paragraph
                 forward-paragraph
                 ))
    (put cmd 'repeat-map 'repeat-movement-map))

  (defvar repeat-sexp-movement-map
    (define-keymap :parent repeat-movement-map
      (kbd "C-M-f") #'forward-sexp
      (kbd "f")     #'forward-sexp
      (kbd "C-M-b") #'backward-sexp
      (kbd "b")     #'backward-sexp)
    "A repeat sexp movement map")

  (dolist (cmd '(forward-sexp backward-sexp))
    (put cmd 'repeat-map 'repeat-sexp-movement-map))

  (defvar repeat-paredit-map
    (define-keymap
      (kbd "C-M-u") #'paredit-backward-up
      (kbd "u")     #'paredit-backward-up
      (kbd "C-M-d") #'paredit-forward-down
      (kbd "d")     #'paredit-forward-down
      (kbd "C-M-n") #'paredit-forward-up
      (kbd "n")     #'paredit-forward-up
      (kbd "C-M-f") #'paredit-forward
      (kbd "f")     #'paredit-forward
      (kbd "C-M-b") #'paredit-backward
      (kbd "b")     #'paredit-backward)
    "A repeat keymap for paredit")

  (dolist (cmd '(paredit-forward paredit-backward paredit-forward-up paredit-backward-down paredit-forward-down paredit-backward-up))
    (put cmd 'repeat-map 'repeat-paredit-map))

  (defun ntdef/repeat-map-ok (km)
    (cl-labels ((frec (evt defn)
                  (when (keymapp defn)
                    (map-keymap #'frec defn))
                  (when (functionp defn)
                    (put defn 'repeat-map km))))
      #'frec))
  (map-keymap (ntdef/repeat-map-ok 'repeat-paredit-map) repeat-paredit-map)

  )

(defvar default-text-scale-repeat-map
  (define-keymap
    (kbd "C-M-=") #'default-text-scale-increase
    (kbd "=")     #'default-text-scale-increase
    (kbd "C-M--") #'default-text-scale-decrease
    (kbd "-")     #'default-text-scale-decrease)
  "Keymap for text scale repeat.")

(dolist (cmd '(default-text-scale-increase default-text-scale-decrease))
  (put cmd 'repeat-map 'default-text-scale-repeat-map))


;; Yanking
;; See https://karthinks.com/software/a-better-yank-pop-binding/

(global-set-key [remap yank-pop] #'consult-yank-pop)


;; Commands for manipulating shell args
;;
;; see https://xenodium.com/emacs-quote-wrap-all-in-region/
;; FIXME Note that there's probably a better way to do this with insert-pair
;; TODO See https://emacs.stackexchange.com/questions/45798/split-command-line-into-a-list-of-arguments
;; TODO See also https://emacs.stackexchange.com/questions/32077/how-to-programmatically-surround-a-string-with-escaped-double-quotes
(defun ntdef/toggle-quote-wrap-all-in-region (beg end)
  "Toggle wrapping all items in region with double quotes."
  (interactive (list (mark) (point)))
  (unless (region-active-p)
    (user-error "no region to wrap"))
  (let ((deactivate-mark nil)
        (replacement (string-join
                      (mapcar (lambda (item)
                                (if (string-match-p "^\".*\"$" item)
                                    (string-trim item "\"" "\"")
                                  (format "\"%s\"" item)))
                              (split-string (buffer-substring beg end)))
                      " ")))
    (delete-region beg end)
    (insert replacement)))

(defun ntdef/--wrap-each (arg)          ; FIXME
  (interactive "p")
  ;; (re-search-forward "[^ \t\n]+")
  ;; (replace-match "\"\\&\"")
  (while (and (> arg 0) (re-search-forward "[^ \t\n]+"))
    (replace-match "\"\\&\"")
    (setq arg (1- arg))))

(defun ntdef/--wrap-quote (beg end)     ; FIXME
  (interactive "r")
  (replace-region-contents beg end
                           (lambda ()
                             (when-let (arr (split-string-shell-command))
                               (delete-region (point-min) (point-max))
                               (dolist (el arr)
                                 (insert-pair)
                                 (insert arr)
                                 ))
                             (seq-mapcat #'shell-quote-argument
                                         (split-string-and-unquote (buffer-string))
                                         'string))))

(defun ntdef/--wrap-quote (beg end)     ; FIXME
  (interactive "r")
  (replace-region-contents beg end
                           (lambda ()
                             (combine-and-quote-strings))))
;; "one" "two" "three"

;; (combine-and-quote-strings (mapcar #'prin1-to-string (split-string-and-unquote "\"--one\" \"--two\" \"--three\"")))

;; (combine-and-quote-strings '("one four" "two" "--three" "\"first\""))


;; Scratch buffers

(global-set-key (kbd "C-c s") #'scratch)


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
