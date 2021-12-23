;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;; Navigate window layouts with "C-c <left>" and "C-c <right>"

(add-hook 'after-init-hook 'winner-mode)


;; Make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)



;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)


;; Rearrange split windows

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs

(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)




(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)




(unless (memq window-system '(nt w32))
  (require-package 'windswap)
  (add-hook 'after-init-hook (apply-partially 'windmove-default-keybindings 'control))
  (add-hook 'after-init-hook (apply-partially 'windswap-default-keybindings 'shift 'control)))


;;; Setup `display-buffer-alist'
;; copied from https://protesilaos.com/emacs/dotemacs
(setq display-buffer-alist
      `(;; no window
        ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
        ;; top side window
        ("\\*\\(Flymake diagnostics\\|Package-Lint\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 0))
        ("\\*Messages.*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 1))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.16)
         (side . top)
         (slot . 2))
        ;; left side window
        ("\\*\\(.* # Help.*\\|Help\\)\\*" ; See the hooks for `visual-line-mode'
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         (window-width . 0.25)
         (side . left)
         (slot . 0))
        ;; right side window
        ("\\*keycast\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (window-width . 0.25)
         (side . right)
         (slot . -1)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ;; bottom side window
        ("\\*Org Select\\*"
         (display-buffer-in-side-window)
         (dedicated . t)
         (side . bottom)
         (slot . 0)
         (window-parameters . ((mode-line-format . none))))
        ;; bottom buffer (NOT side window)
        ("\\*Embark Actions\\*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-height . fit-window-to-buffer)
         (window-parameters . ((no-other-window . t)
                               (mode-line-format . none))))
        ("\\*\\(Embark\\)?.*Completions.*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom)
         (window-parameters . ((no-other-window . t))))
        ("\\*\\(Output\\|Register Preview\\).*"
         (display-buffer-reuse-mode-window display-buffer-at-bottom))
        ;; below current window
        ("\\*.*\\(e?shell\\|v?term\\).*"
         (display-buffer-in-side-window)
         (dedicatd . t)
         (window-height 0.25)
         (side . bottom)
         (slot . 0))
        ("\\*\\vc-\\(incoming\\|outgoing\\|git : \\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         ;; NOTE 2021-10-06: we cannot `fit-window-to-buffer' because
         ;; the height is not known in advance.
         (window-height . 0.2))
        ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . fit-window-to-buffer))))

(setq window-combination-resize t)
(setq even-window-sizes 'height-only)
(setq window-sides-vertical nil)
(setq switch-to-buffer-in-dedicated-window 'pop)

(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'custom-mode-hook #'visual-line-mode)

(provide 'init-windows)
;;; init-windows.el ends here
