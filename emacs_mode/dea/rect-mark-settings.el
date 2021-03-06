;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-03 18:42:15 星期二 by ahei>

(require 'rect-mark)

(defun rm-mark-command ()
  "如果是CUA mode, 则执行`cua-set-rectangle-mark', 否则执行`rm-set-mark'"
  (interactive)
  (setq last-region-beg (point))
  (setq last-region-is-rect t)
  (setq last-region-use-cua cua-mode)
  (if cua-mode
      (call-interactively 'cua-set-rectangle-mark)
    (call-interactively 'rm-set-mark)))
(global-set-key (kbd "C-x \\") 'rm-mark-command)
(global-set-key (kbd "M-w") 'copy-region)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(defun copy-region (beg end)
  "根据`mark-active'和`rm-mark-active'来决定是执行`copy-region-as-kill-nomark'还是`rm-kill-ring-save'"
  (interactive "r")
  (if cua-mode
      (if cua--rectangle
          (progn
            (cua-copy-rectangle t)
            (cua-cancel))
        (call-interactively 'cua-copy-region))
    (if rm-mark-active (call-interactively 'rm-kill-ring-save) (copy-region-as-kill-nomark beg end))))
