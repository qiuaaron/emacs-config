;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-17 10:58:46 Tuesday by ahei>

(require 'ediff)

(global-set-key (kbd "C-x D") 'ediff)
(add-hook 'ediff-mode-hook
          (lambda ()
            (setq ediff-highlight-all-diffs nil
                  ediff-highlighting-style 'face)))
(add-hook 'ediff-keymap-setup-hook 'ediff-my-keys)

(defun ediff-my-keys ()
  (interactive)
  "`ediff-mode'的按键设置"
  (define-key ediff-mode-map (kbd "u") 'ediff-update-diffs)
  (define-key ediff-mode-map (kbd "/") 'ediff-toggle-help)
  (define-key ediff-mode-map (kbd "c") 'ediff-inferior-compare-regions)
  (define-key ediff-mode-map (kbd "f") 'ediff-jump-to-difference)
  (define-key ediff-mode-map (kbd "j") '
    (lambda ()
      (interactive)
      (setq last-command-char ?v)
      (call-interactively 'ediff-scroll-vertically)))
  (define-key ediff-mode-map (kbd "k") 'ediff-scroll-vertically)
  (define-prefix-command 'R-map)
  (define-key ediff-mode-map (kbd "R") 'R-map)
  (define-key ediff-mode-map (kbd "R a") 'ediff-toggle-read-only)
  (define-key ediff-mode-map (kbd "R b") 'ediff-toggle-read-only)
  (define-key ediff-mode-map (kbd "A") '(lambda () (interactive) (other-window 1)))
  (define-key ediff-mode-map (kbd "B") '(lambda () (interactive) (other-window 2))))
(add-hook 'ediff-startup-hook '(lambda () (ediff-next-difference)))
(add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
;; 用ediff比较的时候在同一个frame中打开所有窗口
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
