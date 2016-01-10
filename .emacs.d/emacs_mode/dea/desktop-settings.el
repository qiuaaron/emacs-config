;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-03 10:37:02 星期二 by ahei>

;; HACK: 要放在最后,免得会出现比较奇怪的现象
;; 保存和恢复工作环境
;; desktop,用来保存Emacs的桌面环境 — buffers、以及buffer的文件名、major modes和位置等等

(require 'desktop)

(setq desktop-load-locked-desktop t)
(define-key global-map (kbd "C-x M-C") 'desktop-clear)
(if is-before-emacs-21 (desktop-load-default) (desktop-save-mode 1))
(desktop-read)
(dolist (var (list 'command-history 'kill-ring 'file-name-history 'find-symbol-last-symbol
                   'extended-command-history 'grep-history 'compile-history 'last-template
                   'minibuffer-history 'query-replace-history 'regexp-history
                   'shell-command-history 'recentf-open-last-file 'describe-symbol-last-symbol
                   'switch-major-mode-last-mode 'sb-keep-buffer))
  (add-to-list 'desktop-globals-to-save var))
