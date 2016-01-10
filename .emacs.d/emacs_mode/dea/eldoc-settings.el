;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-16 18:06:49 Monday by ahei>

(require 'eldoc)

(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook))
  (add-hook hook 'turn-on-eldoc-mode))

(setq eldoc-idle-delay 0.2)
(custom-set-faces '(eldoc-highlight-function-argument
                    ((((type tty)) :bold t :foreground "green")
                     (t :bold nil :foreground "green"))))
(eldoc-add-command 'describe-symbol-at-point 'View-scroll-half-page-backward 'l-command
                   'save-buffer-sb 'switch-to-other-buffer)
(eldoc-remove-command 'goto-paren)
