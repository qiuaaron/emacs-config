;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-24 23:46:21 Tuesday by ahei>

(require 'multi-term)

(when use-cua
  (cua-selection-mode 1))

(defun term-mode-settings ()
  "Settings for `term-mode'"
  ;; emacs gui版本如果不把scroll-margin设为0
  ;; 当光标最屏幕底部时，有可能使得屏幕发生抖动
  (make-local-variable 'scroll-margin)
  (setq-default scroll-margin 0))

(add-hook 'term-mode-hook 'term-mode-settings)

(define-key global-map (kbd "C-x e") 'multi-term)

(defun term-send-kill-whole-line ()
  "Kill whole line in term mode."
  (interactive)
  (term-send-raw-string "\C-a")
  ;; 没有`sleep-for'有时候就不行, 不知道为什么
  (sleep-for 0 1)
  (kill-line)
  (term-send-raw-string "\C-k"))

(defun term-send-kill-line ()
  "Kill line in term mode."
  (interactive)
  (call-interactively 'kill-line)
  (term-send-raw-string "\C-k"))

(defun term-send-yank ()
  "Yank in term mode."
  (interactive)
  (yank)
  (term-send-raw-string (current-kill 0)))

(defun term-send-copy-line ()
  "Copy left line in term mode."
  (interactive)
  (term-send-home)
  (sleep-for 0 1)
  (term-send-copy-line-left))

(defun term-send-copy-line-left ()
  "Copy current left line in term mode."
  (interactive)
  (term-send-kill-line)
  (term-send-raw-string "\C-_"))

(defun term-send-backward-kill-semi-word ()
  "Backward kill semiword in term mode."
  (interactive)
  (term-send-raw-string "\e\C-h"))

(setq multi-term-switch-after-close nil)
(setq multi-term-program "/bin/bash")
(setq term-unbind-key-list '("C-x" "<ESC>" "<up>" "<down>" "C-j"))
(setq
 term-bind-key-alist
 '(("C-c"   . term-send-raw)
   ("C-p"   . term-send-raw)
   ("C-n"   . term-send-raw)
   ("C-s"   . isearch-forward)
   ("C-r"   . term-send-raw)
   ("C-m"   . term-send-raw)
   ("C-k"   . term-send-kill-whole-line)
   ("C-y"   . term-send-raw)
   ("C-_"   . term-send-raw)
   ("C-M-h" . term-send-backward-kill-semi-word)
   ("M-f"   . term-send-raw-meta)
   ("M-b"   . term-send-raw-meta)
   ("M-d"   . term-send-raw-meta)
   ("M-K"   . term-send-kill-line)
   ("M-p"   . previous-line)
   ("M-n"   . next-line)
   ("M-w"   . term-send-copy-line)
   ("M-W"   . term-send-copy-line-left)
   ("M-y"   . term-send-raw-meta)
   ("M-."   . term-send-raw-meta)
   ("M-0"   . term-send-raw-meta)
   ("M-1"   . term-send-raw-meta)
   ("M-2"   . term-send-raw-meta)
   ("M-3"   . term-send-raw-meta)
   ("M-4"   . term-send-raw-meta)
   ("M-5"   . term-send-raw-meta)
   ("M-6"   . term-send-raw-meta)
   ("M-7"   . term-send-raw-meta)
   ("M-8"   . term-send-raw-meta)
   ("M-9"   . term-send-raw-meta)))

(defun switch-multi-term-and-text ()
  "if current in `term-mode', switch to `text-mode', else switch to `term-mode'."
  (interactive)
  (if (equal major-mode 'term-mode)
      (text-mode)
    (enter-multi-term)))
(defun enter-multi-term ()
  "Enter in `term-mode'."
  (interactive)
  (term-mode)
  (term-char-mode))
(defun enter-text-mode ()
  "Enter in `text-mode'."
  (interactive)
  (text-mode))

(define-key global-map (kbd "M-H") 'enter-text-mode)
(define-key global-map (kbd "M-J") 'switch-multi-term-and-text)
(define-key global-map (kbd "M-L") 'enter-multi-term)

(define-key global-map (kbd "C-x n") 'multi-term-next)
(define-key global-map (kbd "C-x P") 'multi-term-prev)
