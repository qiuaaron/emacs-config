;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-05 16:14:19 星期四 by ahei>

(setq use-cua t)

(when is-after-emacs-23
  (setq cua-remap-control-z nil)
  (setq cua-remap-control-v nil))

(autoload 'cua--init-rectangles "cua-rect")
(eval-after-load "cua-base"
  '(progn
     (unless is-after-emacs-23
       (define-key cua--cua-keys-keymap [(control z)] nil)
       (define-key cua--cua-keys-keymap [(control v)] nil)
       (define-key cua--cua-keys-keymap [(meta v)] nil))
     
     (cua--init-rectangles)
     (define-key cua--rectangle-keymap (kbd "M-f") 'forward-word-remember)
     (define-key cua--rectangle-keymap (kbd "M-b") 'backward-word-remember)))
