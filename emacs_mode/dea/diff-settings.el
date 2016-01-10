;; -*- Emacs-Lisp -*-

;; Time-stamp: <10/27/2009 13:47:03 星期二 by ahei>

(require 'diff-mode)

(define-key diff-mode-map (kbd "C-k") 'diff-hunk-kill)
(define-key diff-mode-map (kbd "SPC") 'scroll-up)
(define-key diff-mode-map (kbd "u") 'View-scroll-half-page-backward)

(define-key diff-mode-shared-map (kbd "k") 'previous-line)
(define-key diff-mode-shared-map (kbd "K") 'roll-up)
