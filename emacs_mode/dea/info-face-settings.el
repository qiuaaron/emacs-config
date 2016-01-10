;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-09 13:11:35 星期一 by ahei>

(require 'info)

(set-face-foreground 'info-menu-header "blue")
(unless is-before-emacs-21
  (set-face-foreground 'info-menu-star "red")
  (set-face-background 'info-menu-star "yellow"))
(custom-set-faces '(info-header-node ((((class color) (background dark)) (:foreground "red")))))
(custom-set-faces '(info-title-1
                    ((((type tty pc) (class color) (background dark))
                      :foreground "yellow" :weight bold)
                     (t :foreground "yellow"))))
(custom-set-faces '(info-title-2
                    ((((type tty pc) (class color) (background dark))
                      :foreground "yellow" :weight bold)
                     (t :foreground "lightblue"))))
(custom-set-faces '(info-title-3
                    ((((type tty pc) (class color) (background dark))
                      :foreground "yellow" :weight bold)
                     (t :foreground "violetred1"))))
(custom-set-faces '(info-title-4
                    ((((type tty pc) (class color) (background dark))
                      :foreground "yellow" :weight bold)
                     (t :foreground "green"))))
(custom-set-faces '(info-menu-header
                    ((((type tty pc)) :underline t :weight bold)
                     (t :inherit nil :foreground "coral2" :bold nil))))
