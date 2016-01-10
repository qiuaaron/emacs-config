;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-13 16:13:32 Friday by ahei>

(require 'info)

(defvar Info-mode-map-key-pairs
  `(("j"     next-line)
    ("k"     previous-line)
    ("h"     backward-char)
    ("l"     forward-char)
    ("U"     Info-up)
    ("u"     View-scroll-half-page-backward)
    ("Q"     kill-this-buffer)
    ("o"     other-window)
    ("SPC"   scroll-up)
    ("C-h"   Info-up)
    ("N"     Info-next-reference)
    ("P"     Info-prev-reference)
    ("'"     switch-to-other-buffer)
    ("C-c ," Info-history-back)
    ("C-c ." Info-history-forward))
  "*Keys map for `Info-mode'.")

(apply-map-define-keys 'Info-mode-map)

(defun info-max ()
  "After call `info', select \"*info*\" buffer, and maximize it."
  (interactive)
  (call-interactively 'info)
  (switch-to-buffer "*info*")
  (delete-other-windows))

(define-kbd global-map "C-x I" 'info-max)
