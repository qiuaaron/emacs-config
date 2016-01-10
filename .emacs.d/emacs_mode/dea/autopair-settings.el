;; -*- Emacs-Lisp -*-

;; Time-stamp: <2009-11-04 00:57:49 ÐÇÆÚÈý by ahei>

(require 'autopair)

(autopair-global-mode)

(defun autopair-insert-opening-internal ()
  (interactive)
  (when (autopair-pair-p)
    (setq autopair-action (list 'opening (autopair-find-pair) (point))))
  (autopair-fallback))

(defun autopair-insert-opening ()
  (interactive)
  (if (and (memq major-mode '(c-mode c++-mode java-mode)) (equal last-command-event ?{))
      (call-interactively 'skeleton-c-mode-left-brace)
    (call-interactively 'autopair-insert-opening-internal)))
