(load-file "~/.emacs.d/emacs_mode/dired-plus/dired+.el" )
(define-key ctl-x-map   "d" 'diredp-dired-for-files)
(define-key ctl-x-4-map "d" 'diredp-dired-for-files-other-window)
(toggle-diredp-find-file-reuse-dir 1)

(defun diredp-hook-fun ( )
  (local-set-key (kbd "`") 'diredp-up-directory)
  )

(add-hook 'diredp-mode-hook
	  (lambda ()
	    (dired-hook-fun)
	    )
	  )
