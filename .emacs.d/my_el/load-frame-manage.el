
(winner-mode 1)
(defvar easy-one-key-menu-alist nil)
(setq easy-one-key-menu-alist 
      '(
	( ( "n" . "find project file") . make-frame-command)
	( ( "k" . "delete frame") . delete-frame)
	( ( "c" . "copy buffers between frames") . Aaron-copy-buffers-between-frames)
	( ( "s" . "switch buffers between frames") . switch-buffers-between-frames)
	( ( "m" . "switch buffers and make them same") . Aaron-two-frame-switch-make-same)
	( ( "M" . "switch to matlab shell window ")    . (lambda ()(interactive) (switch-to-buffer "*MATLAB*")  ) )
	( ( "w" . "undo window split") . winner-undo)
	( ( "W" . "redo window split") . winner-redo)
	)
      )
(defun easy-one-key-menu()
  (interactive)
  (one-key-menu "easy" easy-one-key-menu-alist  t )
)   

(global-set-key ( kbd  "C-\\") 'easy-one-key-menu )

