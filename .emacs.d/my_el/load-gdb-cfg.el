
(defvar gdb-one-key-menu-alist nil)
(setq gdb-one-key-menu-alist 
      '(
	( ( "b" . "add breakpoing") . gud-break)
	( ( "g" . "display gdb buffer") . gdb-display-gdb-buffer)
	( ( "B" . "remove breakpoing") . gud-remove)
	)
      )
(defun gdb-one-key-menu()
  (interactive)
  (one-key-menu "gdb" gdb-one-key-menu-alist  t )
)   

(global-set-key (kbd "<f1>") 'gdb-one-key-menu)


