

;;define a one-key-menu for jump 
(defvar jump-one-key-menu-alist nil)
(setq verilog-one-key-menu-alist 
      '(
	( ( "i" . "instantiated module") . Aaron-verilog-inst-module)
	( ( "d" . "remove-module-signal") . Aaron-verilog-remove-module-signal )
	( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
      )
(defun Verilog-one-key-menu()
  (interactive)
  (one-key-menu "verilog" verilog-one-key-menu-alist t )
)   
