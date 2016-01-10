(add-to-list 'load-path "~/.emacs.d/emacs_mode/avy/")
(require 'avy)


(defvar avy-one-key-menu-alist nil)
(setq avy-one-key-menu-alist 
      '(
	( ( "C-." . "goto-char") . avy-goto-char)
	( ( "l" . "goto-line") . avy-goto-line)
	( ( "C-l" . "emacs-goto-line") . goto-line)
	( ( "w" . "goto-word") . avy-goto-word-1)
	( ( "k" . "goto-char-2") . avy-goto-char-2)
	( ( "b" . "create tmp bookmakr") . Aaron-set-tmp-bookmark )
	( ( "B" . "jump to tmp bookmark") . Aaron-jump-to-tmp-bookmark)
	
	
	;; ( ( "g" . "go to gdb window") . Aaron-erilog-remove-module-signal )
	;; ( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
      )
(defun avy-one-key-menu()
  (interactive)
  (one-key-menu "avy" avy-one-key-menu-alist t )
  
)   


(define-key global-map (kbd "C-.") 'avy-one-key-menu)



