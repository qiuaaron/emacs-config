(require 'tcl)
(add-hook 'tcl-mode-hook 
( lambda ( )
  (set (make-local-variable 'company-backends) 
       '(company-yasnippet  
	 company-bbdb
	 company-capf
	 (company-dabbrev-code company-gtags company-etags
			       company-keywords)
	 company-files company-dabbrev) )
  )
)


(setq tcl-help-directory-list '("/eda/Xilinx/v/doc/eng/"))
(defun Aaron-tcl-help-on-word-at-point ()
  ""
  (interactive)
  (tcl-help-on-word (tcl-current-word))
  )

(defvar tcl-jump-one-key-menu-alist nil)
(setq tcl-jump-one-key-menu-alist 
					;ace-jump-command-list
      avy-one-key-menu-alist
      )

(defun tcl-jump-one-key-menu()
  (interactive)
  (one-key-menu "tcl" tcl-jump-one-key-menu-alist t )
)   

(defvar tcl-func-one-key-menu-alist nil)
(setq tcl-func-one-key-menu-alist 
      '( ( ("h" . "show help on word ") . (lambda () (interactive)(tcl-help-on-word (tcl-current-word)) )   )
	 ( ("C-h" . "show help for command ( user input )") . tcl-help-on-word )
	)
      )

(defun tcl-jump-one-key-menu()
  (interactive)
  (one-key-menu "tcl" tcl-jump-one-key-menu-alist t )
)   

(add-hook 'tcl-mode-hook
            (lambda ()
	      (local-set-key (kbd "C-.") 'tcl-jump-one-key-menu)              
	      (local-set-key (kbd "C-9") '(lambda ()(interactive) ( one-key-menu "tcl func" tcl-func-one-key-menu-alist t)) ) 
	      ))


;(setq help-xref-following t)

(require 'xtcl-shell-mode)

;; I can't determine where the verilog-error-regexp-add called and defined
;; but this funtion void make the xtcl-shell-mode-hook failed , So When 
;; there is not the verilog-error-regexp-add defined , I define one
(if (fboundp 'verilog-error-regexp-add) 
    t
  (defun verilog-error-regexp-add() (+ 1 1 ) ) 
  )

(add-hook 'xtcl-shell-mode-hook ( lambda ( )
				  (set (make-local-variable 'company-backends) 
				       '(company-vivado-tcl
					 company-bbdb
					 company-capf
					 (company-dabbrev-code company-gtags company-etags
							       company-keywords)
					 company-files company-dabbrev) )
				  )
	  )
