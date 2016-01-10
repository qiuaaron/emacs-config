;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto load the vhdl mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/.emacs.d/emacs_mode/vhdl-mode/" load-path))
(autoload 'vhdl-mode "vhdl-mode" "VHDL Mode" t) 
(setq auto-mode-alist (cons '("\\.vhdl?\\'" . vhdl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.vhd\\'" . vhdl-mode) auto-mode-alist))

(add-hook 'vhdl-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) 
		   '(( company-yasnippet  
		     company-bbdb
		      company-cmake
		      company-capf
		      company-dabbrev-code company-gtags company-etags company-keywords
		      company-files company-dabbrev)) )))
