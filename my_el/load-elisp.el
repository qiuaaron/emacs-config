;(add-to-list 'global-company-modes 'elisp-mode)
(add-hook 'Emacs-Lisp-mode 
            (lambda ()
              (set (make-local-variable 'company-backends) 
		   '(company-yasnippet  
		     company-elisp
		     company-bbdb
		      company-cmake
		      company-capf
		      (company-dabbrev-code company-gtags company-etags
					    company-keywords)
		      company-files company-dabbrev) )))
