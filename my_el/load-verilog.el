;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto load the verilog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq load-path (cons "~/.emacs.d/emacs_mode/verilog-mode/" load-path))
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (cons  '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.dv\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.sv\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.vh\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons  '("\\.svh\\'" . verilog-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config the company backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Its value is (company-elisp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
;; 	       (company-dabbrev-code company-gtags company-etags company-keywords)
;; 	       company-oddmuse company-files company-dabbrev)

;(add-to-list 'company-global-modes 'verilog-mode)
(add-hook 'verilog-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) 
		   '(( company-yasnippet  
		     company-bbdb
		      company-cmake
		      company-capf
		      company-dabbrev-code company-gtags company-etags company-keywords
		      company-files company-dabbrev)) )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun my-compile-verilog ( &optional cmd )
  (interactive)
"define a function to compile the verilog project"
(progn
  (set-buffer "Makefile"))
(compile "make -k" )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set the compile-mode of novasfor the verilog 
(defun my-compile-verilog-novas ( &optional cmd )
  (interactive)
"define a function to compile the verilog project"
(progn
  (set-buffer "Makefile"))
(compile "make -k cnovas" )
)



(defun my-compile-verilog-gentag ( &optional cmd )
  (interactive)
"define a function to compile the verilog project"
(progn
  (set-buffer "Makefile"))
(compile "make -k gentags" )
)

(setq verilog-linter (concat "nLint -rdb /dev/null -out screen -sv -rs ~/nLint.rs +define+LINT" (buffer-file-name) ) ) 

(defvar verilog-one-key-menu-alist nil)
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

(defvar Verilog-occur-one-key-menu-alist nil)
(setq Verilog-occur-one-key-menu-alist 
      `(
	( ( "c" . "occur-class ") . Aaron-verilog-class-occur)	    
	( ( "i" . "occur-inst" ) . Aaron-verilog-module-inst)	    
	( ( "f" . "occur-function") . Aaron-verilog-function-occur)
	( ( "d" . "occur-declare" ) . Aaron-verilog-declaration-occur)
	( ( "p" . "occur-port")  . Aaron-verilog-port-occur )
	( ("C-'" . "occur-cw") . Aaron-verilog-cw-occur)
	( ("e"   . "occur-entity-in-file") . Aaron-verilog-entity-in-file-occur)
	)
      )

(defun Verilog-occur-one-key-menu()
  (interactive)
  (one-key-menu "verilog-occur" Verilog-occur-one-key-menu-alist t )
)   
(defun my-lint-verilog ( &optional cmd )
  (interactive)
"define a function to compile the verilog project"
(progn
  (set-buffer "Makefile"))
(compile "make -k nlint" )
)

(defun my-compile-verilog-hallint ( &optional cmd )
  (interactive)
"define a function to compile the verilog project"
(progn
  ( set-buffer "Makefile")
  ;( get-buffer-create "" )
  (find-file "hal.comlog" )
  ( call-process-shell-command "make -k hallint" nil "hal.comlog" )
  ( switch-to-buffer "*hallint*")
  )
)

(add-hook 'verilog-mode-hook 
	  ( lambda ( )
	    (require 'imenu)
	    (setq imenu-generic-expression
		  '((nil "^\\s-*\\(\\(m\\(odule\\|acromodule\\)\\)\\|primitive\\)\\s-+\\([a-zA-Z0-9_.:]+\\)" 4)
		    ("*Vars*" "^\\s-*\\(reg\\|wire\\|bit\\|integer\\|real\\|parameter\\)\\s-+\\(\\|\\[[^]]+\\]\\s-+\\)\\([A-Za-z0-9_]+\\)" 3)
		    )
		  )
	    )
)

(add-hook 'verilog-mode-hook 
	  ( lambda ( )
	    ;;
	    ;(auto-complete-mode)
	    ;define some keychord
	    
	    (key-chord-define verilog-mode-map ",,"  "<=")
	    (key-chord-define verilog-mode-map ".."  "_")
	    (key-chord-define verilog-mode-map ";;"  ":")

	    (setq ac-sources '( ac-source-filename ac-source-words-in-same-mode-buffers ac-source-yasnippet ac-source-filename ac-source-files-in-current-dir))
	    (make-variable-buffer-local 'ac-sources)
	    ;define some vars for the package 
	    ;the package for compile
	    ( require 'compile)
	    ;; (defconst 'compilation-error-regexp-alist-alist
;; 	      '( ( irun "\\(*E|*W\\),.*(\\(.*\\),\\([0-9]+\\)|[0-9]+):"  2 3 ) ) )
;; 	    (add-to-list 'compilation-error-regexp-alist 'irun )
	    (setq verilog-typedef-regexp "_t$")
	   
	    ;define the key for 
	    ;(local-set-key (kbd "C-." ) 'next-error)
	    ;(local-set-key "\C-cvi" 'Aaron-verilog-inst-module)
	    ;(local-set-key (kbd "C-c v i") 'Aaron-verilog-inst-module)
	    ;(local-set-key "\C-cvd" 'Aaron-verilog-remove-module-signal)
	    ;(local-set-key "\C-cvc" 'Aaron-verilog-class-occur)
	    ;(local-set-key (kbd "C-c v d" ) 'Aaron-verilog-remove-module-signal)

					;(local-set-key (kbd "C-,")  'find-tag )
	    (local-set-key (kbd "C-,")  'Aaron-verilog-find-current-tag)
;; 	    (local-set-key (kbd "C-.")  'pop-tag-mark )
;; 	    (local-set-key "\C-cts" 'tags-search )
;; 	    (local-set-key "\C-ctr" 'tags-query-replace)
	    
;; 	    (local-set-key "\C-ctl" 'list-tags)
	    
	    ;(local-set-key (kbd "C-9") 'Aaron-verilog-cw-occur)
	    (local-set-key (kbd "C-8") 'Aaron-verilog-occur)
	    ;; (local-set-key (kbd "C-c o c" ) 'Aaron-verilog-class-occur)	    
	    ;; (local-set-key (kbd "C-c o i" ) 'Aaron-verilog-module-inst)	    
	    
	    (local-set-key (kbd "C-9") 'Verilog-one-key-menu)
	    ;; (local-set-key (kbd "C-'") 'Verilog-occur-one-key-menu)
					;the package hideshow
	    (hs-minor-mode)
	    (setq hs-special-modes-alist
		  (cons '(verilog-mode  "\\<begin\\>" "\\<end\\>" nil
					verilog-forward-sexp-function)
			hs-special-modes-alist))
					;(which-function-modes)
	    )
	  
	  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add error regexp for vcs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vcs-error-regexp-emacs-alist
  '(
    (vcs-error
					;"\\(Error\\|War\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 3)
     "\\(Error\\)-\\[\\(.+\\)\\] .*\n\\(.*\\), \\([0-9]+\\)" 3 4 2 )

    (vcs-syntax-error
					;"\\(Error\\|War\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 3)
     "\\(Error\\)-\\[\\(.+\\)\\] .*\n.*\n  \\(.*\\), \\([0-9]+\\)" 3 4 2 )
    (vcs-warning
					;"\\(Error\\|War\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 3)
     "\\(Warning\\)-\\[\\(.+\\)\\] .*\n\\(.*\\), \\([0-9]+\\)" 3 4 1 )
  )
    
  "List of regexps for synopsys vcs ")

(defun vcs-error-regexp-add-emacs ()
   (interactive)
   (if (boundp 'compilation-error-regexp-alist-alist)
       (progn
         (if (not (assoc 'vcs-syntax-error compilation-error-regexp-alist-alist))
             (mapcar
              (lambda (item)
                (push (car item) compilation-error-regexp-alist)
                (push item compilation-error-regexp-alist-alist)
                )
              vcs-error-regexp-emacs-alist)))))

(if (featurep 'emacs) (add-hook 'compilation-mode-hook 'vcs-error-regexp-add-emacs))
