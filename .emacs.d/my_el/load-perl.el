;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cperl-mode is preferred to perl-mode                                        
;;; "Brevity is the soul of wit" <foo at acm.org>   
(defalias 'perl-mode 'cperl-mode)                            
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(dolist (interpreter '("perl" "perl5" "miniperl" "pugs"))
  (unless (assoc interpreter interpreter-mode-alist)
    (add-to-list 'interpreter-mode-alist (cons interpreter 'cperl-mode))))

( add-hook 'cperl-mode-hook
	   (lambda ( )
	     ( local-set-key "\C-c\C-r" 'perl-eval )
	     ( local-set-key "\C-c\C-hd" 'cperl-perldoc )
	     )
	   )

(defun perl-eval (beg end)
  "Run selected region as Perl code"
  (interactive "r")
  (shell-command-on-region beg end "perl")
   ; feeds the region to perl on STDIN
)

(add-hook 'cperl-mode-hook
          (lambda ()
	    (define-key cperl-mode-map "\C-j" 'join-line)
            (add-to-list 'Info-directory-list "/opt/ActivePerl-5.10/man/man1" )
            (add-to-list 'Info-directory-list "/opt/ActivePerl-5.10/man/man3" )
            (add-to-list 'Info-directory-list "/usr/share/info/perl56" )
	    )
	  )
