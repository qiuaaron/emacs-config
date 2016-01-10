 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/.emacs.d/emacs_mode/yasnippet/" load-path))
(require 'yasnippet) ;; not yasnippet-bundle
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
	"/home/qiuxy/lib/my_el/yasnippet-snippet"
        ))


(yas-global-mode 1)

(defadvice company-yasnippet (after company-yasnippet-add-doc 
				    activate compile )
  (message "run here")
  (message arg)
  
   (cl-case command
     (doc-buffer  
      (setq ad-return-value ( company-doc-buffer 
			      (cl-case major-mode
				(tcl-mode (shell-command-to-string
					   (concat "/home/qiuxy/lib/my_el/get-tcl-help.py "  arg) ))
				(t  "no help for the mode")
				) 
			      )
	    )
      )
     ))






;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/emacs_mode/yasnippet/yasnippet-0.6.1c/snippets" )
;; (setq yas/ignore-filenames-as-triggers t )
;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (if (yas/expansion-at-point)
;;       (progn (company-abort)
;;              (yas/expand))
;;     (company-complete-common)))

;; (defun yas/expansion-at-point ()
;;   "Tested with v0.6.1. Extracted from `yas/expand-1'"
;;     (first (yas/current-key)))
;; (require 'dropdown-list)


