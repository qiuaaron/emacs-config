;customize the cmode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )


(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

					;install moo-jump-local
(add-to-list 'load-path "~/.emacs.d/emacs_mode/function-args")
(require 'function-args)
(fa-config-default)

;;sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)



(defvar c-one-key-menu-alist nil)
(setq c-one-key-menu-alist 
      '(
	( ( "m" . "gdb-many-window") . gdb-many-windows)
	( ( "u" . "uncomment region") . uncomment-region)
	
	;; ( ( "g" . "go to gdb window") . Aaron-erilog-remove-module-signal )
	;; ( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
      )
(defun c-one-key-menu()
  (interactive)
  (one-key-menu "c" c-one-key-menu-alist t )
)   


(defvar c-tag-one-key-menu-alist nil)
(setq c-tag-one-key-menu-alist 
      '(
	( ( "." . "helm tag dwim ") . helm-gtags-dwim )
	( ( "*" . "helm pop statck") . helm-gtags-pop-stack)
	( ( "r" . "helm find rtag") . helm-gtags-find-rtag)
	( ( "t" . "helm find tag ") . helm-gtags-find-tag)
	( ("]" . "helm find tag in point" ) . helm-gtags-find-tag-from-here) 
	( ("a" . "Show tagnames which are referenced in this function and jump to it") . helm-gtags-tags-in-this-function)
	( ("i" . "use imenu and semantic") . helm-semantic-or-imenu)
	;; ( ( "g" . "go to gdb window") . Aaron-erilog-remove-module-signal )
	;; ( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
      )
(defun c-tag-one-key-menu()
  (interactive)
  (one-key-menu "c" c-tag-one-key-menu-alist t )
)   




(defun cmode-hook-fun ( )
  (hs-minor-mode)
;;   (semantic-load-enable-minimum-features)
;;   (semantic-load-enable-code-helpers)
;;   (semantic-load-enable-guady-code-helpers)
;;   (semantic-load-enable-excessive-code-helpers)
;;   (semantic-load-enable-semantic-debugging-helpers)
					;(local-set-key (kbd "C-,")  'find-tag )
  (require 'imenu+)
  (local-set-key (kbd "C-9") 'c-one-key-menu)
  (local-set-key (kbd "M-.") 'c-tag-one-key-menu)

  (setq company-backends (delete 'company-semantic company-backends))
  (add-to-alist 'company-backends 'company-bbyac)


  
  (key-chord-define c-mode-map "[["  "{")
  (key-chord-define c-mode-map "]]"  "}")
  (key-chord-define c-mode-map ",,"  "<")
  (key-chord-define c-mode-map ".."  ">")
  (key-chord-define c-mode-map "\\\\"  "->")
  (key-chord-define c-mode-map "99"  "(")
  (key-chord-define c-mode-map "00"  ")")

  (key-chord-define c++-mode-map "[["  "{")
  (key-chord-define c++-mode-map "]]"  "}")
  (key-chord-define c++-mode-map ",,"  "<")
  (key-chord-define c++-mode-map ".."  ">")
  (key-chord-define c++-mode-map "\\\\"  "->")
  (key-chord-define c++-mode-map "99"  "(")
  (key-chord-define c++-mode-map "00"  ")")
  ;;binding for xref
  ;; (local-set-key "\C-cd" 'xref-push-and-goto-definition)
  ;; (local-set-key "\C-cp" 'xref-pop-and-return)
  ;; (local-set-key "\C-cc" 'xref-completion)
  ;; (local-set-key "\C-c." 'xref-next-reference)
  ;; (local-set-key "\C-c," 'xref-previous-reference)



)

( add-hook 'c++-mode-hook 
	   ( lambda ( )
	     (cmode-hook-fun)

	     )
)

(add-hook 'c-mode-hook 
	   ( lambda ( )
	     (cmode-hook-fun)

	     )
)

;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
