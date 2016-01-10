
;(require 'helm-config)
(setq load-path (cons "~/.emacs.d/emacs_mode/helm/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/helm-swoop/" load-path))
(require 'helm-swoop)


(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;
(setq helm-delete-minibuffer-contents-from-point t)


;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)


;;;;;;;;;;;;;;;;;;;;;;
;helm gtag
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-suggested-key-mapping nil
 )
(setq load-path (cons "~/.emacs.d/emacs_mode/emacs-helm-gtags/" load-path))
(require 'helm-gtags)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(defvar helm-one-key-menu-alist nil)
(setq helm-one-key-menu-alist 
      '(
	( ( "g" . "do-grep") . helm-do-grep)
	( ( "l" . "locate" ) . helm-locate)
	( ( "r" . "register" ) . helm-register )
	( ( "t" . "register" ) . helm-top )
	( ( "c" . "calc" ) . helm-calcul-expression )
	( ( "w" . "swoop" ) . helm-swoop )
	( ( "C-w" . "swoop" ) . helm-swoop-back-to-last-point )
	( ( "m" . "multi swoop for all" ) . helm-multi-swoop-all )
	( ( "M" . "multi swoop" ) . helm-multi-swoop )
	( ( "b" . "helm bookmark") . helm-bookmarks) 

 )
	
	;; ( ( "g" . "go to gdb window") . Aaron-erilog-remove-module-signal )
	;; ( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
(defun helm-one-key-menu()
  (interactive)
  (one-key-menu "helm" helm-one-key-menu-alist  t )
)   

(global-set-key ( kbd  "C-'") 'helm-one-key-menu )





