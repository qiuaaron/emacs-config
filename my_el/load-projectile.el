(setq load-path (cons "~/.emacs.d/emacs_mode/projectile/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/s.el/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/pkg-info.el/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/dash.el/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/epl/" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/ack-and-a-half/" load-path))
(require 'helm-projectile)
;; 默认全局使用
(projectile-global-mode)
;; 默认打开缓存
(setq projectile-enable-caching t)





;;
(defvar project-one-key-menu-alist nil)
(setq project-one-key-menu-alist 
      '(
	( ( "f" . "find project file") . helm-projectile)
	( ( "g" . "search in project file" ) . projectile-grep)
	( ( "a" . "switch between alternate files" ) . projectile-find-other-file )
	;; ( ( "g" . "go to gdb window") . Aaron-erilog-remove-module-signal )
	;; ( ( "C-i" . "instantiated module with param ") . Aaron-verilog-inst-module-with-param )       ( ( "c" . "generate tag") . my-compile-verilog-gentag )
	)
      )
(defun project-one-key-menu()
  (interactive)
  (one-key-menu "project" project-one-key-menu-alist  t )
)   

(global-set-key ( kbd  "C-0") 'project-one-key-menu )



;;

