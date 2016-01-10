;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define global variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq message-log-max t)
(setq x-select-enable-clipboard t) 
(setq frame-title-format "qiuxy@%b-emacs")
(setq visible-bell t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; (setq default-frame-alist
;;        '((foreground-color . "Grey")
;;          (background-color . "Black")
;;          (cursor-color     . "Orchid")
;; 	 ;(font . "fontset-courier")
;; 	 (top  . 0 )
;; 	 (left . 1300 )
;; 	 (height . 62 )
;; 	 (width  . 158 )
;; 	 ;(menu-bar-lines . nil )
;; 	 (tool-bar-lines . nil )
;; 	 (vertical-scroll-bars . nil)
;; ))

(setq initial-frame-alist
       '((foreground-color . "Grey")
         (background-color . "Black")
         (cursor-color     . "Orchid")
	 (top  . 0 )
	 (left . 0 )
	 ;(font . "fontset-courier")
	 (tool-bar-lines . nil )
	 (vertical-scroll-bars . nil)
	 
	 ))
(setq default-frame-alist initial-frame-alist)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(info-reference-item ((((type tty pc)) :background "white" :foreground "black") (t :background "white" :foreground "cornflower blue"))))
;;(set-face-foreground 'info-function-ref-item "deeppink1")


(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(setq dired-recursive-deletes t)
(setq dired-recursive-copies t) 
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode t)
 '(delete-auto-save-files t)
 '(ecb-options-version "2.32")
 '(ede-project-directories (quote ("/home/agump/study/xilinx_pcie/1052/dma_performance_demo/linux_sw/xbmd" "/home/agump/ede")))
 '(hippie-expand-try-functions-list (quote (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(matlab-shell-ask-MATLAB-for-completions t)
 '(matlab-shell-command "/home/qiuxy/bin/mmatlab")
 '(matlab-shell-command-switches nil)
 '(matlab-shell-echoes nil)
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 3000)
 '(menu-bar-mode nil)
 '(outline-regexp "!!.*" t)
 '(safe-local-variable-values (quote ((folded-file . t) (folding-internal-margins) (checkdoc-permit-comma-termination-flag . t) (checkdoc-force-docstrings-flag) (verilog-library-directories "../../src" ".") (verilog-library-directories "./clk_rst" "." "./cpu_if" "./dds_core" "./device") (verilog-library-directories "/eda/Xilinx/Xilinx/ISE/verilog/src/unisims/"))))
 '(scroll-bar-mode nil)
 '(setq show-paren-match)
 '(spice-simulator "Hspice")
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#dddd00")
 '(tool-bar-mode nil nil (tool-bar))
 '(vera-intelligent-tab nil)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-compiler "make -k")
 '(verilog-date-scientific-format t t)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-p1800-keywords t)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-module 2)
 '(verilog-library-directories (quote ("/eda/Xilinx/Xilinx/ISE/verilog/src/unisims/" ".")))
 '(vhdl-indent-tabs-mode t)
 '(vhdl-intelligent-tab nil)
 '(vhdl-upper-case-keywords t)
 '(yas/ignore-filenames-as-triggers t)
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt))))
