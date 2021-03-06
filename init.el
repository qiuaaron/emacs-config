;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load my file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/.emacs.d/emacs_mode" load-path))
(setq load-path (cons "~/.emacs.d/my_el" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/dea" load-path) )
(require 'util)
(load-file "~/.emacs.d/my_el/load-dired.el")
(load-file "~/.emacs.d/my_el/myutility.el")
(load-file "~/.emacs.d/my_el/global-set.el")
(load-file "~/.emacs.d/my_el/load-basic-mode.el")
(load-file "~/.emacs.d/my_el/load-frame-manage.el")
					;(load-file "~/.emacs.d/my_el/load-auto-complete.el")
					;(load-file "~/.emacs.d/my_el/load-ace-jump.el")
(load-file "~/.emacs.d/my_el/load-avy.el")
(load-file "~/.emacs.d/my_el/load-helm.el")
(load-file "~/.emacs.d/my_el/load-projectile.el")
(load-file "~/.emacs.d/my_el/load_mode.el")
(load-file "~/.emacs.d/my_el/load-company.el")
(load-file "~/.emacs.d/my_el/load-yasnippet.el")
(load-file "~/.emacs.d/my_el/load-bbyac.el")
(load-file "~/.emacs.d/my_el/load-switchwindow.el")
(load-file "~/.emacs.d/my_el/load-bookmark.el")


(load-file "~/.emacs.d/my_el/load-elisp.el")
(load-file "~/.emacs.d/my_el/load-verilog.el")
(load-file "~/.emacs.d/my_el/load-shell-mode.el")
(load-file "~/.emacs.d/my_el/load-vhdl.el")
(load-file "~/.emacs.d/my_el/load-xml-mode.el")
(load-file "~/.emacs.d/my_el/load-sdc.el")
(load-file "~/.emacs.d/my_el/load-c.el")
(load-file "~/.emacs.d/my_el/load-python.el")
(load-file "~/.emacs.d/my_el/load-matlab.el")
(load-file "~/.emacs.d/my_el/load-perl.el")
(load-file "~/.emacs.d/my_el/load-tcl-mode.el")
(load-file "~/.emacs.d/my_el/load-sawfish.el")
(load-file "~/.emacs.d/my_el/load-xtcl.el")
(load-file "~/.emacs.d/my_el/load-vivado-tcl-shell-mode.el")
(load-file "~/.emacs.d/my_el/load-dts.el")
(load-file "~/.emacs.d/my_el/load-fvwm.el")


(load-file "~/.emacs.d/my_el/rigol.el")
(load-file "~/.emacs.d/my_el/Aaron-verilog.el" )
(load-file "~/.emacs.d/my_el/Aaron.el" )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the global key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'shell)

(global-set-key "\C-j" 'join-line )
(global-set-key "\C-c\c" 'compile)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\C-x\M-l" 'locate-with-filter)
(global-set-key [\C-backspace] 'backward-kill-word)

(global-set-key [f12] 'my-compile-verilog)
(global-set-key (kbd "C-<f12>") 'my-compile-verilog-novas)
(global-set-key [f2] 'my-lint-verilog)

;(define-key yas-minor-mode-map (kbd "S-<SPC>" ) 'company-yasnippet )
(global-set-key  (kbd "S-<SPC>" ) 'company-yasnippet )
;(global-set-key ( kbd  "S-<SPC>") 'yas/expand )
(global-set-key ( kbd  "M-p") 'yas/prev-field )
(global-set-key ( kbd  "M-n") 'yas/next-field )


(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)


(global-set-key "\C-x\C-hhb" 'hs-hide-block)
(global-set-key "\C-x\C-hsb" 'hs-show-block)
(global-set-key "\C-x\C-hha" 'hs-hide-all)
(global-set-key "\C-x\C-hsa" 'hs-show-all)
(global-set-key "\C-x\C-hhl"  'hs-hide-level)
(global-set-key "\C-x\C-ht" 'hs-toggle-hiding)



;(global-set-key (kbd "C-'") 'anything)

;(global-set-key "\M-1" 'quick-calc)



;(global-set-key (kbd "C-0") 'Aaron-anything-find-occur)


(global-set-key (kbd "C-t") 'multi-term-next)

(global-set-key (kbd "\C-hh") 'Aaron-find-help )
;(global-set-key (kbd "<C-tab>" ) 'other-frame )

(global-set-key "\M-y" 'helm-show-kill-ring)
(global-set-key "\C-xb" 'helm-mini)
(global-set-key "\C-x\C-f" 'helm-find-files)
(global-set-key "\M-x" 'helm-M-x)


(global-set-key "\C-xo" 'switch-window)


(global-set-key (kbd "C-,") 'avy-goto-char)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'tt-mode "tt-mode")
 (setq auto-mode-alist
  (append '(("\\.tt$" . tt-mode))  auto-mode-alist ))

;:::::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the session mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "/home/.emacs.d/ses21-031130/")
(autoload 'ses-mode "ses.el" "Spreadsheet mode" t)
(add-to-list 'auto-mode-alist '("\\.ses$" . ses-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define my variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(desktop-load-default)

;(desktop-read)

(defvar gdb-many-windows t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;run some command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq compilation-scroll-output t)


;(require 'psvn)

(setq outline-regexp ".*older")

;(global-set-key ( kbd  "S-<SPC>") 'yas/next-field-or-maybe-expand)

(require 'csv-mode)
;; (global-set-key "\C-xb" 'ido-switch-buffer)
;; (global-set-key "\C-x\C-f" 'ido-find-file)

(require 'kmacro-ring-list)
;(require 'popup-kill-ring)
(setq popup-kill-ring-interactive-insert t)

;; (require 'lusty-explorer)
;; (global-set-key "\C-xb" 'lusty-buffer-explorer)
;; (global-set-key "\C-x\C-f" 'lusty-file-explorer)

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

;start emacs server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode t)
 '(delete-auto-save-files t)
 '(ecb-options-version "2.32")
 '(ede-project-directories
   (quote
    ("/home/agump/study/xilinx_pcie/1052/dma_performance_demo/linux_sw/xbmd" "/home/agump/ede")))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(matlab-cont-requires-ellipsis nil)
 '(matlab-shell-ask-MATLAB-for-completions t)
 '(matlab-shell-command "/home/qiuxy/bin/mmatlab")
 '(matlab-shell-command-switches nil)
 '(matlab-shell-echoes nil)
 '(max-lisp-eval-depth 1000)
 '(max-specpdl-size 3000)
 '(menu-bar-mode nil)
 '(outline-regexp "!!.*" t)
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/home/qiuxy/tools/systemc/include/" "-I/home/qiuxy/project_root/include2/")
     (company-clang-arguments "-I/eda/Xilinx/h/include/")
     (company-clang-arguments "-I/home/qiuxy/study/zynq_linux/fmcomms/linux-xcomm_zynq/include/")
     (emacs-lisp-docstring-fill-column . 75)
     (folded-file . t)
     (folding-internal-margins)
     (checkdoc-permit-comma-termination-flag . t)
     (checkdoc-force-docstrings-flag)
     (verilog-library-directories "../../src" ".")
     (verilog-library-directories "./clk_rst" "." "./cpu_if" "./dds_core" "./device")
     (verilog-library-directories "/eda/Xilinx/Xilinx/ISE/verilog/src/unisims/"))))
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
 '(vhdl-standard (quote (8 (math))))
 '(vhdl-upper-case-keywords t)
 '(yas-prompt-functions
   (quote
    (yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt)))
 '(yas/ignore-filenames-as-triggers t)
 '(yas/prompt-functions
   (quote
    (yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(info-reference-item ((((type tty pc)) :background "white" :foreground "black") (t :background "white" :foreground "cornflower blue"))))


(message "we set goto line")
(global-set-key "\M-g" 'goto-line )
(message "end set goto line")
