;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto load the Xilinx ucf mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ucf-mode "ucf-mode" "Ucf Mode" t)
(setq auto-mode-alist (cons '("\\.ucf\\'" . ucf-mode) auto-mode-alist))
