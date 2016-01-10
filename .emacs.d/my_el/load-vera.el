;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto load the vera mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/lib/hdl_mode/vera-mode-2.17/" load-path))
(autoload 'vera-mode "vera-mode" "Vera Mode" t)
(setq auto-mode-alist (cons '("\\.vr[hi]?\\'" . vera-mode) auto-mode-alist))
