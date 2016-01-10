(setq load-path (cons "~/.emacs.d/emacs_mode/dts-mode" load-path))
(autoload 'dts-mode "dts-mode" "dts mode" t )
(setq auto-mode-alist (cons  '("\\.dts\\'" . dts-mode) auto-mode-alist))
(require 'dts-mode)
