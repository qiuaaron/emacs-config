;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;auto load the compilation-mode mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'compilation-mode "compilation-mode" "compilation Mode" t)
(setq auto-mode-alist (cons '("\\.comlog\\'" . compilation-mode) auto-mode-alist))



(add-hook 'compilation-mode-hook ( lambda ()
					 (local-set-key "j" 'next-line)
					 (local-set-key "k" 'previous-line)
					 (local-set-key "p" 'compilation-previous-error)
					 (local-set-key "n" 'compilation-next-error)

					 )
