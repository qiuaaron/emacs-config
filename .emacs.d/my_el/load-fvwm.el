;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load fvwm mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "~/.emacs.d/emacs_mode/fvwm-mode/" load-path))
(require 'fvwm-mode)
(add-to-list 'auto-mode-alist '(".*fvwm2rc\\'" . fvwm-mode))
(add-to-list 'auto-mode-alist '(".*\\.fm\\'" . fvwm-mode))

