
(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key shell-mode-map (kbd "<down>") 'comint-next-input)))

(add-hook 'shell-mode-hook
          (lambda ()
            ( verilog-error-regexp-add )))
