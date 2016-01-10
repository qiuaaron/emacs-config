(add-to-list 'load-path "~/.emacs.d/emacs_mode/ace-jump-mode/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-.") 'ace-jump-mode)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-,") 'ace-jump-mode-pop-mark)

(defvar ace-jump-command-list '(
				( ( "C-." . "ace-jump-word-mode") . (lambda () ( interactive) (ace-jump-mode 1)))
				( ( ">" . "ace-jump-line-mode")   . (lambda () (interactive )(ace-jump-mode 16)) )
				( ( "." . "ace-jump-char-mode") . (lambda () (interactive )(ace-jump-mode 4)) )
				)
  )
