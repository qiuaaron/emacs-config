;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;load sdc mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'sdc-mode "sdc-mode" "sdc mode" t )
(setq auto-mode-alist (cons  '("\\.sdc\\'" . sdc-mode) auto-mode-alist))


;; (defun ac-sdc-candidate ()
;;   ""
;;   (append sdc-keyword sdc-ext-keyword sdc-sta-keyword)
  
;; )
;; (ac-define-source sdc
;;   '(
;;     (candidates . ac-sdc-candidate)
;;     (document . ac-sdc-help)
;;     (symbol . "sdc")))
;; (defvar timequest-sdc-help-directory "~/lib/help/txt/timequest/timequest")
;; (defun ac-sdc-help( symbol )
;;   ""
;;   ( with-temp-buffer
;;     ( let ((standard-output (current-buffer )))
;;       ( insert-file (concat timequest-sdc-help-directory "/" symbol ))
;; ;;        (princ (or (documentation-property symbol 'variable-documentation t)
;; ;; 		  "Not documented."))
;;       (buffer-string)
;;       )
;;     )
;;   ;; (princ symbol)
;; ;;   (message "test")
;;   )


(add-hook 'sdc-mode-hook 
	  ( lambda () 
	    (message "load sdc-mode")
	    (setq tcl-help-directory-list '("~/lib/help/txt/timequest"))
	    
	    ;config for auto complete
	    ;; (setq ac-sources '( ac-source-sdc ac-source-filename ac-source-words-in-same-mode-buffers ac-source-yasnippet ac-source-filename))	    
	    ;; (define-key ac-mode-map (kbd "C-?") 'ac-persist-help)
	    ) )

;; (add-to-list 'ac-modes 'sdc-mode)


