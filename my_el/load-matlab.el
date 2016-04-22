;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the matlab mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit the path in the following line to reflect the
;; actual location of the MATLAB root directory on your system.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;old config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (add-to-list 'load-path "~/lib/matlab_mode" )
;; (autoload 'matlab-eei-connect "matlab"
;;   "Connects Emacs to MATLAB's external editor interface.")

;; (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; (add-hook 'matlab-mode-hook
;;           (lambda ()
;;             (define-key matlab-mode-map "\C-j" 'join-line)
;; 	    (local-set-key (kbd "M-;") 'matlab-comment-region)
	    
;; 	    ) 
;; 	  )





;; (setq matlab-indent-function t)		if you want function bodies indented
;; (setq matlab-verify-on-save-flag nil)	turn off auto-verify on save
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 90)
;;   (imenu-add-to-menubar "Find"))	
;; 	where auto-fill should wrap
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)


;; (defun start-matlab ()
;;   (interactive)
;;   (setq matlab-shell-command "/home/qiuxy/bin/mmatlab")
;;   (matlab-shell)
;;   )

;; (defun start-sysgen ()
;;   (interactive)
;;   (setq matlab-shell-command "/home/qiuxy/bin/sysgen.bat")
;;   (matlab-shell)
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;end old config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Replace path below to be where your matlab.el file is.
(add-to-list 'load-path "~/.emacs.d/emacs_mode/matlab-mode")
(load-library "matlab-load")

;; Enable CEDET feature support for MATLAB code. (Optional)
;; (matlab-cedet-setup)

;;To use the company completion engine, add company-matlab to company-mode backends list

;;(add-to-list 'company-backends 'company-anaconda)

(require 'company-matlab)

(defun my-matlab-shell-doc (str)
  "Get a list of completions from MATLAB.
STR is a substring to complete."
  (save-excursion
    (let* ((msbn (matlab-shell-buffer-barf-not-running))
	   (cmd (concat "help " str " "))
	   (comint-scroll-show-maximum-output nil)
	   output
	   (completions nil))
      (set-buffer msbn)
      (if (not (matlab-on-prompt-p))
	  (error "MATLAB shell must be non-busy to do that"))
      (matlab-shell-collect-command-output cmd)
      )))
(defun my-company-matlab-get-doc (cmd)
  "Retrieve completions for the given command CMD."
  (let* ((pt-start (save-excursion (matlab-beginning-of-command) (point)))
         (pt-shell nil)
	 (my-cmd cmd)
         (cmd (buffer-substring-no-properties pt-start (point)))
         (completions nil)
         (pre-shell-line nil)
         (post-shell-line nil)
         (shell-buf (get-buffer (concat "*" matlab-shell-buffer-name "*")))
         (inhibit-quit nil))            ; For some reason this is set and produces error
    ;; Remove everything before ;
    (setq cmd (replace-regexp-in-string ".*;" "" cmd))
    ;; Replace every ' with ''''
    (setq cmd (replace-regexp-in-string "'" "''''" cmd))
    (with-current-buffer shell-buf
      ;; Remember shell cursor position
      (setq pt-shell (point))
      (setq pre-shell-line (save-excursion
                             (let ((inhibit-field-text-motion t))
                               (beginning-of-line))
                             (point)))
      ;; Get completions
      (setq completions (let ((comint-inhibit-carriage-motion t))
                          (my-matlab-shell-doc my-cmd))))
    ;; Restore point if it was misplaced in console
    (dolist (win (get-buffer-window-list shell-buf nil t))
      (with-selected-window win
        (setq post-shell-line (save-excursion
                                (let ((inhibit-field-text-motion t))
                                  (beginning-of-line))
                                (point)))
        (goto-char (+ post-shell-line (- pt-shell pre-shell-line)))))
					;(mapcar 'car completions)))
    (concat "help:" completions)))

(defun company-matlab (command &optional arg &rest ignored)
  "A `company-mode' completion back-end for matlab-mode and matlab-shell-mode."
  (interactive (list 'interactive))
  (cl-case command
    ('interactive (company-begin-backend 'company-matlab))
    (doc-buffer (company-doc-buffer (my-company-matlab-get-doc arg) ) )
    ;))))
    ;;('doc (my-company-matlab-get-doc arg))
    ('prefix (company-matlab-prefix))
    ('candidates (company-matlab-get-completions arg))
    ('sorted t)))

(defun show-matlab-help ( cmd )
  (interactive)
  (with-temp-buffer
    (insert-buffer   (my-company-matlab-get-doc cmd)) )
  )

(add-hook 'matlab-mode-hook 
( lambda ( )
  (set (make-local-variable 'company-backends) 
       '(company-matlab
	 company-yasnippet
	 
	 company-bbdb
	 company-capf
	 (company-dabbrev-code company-gtags company-etags
			       company-keywords)
	 company-files company-dabbrev) )
  )
)

;;NOTE! In order for the completions to work Matlab shell must be started!

;;To use the flycheck mlint backedn, add the following to your init file:

(eval-after-load 'flycheck
  '(require 'flycheck-matlab-mlint))

(defun start-matlab ()
  (interactive)
  (setq matlab-shell-command "/home/qiuxy/bin/mmatlab")
  (matlab-shell)
  )

(defun start-sysgen ()
  (interactive)
  (setq matlab-shell-command "/home/qiuxy/bin/sysgen.bat")
  (matlab-shell)
  )

(eval-after-load "matlab-mode"
  '(define-key matlab-mode-map (kbd "C-j") nil))

;(add-to-list 'ac-modes 'matlab-mode)

