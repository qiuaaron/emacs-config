(require 'shell)
(require 'tcl)

(defun xtcl-shell-current-word (flag)
  "Return current command word, or nil.
If FLAG is nil, just uses `current-word'.
Otherwise scans backward for most likely Tcl command word."
  (if (and flag
	   (memq major-mode '(xtcl-shell-mode)))
      (condition-case nil
	  (save-excursion
	    ;; Look backward for first word actually in alist.
	    (if (bobp)
		()
	      ;(while (and (not (bobp))
	      (while (and (not (bobp))
			  (not (tcl-real-command-p)))
		(backward-sexp)))
	    (if (assoc (tcl-word-no-props) tcl-help-alist)
		(tcl-word-no-props)))
	(error nil))
    (tcl-word-no-props)))


(defun xtcl-shell-test-temp ()
  (interactive)
  (let  ( (my-buffer (current-buffer ) ) 
	  (my-first-point ( point ) )
	  (back-lines 0)
	  (forward-lines 0 )
	  )
    (save-excursion)

)

)

;; (defun xtcl-shell-help-temp ( )
;;   (interactive)
;;   ( let ( ( my-save-buffer (current-buffer ) ) newbuf (mypoint 0 ) )
;;     ( prog 
;;       (save-excursion 
;; 	     (setq mypoint (point) )
;; 	     (comint-bol)
;; 	     ;(inser)
;; 	     (setq mypoint (- mypoint (point)))
      
;;       (save-excursion 
;; 	(with-current-buffer (get-buffer-create "my-temp-buf") 
;; 	       (barf-if-buffer-read-only)
;; 	       (erase-buffer)
;; 	       (goto-char (point-min))
;; 	       (setq newbuf (current-buffer ) )
;; 	       (switch-to-buffer  my-save-buffer )
;; 	       )
;; 	     )
;; 	   (save-excursion 
;; 	     (setq mypoint (point) )
;; 	     (comint-bol)
;; 	     ;(inser)
;; 	     (setq mypoint (- mypoint (point)))
	     
;; 	     ( while 
;; 	     (previous-line)
;; 	     (move-end-of-line)
;; 	     (setq mypoint (+ mypoint (point)))
;; 	   ()
	   
;;     )
;;   )

(defun xtcl-shell-help-on-word (command &optional arg)
  "Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'."
  (interactive
   (list
    (progn
      (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
	  (tcl-reread-help-files))
      (let ((word (xtcl-shell-current-word
		   (if current-prefix-arg
		       (not tcl-use-smart-word-finder)
		     tcl-use-smart-word-finder))))
	(completing-read
	 (if (or (null word) (string= word ""))
	     "Help on Tcl command: "
	   (format "Help on Tcl command (default %s): " word))
	 tcl-help-alist nil t nil nil word)))
    current-prefix-arg))
  (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
      (tcl-reread-help-files))
  (if (string= command "")
      (setq command (xtcl-shell-current-word
		     (if arg
			 (not tcl-use-smart-word-finder)
		       tcl-use-smart-word-finder))))
  (let* ((help (get-buffer-create "*Tcl help*"))
	 (cell (assoc command tcl-help-alist))
	 (file (and cell (cdr cell))))
    (set-buffer help)
    (delete-region (point-min) (point-max))
    (if file
	(progn
	  (insert "*** " command "\n\n")
	  (insert-file-contents file))
      (if (string= command "")
	  (insert "Magical Pig!")
	(insert "Tcl command " command " not in help\n")))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (display-buffer help)))


(defvar xtcl-shell-echoes t )




(define-derived-mode xtcl-shell-mode shell-mode "xtcl-shell"
"Major mode for xilinx tcl shell. Special commands: \\ quip-mode-map}"
					;(define-key sdc-mode-map (kbd "C-'") 'sdc-list-cmd)
(set (make-local-variable 'comint-prompt-regexp)
       (or tcl-prompt-regexp
	   (concat "^" (regexp-quote tcl-application) ">")))

(setq mode-line-process '(": %s"))
(setq local-abbrev-table tcl-mode-abbrev-table)
(set-syntax-table tcl-mode-syntax-table)
(set (make-local-variable 'defun-prompt-regexp) tcl-omit-ws-regexp)
(set (make-local-variable 'inferior-tcl-delete-prompt-marker) (make-marker))
(set-process-filter (get-buffer-process (current-buffer)) 'tcl-filter)

)


(defun xtcl-shell (&optional buffer)
  ""
  (interactive
   (list
    (and current-prefix-arg
	 (prog1
	     (read-buffer "Xtcl buffer: "
			  (generate-new-buffer-name "*xtcl*"))
	   (if (file-remote-p default-directory)
	       ;; It must be possible to declare a local default-directory.
	       (setq default-directory
		     (expand-file-name
		      (read-file-name
		       "Default directory: " default-directory default-directory
		       t nil 'file-directory-p))))))))
  ;;(setq comint-prompt-regexp "^Vivado\\(%\\|-\\) *"
  (setq comint-prompt-regexp "^Vivado% *"
	comint-delimiter-argument-list (list [ 59 ]) ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes xtcl-shell-echoes
	)
  (setq buffer (get-buffer-create (or buffer "*xtcl*")))
  ;; Pop to buffer, so that the buffer's window will be correctly set
  ;; when we call comint (so that comint sets the COLUMNS env var properly).
  (pop-to-buffer buffer) 
  (unless (comint-check-proc buffer)
    (let* ((prog "vivado.bat")
	   (name (file-name-nondirectory prog))
	   (startfile (concat "~/.emacs_" name))
	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
	(setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
	     (if (file-exists-p startfile) startfile)
	     (if (and xargs-name (boundp xargs-name))
		 (symbol-value xargs-name)
	       '("-nolog -nojournal -mode tcl")))
      (if (fboundp 'comint-read-input-ring)
	  (comint-read-input-ring t))
      (xtcl-shell-mode))))
					;buffer)
(provide `xtcl-shell-mode)
