(require 'tcl)

(cond ((fboundp 'point-at-bol)
       (defalias 'vivado-tcl-point-at-bol 'point-at-bol)
       (defalias 'vivado-tcl-point-at-eol 'point-at-eol))
      ;; Emacs 20.4
      ((fboundp 'line-beginning-position)
       (defalias 'vivado-tcl-point-at-bol 'line-beginning-position)
       (defalias 'vivado-tcl-point-at-eol 'line-end-position))
      (t
       (defmacro vivado-tcl-point-at-bol ()
	 (save-excursion (beginning-of-line) (point)))
       (defmacro vivado-tcl-point-at-eol ()
	 (save-excursion (end-of-line) (point)))))


(defvar vivado-tcl-shell-mode-map nil)

(defvar vivado-tcl-shell-echoes t )

(defvar vivado-tcl-shell-input-ring-size 32)
(defvar vivado-tcl-shell-history-file "~/.vivado/history.tcl")
(defvar vivado-tcl-font-lock-syntactic-keywords
  ;; Mark the few `#' that are not comment-markers.
  '(("[^;[{ \t\n][ \t]*\\(#\\)" (1 ".")))
  "Syntactic keywords for `tcl-mode'.")

(defvar vivado-tcl-keyword-list
  '("if" "then" "else" "elseif" "for" "foreach" "break" "continue" "while"
    "eval" "case" "in" "switch" "default" "exit" "error" "proc" "return"
    "uplevel" "constructor" "destructor" "itcl_class" "loop" "for_array_keys"
    "for_recursive_glob" "for_file" "method" "body" "configbody" "class"
    "chain")
  "List of Tcl keywords.  Used only for highlighting.
Default list includes some TclX keywords.
Call `tcl-set-font-lock-keywords' after changing this list.")

(defvar vivado-tcl-shell-command "vivado.bat" "")
(defvar vivado-tcl-shell-command-switches '("-nolog" "-nojournal" "-mode" "tcl") "")

(defvar vivado-tcl-shell-font-lock-keywords (list
	 ;; Names of type-defining things.
	 (list (concat "\\(\\s-\\|^\\)"
		       (regexp-opt tcl-typeword-list t)
		       "\\(\\s-\\|$\\)")
	       2 'font-lock-type-face)

         (list (concat "\\_<" (regexp-opt tcl-builtin-list t) "\\_>")
	       1 'font-lock-builtin-face)

         ;; When variable names are enclosed in {} braces, any
         ;; character can be used. Otherwise just letters, digits,
         ;; underscores.  Variable names can be prefixed with any
         ;; number of "namespace::" qualifiers.  A leading "::" refers
         ;; to the global namespace.
         '("\\${\\([^}]+\\)}" 1 font-lock-variable-name-face)
         '("\\$\\(\\(?:::\\)?\\(?:[[:alnum:]_]+::\\)*[[:alnum:]_]+\\)"
           1 font-lock-variable-name-face)
         '("\\(?:\\s-\\|^\\|\\[\\)set\\s-+{\\([^}]+\\)}"
           1 font-lock-variable-name-face keep)
         '("\\(?:\\s-\\|^\\|\\[\\)set\\s-+\\(\\(?:::\\)?\
\\(?:[[:alnum:]_]+::\\)*[[:alnum:]_]+\\)"
           1 font-lock-variable-name-face keep)

         '("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'tcl-escaped-newline)

	 ;; Keywords.  Only recognized if surrounded by whitespace.
	 ;; FIXME consider using "not word or symbol", not
	 ;; "whitespace".
	 (cons (concat "\\_<" (regexp-opt vivado-tcl-keyword-list t) "\\_>")
	       1))
)

;; (defvar matlab-shell-font-lock-keywords-1
;;   (append matlab-font-lock-keywords matlab-shell-font-lock-keywords)
;;   "Keyword symbol used for font-lock mode.")

;; (defvar matlab-shell-font-lock-keywords-2
;;   (append matlab-shell-font-lock-keywords-1 matlab-gaudy-font-lock-keywords)
;;   "Keyword symbol used for gaudy font-lock symbols.")

;; (defvar matlab-shell-font-lock-keywords-3
;;   (append matlab-shell-font-lock-keywords-2
;; 	  matlab-really-gaudy-font-lock-keywords)
;;   "Keyword symbol used for really gaudy font-lock symbols.")

(defvar vivado-tcl-shell-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?%  "_"  st)
    (modify-syntax-entry ?@  "_"  st)
    (modify-syntax-entry ?&  "_"  st)
    (modify-syntax-entry ?*  "_"  st)
    (modify-syntax-entry ?+  "_"  st)
    (modify-syntax-entry ?-  "_"  st)
    (modify-syntax-entry ?.  "_"  st)
    (modify-syntax-entry ?:  "_"  st)
    (modify-syntax-entry ?!  "_"  st)
    (modify-syntax-entry ?$  "_"  st)	; FIXME use "'"?
    (modify-syntax-entry ?/  "_"  st)
    (modify-syntax-entry ?~  "_"  st)
    (modify-syntax-entry ?<  "_"  st)
    (modify-syntax-entry ?=  "_"  st)
    (modify-syntax-entry ?>  "_"  st)
    (modify-syntax-entry ?|  "_"  st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\; "."  st)
    (modify-syntax-entry ?\n ">"  st)
    ;; (modify-syntax-entry ?\f ">"  st)
    (modify-syntax-entry ?#  "<"  st)
    st)
  "Syntax table in use in `vivado-tcl-mode' buffers. derived from the tcl mode")



(defvar vivado-tcl-shell-buffer-name "vivado"
  "Name used to create `vivado-tcl-shell' mode buffers.
This name will have *'s surrounding it.")

(defun vivado-tcl-shell-active-p ()
  "Return t if the vivado tcl shell is active."
  (if (get-buffer (concat "*" vivado-tcl-shell-buffer-name "*"))
      (save-excursion
	(set-buffer (concat "*" vivado-tcl-shell-buffer-name "*"))
	(if (comint-check-proc (current-buffer))
	    (current-buffer)))))

(defun vivado-tcl-shell-last-error ()
  "In the vivado tcl shell interactive buffer, find the last vivado error, and go there.
To reference old errors, put the cursor just after the error text."
  (interactive)
  )

(defun vivado-tcl-shell ()
  "Create a buffer with vivado running as a subprocess."
  (interactive)
  (require 'shell)
  (switch-to-buffer (concat "*" vivado-tcl-shell-buffer-name "*"))
  (if (vivado-tcl-shell-active-p)
      nil
    ;; Clean up crufty state
    (kill-all-local-variables)
    ;; Build keymap here in case someone never uses comint mode
    (if vivado-tcl-shell-mode-map
	()
      (setq vivado-tcl-shell-mode-map
	    (let ((km (make-sparse-keymap 'vivado-tcl-shell-mode-map)))
	      (if (fboundp 'set-keymap-parent)
		  (set-keymap-parent km comint-mode-map)
		;; 19.31 doesn't have set-keymap-parent
		(setq km (nconc km comint-mode-map)))
	      (substitute-key-definition 'next-error 'vivado-tcl-shell-last-error
					 km global-map)
	      ;; (define-key km [(control h) (control m)]
	      ;; 	matlab-help-map)
              ;; (define-key km "\C-c." 'matlab-find-file-on-path)
	      ;; (define-key km [(tab)] 'matlab-shell-tab)
	      (define-key km [(control up)]
		'comint-previous-matching-input-from-input)
	      (define-key km [(control down)]
		'comint-next-matching-input-from-input)
	      (define-key km [up]
		'comint-previous-matching-input-from-input)
	      (define-key km [down]
		'comint-next-matching-input-from-input)
	      (define-key km [(control return)] 'comint-kill-input)
	      ;; (define-key km "\C-?"
	      ;; 	'matlab-shell-delete-backwards-no-prompt)
	      ;; (define-key km [(backspace)]
	      ;; 	'matlab-shell-delete-backwards-no-prompt)
	      km)))
    (switch-to-buffer
      (apply 'make-comint vivado-tcl-shell-buffer-name vivado-tcl-shell-command
		   nil vivado-tcl-shell-command-switches))
    
    (setq shell-dirtrackp t)
    (comint-mode)
    ;; Comint and GUD both try to set the mode.  Now reset it to
    ;; matlab mode.
    (vivado-tcl-shell-mode)))


(defun vivado-tcl-shell-mode ()
  (setq major-mode 'vivado-tcl-shell-mode
	mode-name "vivado-tcl-shell"
	comint-prompt-regexp "^Vivado% *"
	comint-delimiter-argument-list (list [ 59 ]) ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes vivado-tcl-shell-echoes
	)
  (require 'shell)
  (if (fboundp 'shell-directory-tracker)
      (add-hook 'comint-input-filter-functions 'shell-directory-tracker))
  ;; Add a spiffy logo if we are running XEmacs
  ;; (if (and (string-match "XEmacs" emacs-version)
  ;; 	   (stringp matlab-shell-logo)
  ;; 	   (file-readable-p matlab-shell-logo))
  ;;     (add-hook 'comint-output-filter-functions 'matlab-shell-hack-logo))
  ;; Add pseudo html-renderer
  ;;(add-hook 'comint-output-filter-functions 'matlab-shell-render-html-anchor nil t)
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  ;(use-local-map matlab-shell-mode-map)
  (use-local-map vivado-tcl-shell-mode-map)
  ;(set-syntax-table matlab-mode-syntax-table)
  (set-syntax-table vivado-tcl-shell-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  ;; (setq font-lock-defaults '((matlab-shell-font-lock-keywords-1
  ;; 			      matlab-shell-font-lock-keywords-2
  ;; 			      matlab-shell-font-lock-keywords-3)
  ;; 			     t nil ((?_ . "w"))))
  (setq font-lock-defaults '(vivado-tcl-shell-font-lock-keywords
  			     ;t nil (font-lock-syntactic-keywords . tcl-font-lock-syntactic-keywords)
  			     t nil 
			     (parse-sexp-lookup-properties . t) ) )
  (set (make-local-variable 'comint-input-ring-size)
       vivado-tcl-shell-input-ring-size)
  (set (make-local-variable 'comint-input-ring-file-name)
       vivado-tcl-shell-history-file)
  (set (make-local-variable 'company-backends) 
       '(company-vivado-tcl
	 company-yasnippet  
  	 company-bbdb
  	 company-capf
  	 (company-dabbrev-code company-gtags company-etags
  			       company-keywords) 
  	 company-files company-dabbrev) ) ;
  ;; (set 'company-backends 
  ;;      '(company-yasnippet  
  ;; 	 company-bbdb
  ;; 	 company-capf
  ;; 	 (company-dabbrev-code company-gtags company-etags
  ;; 			       company-keywords)
  	 ;; company-files company-dabbrev) ) ;
  (if (fboundp 'comint-read-input-ring)
      (comint-read-input-ring t))
  (run-hooks 'vivado-tcl-shell-mode-hook)
  )



;; (define-derived-mode vivado-tcl-shell-mode shell-mode "xtcl"
;;   "Major mode for xilinx vivado tcl shell. Special commands: \\ quip-mode-map}"
;; )


;; (defun xtcl-shell (&optional buffer)
;;   ""
;;   (interactive
;;    (list
;;     (and current-prefix-arg
;; 	 (prog1
;; 	     (read-buffer "Xtcl buffer: "
;; 			  (generate-new-buffer-name "*xtcl*"))
;; 	   (if (file-remote-p default-directory)
;; 	       ;; It must be possible to declare a local default-directory.
;; 	       (setq default-directory
;; 		     (expand-file-name
;; 		      (read-file-name
;; 		       "Default directory: " default-directory default-directory
;; 		       t nil 'file-directory-p))))))))
;;   (setq buffer (get-buffer-create (or buffer "*xtcl*")))
;;   ;; Pop to buffer, so that the buffer's window will be correctly set
;;   ;; when we call comint (so that comint sets the COLUMNS env var properly).
;;   (pop-to-buffer buffer)
;;   (unless (comint-check-proc buffer)
;;     ;; (let* ((prog (or explicit-shell-file-name
;;     ;; 		     (getenv "ESHELL") shell-file-name))
;;     (let* ((prog "/eda/Xilinx/Xilinx/ISE/bin/lin/xtclsh")
;; 	   (name (file-name-nondirectory prog))
;; 	   (startfile (concat "~/.emacs_" name))
;; 	   (xargs-name (intern-soft (concat "explicit-" name "-args"))))
;;       (unless (file-exists-p startfile)
;; 	(setq startfile (concat user-emacs-directory "init_" name ".sh")))
;;       (apply 'make-comint-in-buffer "shell" buffer prog
;; 	     (if (file-exists-p startfile) startfile)
;; 	     (if (and xargs-name (boundp xargs-name))
;; 		 (symbol-value xargs-name)
;; 	       '("-i")))
;;       (xtcl-mode)))
;;   buffer)
;; (provide `xtcl-shell-mode)


(defun vivado-tcl-shell-send-string (string)
  "Send STRING to the currently running matlab process."
  (if (not vivado-tcl-shell-echoes)
      (let ((proc (get-buffer-process (current-buffer))))
	(goto-char (point-max))
	(insert string)
	(set-marker (process-mark proc) (point))))
  (comint-send-string (get-buffer-process (current-buffer)) string))

(defun vivado-tcl-shell-buffer-barf-not-running ()
  "Return a running MATLAB buffer iff it is currently active."
  (or (vivado-tcl-shell-active-p)
      (error "You need to run the command `vivado-tcl-shell' to do that!")))

(defun vivado-on-prompt-p ()
  "Return t if we vivado can accept input."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (looking-at comint-prompt-regexp))))

(defun vivado-tcl-shell-save-and-go ()
  "Save this tcl file, and evaluate it in a vivado tcl shell."
  (interactive)
  (if (not (eq major-mode 'tcl-mode))
      (error "Save and go is only useful in a  tcl  buffer!"))
  (if (not (buffer-file-name (current-buffer)))
      (call-interactively 'write-file))
  (let ((fn-name  (buffer-file-name))
	(msbn (concat "*" vivado-tcl-shell-buffer-name "*"))
	(param ""))
    (save-buffer)
    ;; Do we need parameters?
    ;; (if (save-excursion
    ;; 	  (goto-char (point-min))
    ;; 	  (end-of-line)
    ;; 	  (forward-sexp -1)
    ;; 	  (looking-at "([a-zA-Z]"))
    ;; 	(setq param (read-string "Parameters: "
    ;; 				 (car matlab-shell-save-and-go-history)
    ;; 				 'matlab-shell-save-and-go-history)))
    (if nil
	;; Execute the current file in MATLAB
	nil

      ;; No buffer?  Make it!
      (if (not (get-buffer msbn)) (vivado-shell))
      ;; Ok, now fun the function in the matlab shell
      (if (get-buffer-window msbn t)
	  (select-window (get-buffer-window msbn t))
	(switch-to-buffer (concat "*" vivado-tcl-shell-buffer-name "*")))

      ;; (let ((cmd (concat fn-name " " param)))
      ;; 	(matlab-shell-add-to-input-history cmd)
      ;; 	(matlab-shell-send-string (concat cmd "\n")))
      (vivado-tcl-shell-send-string (concat "source" " " fn-name "\n"))
      )
))

(defun vivado-tcl-shell-run-region (beg end)
  "Run region from BEG to END and display result in vivado tcl shell.
This command requires an active vivado tcl shell."
  (interactive "r")
  (if (> beg end) (let (mid) (setq mid beg beg end end mid)))
  (let ((command (let ((str (concat (buffer-substring-no-properties beg end)
 				    "\n")))
 		   (while (string-match "\n\\s-*\n" str)
 		     (setq str (concat (substring str 0 (match-beginning 0))
 				       "\n"
 				       (substring str (match-end 0)))))
 		   str))
 	(msbn nil)
 	(lastcmd)
	(inhibit-field-text-motion t))
    (if nil
	;; Run the region w/ Emacs Link
	nil
      (save-excursion
	(setq msbn (vivado-tcl-shell-buffer-barf-not-running))
	(set-buffer msbn)
	(if (not (vivado-on-prompt-p))
	    (error "vivado shell must be non-busy to do that"))
	;; Save the old command
	(beginning-of-line)
	(re-search-forward comint-prompt-regexp)
	(setq lastcmd (buffer-substring (point) (vivado-tcl-point-at-eol)))
	(delete-region (point) (vivado-tcl-point-at-eol))
	;; We are done error checking, run the command.
	(vivado-tcl-shell-send-string command)
	(insert lastcmd))
      (set-buffer msbn)
      (goto-char (point-max))
      (display-buffer msbn))
    ))

(defun vivado-tcl-shell-run-region-or-line ()
  "Run region from BEG to END and display result in MATLAB shell.
If region is not active run the current line.
This command requires an active MATLAB shell."
  (interactive)
 ;(if zmacs-region-active-p
  (if (use-region-p)
     (vivado-tcl-shell-run-region (mark) (point))
   (vivado-tcl-shell-run-region (vivado-tcl-point-at-bol) (vivado-tcl-point-at-eol))))
 


(provide 'vivado-tcl-shell-mode)
