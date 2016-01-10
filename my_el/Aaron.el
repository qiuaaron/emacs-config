( setq Aaron-auto-insert-num-saved-num )
( defun Aaron-auto-insert-num( )
  ""
  (interactive)
  ( while (and ( >= ( string-to-char ( buffer-substring-no-properties ( point ) (+ (point) 1 ) )  ) ( string-to-char "0" ) )
			       ( <= ( string-to-char ( buffer-substring-no-properties ( point ) ( + (point) 1 ) ) )  ( string-to-char "9" ) ) )
		    (goto-char ( - ( point ) 1 ) )
		    (message "move point")
		    )
  (goto-char ( + ( point ) 1 ) )
  ( while (and ( >= ( string-to-char ( buffer-substring-no-properties ( point ) ( + (point) 1 ) ) ) ( string-to-char "0" ) )
			       ( <= ( string-to-char ( buffer-substring-no-properties ( point ) ( + (point) 1 ) ) ) ( string-to-char "9" ) ) )
	  (delete-char 1)
	  (message "delete-char")
	  )
  )

(setq Aaron-insert-number-increase-global-number 0 )
( defun Aaron-insert-number-increase ( &optional num )
  ""
  (interactive "p")
  (message "%s\n" num)
  ( if num 
      ( )
    (setq Aaron-insert-number-increase-global-number num )
    )
  (insert (number-to-string Aaron-insert-number-increase-global-number) )
  (setq Aaron-insert-number-increase-global-number ( + Aaron-insert-number-increase-global-number 1 ) )
)

(defun Aaron-goto-incr-line( arg )
  ""
  (interactive )
  ( let ( lineno (line-number-at-pos) )
    ( goto-line ( + lineno ( - arg 1 ) ) )
    )
  )

(defun switch-buffers-between-frames ()
  "switch-buffers-between-frames switches the buffers between the two last frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    (setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    (switch-to-buffer other-frame-buffer)))




(defun Aaron-copy-buffers-between-frames( )
  "Copy current buffer to another frame, This code is borrowed from switch-buffers-between-frames"
  (interactive)
  (let ((this-frame-buffer nil)
	(other-frame-buffer nil))
    (setq this-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (other-frame 1)
    ;(setq other-frame-buffer (car (frame-parameter nil 'buffer-list)))
    (switch-to-buffer this-frame-buffer)
    (other-frame 1)
    ))




(defun Aaron-open-files-in-current-path( )
  "Open the files specified by the current file path"
  (interactive)
  ( let ( ( b1 (progn (skip-chars-backward "_A-Za-z0-9/." )
		    (point)
		    )
	     )
	  ( b2 (progn (skip-chars-forward "_A-Za-z0-9/." )
		      (point)
		      )
	       )
	  )

    (find-file ( buffer-substring-no-properties b1 b2 ) )

    )
)

(defun Aaron-two-frame-switch-make-same( )
  " "
  (interactive)
  (switch-to-buffer (car (cdr (buffer-list) )) )
  (Aaron-copy-buffers-between-frames)
)


;; (defun Aaron-incr-oct-selected( start end )
;;   "Increment the number in the selected txt as oct"
;;   (interactive "r")
;;   (
;;    (let ( (b1 (point))
;; 	  (b2 (progn (skip-chars-forward "0-9"


(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))




;; (defun Aaron-multi-occur ( )
;;   ""
;;   (interactive)
;;   (mar sta( buffer-list )
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define some function used by word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Aaron-get-word()
  ""
  (save-excursion
    ( let ( b1 b2 )
      ( skip-chars-backward "_A-Za-z0-9" )
      ( setq b1 ( point ) )
      ( skip-chars-forward "_A-Za-z0-9" )
      ( setq b2 ( point ) )
      ( buffer-substring-no-properties b1 b2 ) 
      )
    )
  )



(defun Aaron-all-occur (rexp)
  "Search all buffers for REXP."
  (interactive "MRegexp: ")
  (multi-occur (buffer-list) rexp))

;; this one {c}/{sh}ould be a completing read that would read from a
;; predefined list of filetype extensions (without requiring a match).
(defun Aaron-type-occur (extension rexp)
  "EXTENSION denotes a filetype extension to search.
Run occur in all buffers whose names match this type for REXP."
  (interactive "MExtension: \nMRegexp: ")
  (multi-occur-by-filename-regexp (concat ".*\." extension) rexp)
)

(defun Aaron-mode-occur (mode rexp)
  "Search all buffers with major mode MODE for REXP."
  (interactive (list (read-command "Mode: ")
                     (read-string "Regexp: ")))
  (multi-occur (remove-if (lambda (buf)
                            (set-buffer buf)
                            (not (eq major-mode mode)))
                          (buffer-list))
               rexp)
  )


(defun Aaron-test-func()
  ""
  (interactive)
  (if (string= (file-name-extension (buffer-file-name) ) "el")
      (message "success")
    )
  )


(defun Aaron-cm-cw-occur( )
  ""
  (interactive)
  ( let ( cmode major-mode)
    (occur-1 (Aaron-get-word) 
	   1
	   (remove-if (lambda (buf)
			(set-buffer buf)
			(not (eq major-mode cmode )))
		      (buffer-list))
	   )
    )
  )

(defconst Tcl-help-tclcmd-path "/home/qiuxy/lib/help/tcl_help/TclCmd/" )
(defconst Tcl-help-tkcmd-path "")
(defconst Altera-mnl-sdc-path "/home/qiuxy/lib/help/manual/mnl_sdctmq.htm")
(defun Aaron-tcl-find-help ( ) 
 ( interactive )
 ( let ( ( symbol ( symbol-at-point )))
   ( if ( not symbol ) 
       ( message "No symbol at point")
     ;( browse-url ( concat Tcl-help-tclcmd-path (symbol-name symbol) ".htm")))
   ( w3m-goto-url ( concat Tcl-help-tclcmd-path (symbol-name symbol) ".htm")))
   (search-forward (symbol-name symbol )) 
   ;(backword-char 1)
   ;(w3m-view-this-url)
   )
)

(defun Aaron-sdc-find-help ( ) 
  ( interactive )
  ( let ( ( symbol ( symbol-at-point )))
    ( if ( not symbol ) 
	( message "No symbol at point")
      ( w3m-goto-url Altera-mnl-sdc-path ) )
    (set-buffer "*w3m*")
    (sit-for 0.2)
    (search-forward (symbol-name symbol )) 
    (backward-char 1)
    (w3m-view-this-url)
    )
  )					

(defun Aaron-find-help ( )
  (interactive)
  ( let ( (my-file-name (file-name-extension (buffer-file-name))) )
    (message my-file-name)
    (cond
     ((string= my-file-name "sdc") (Aaron-sdc-find-help))
     ((string= my-file-name "tcl") (Aaron-tcl-find-help)))
    )
  )





(defun my-visit-snippet-file ()
  (interactive)
  (find-file-other-window ( concat ( yas/make-directory-maybe (first  (yas/guess-snippet-directories) ) ) "/" (Aaron-get-word) ) )
)







