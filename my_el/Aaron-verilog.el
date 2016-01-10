;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define a function to instantialize a module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Aaron-verilog-inst-module ( ) 
"The function will instantializing a module , which name is the same as the 
current word"
(interactive)
( let ( b1 b2 )  
  ( skip-chars-backward "_A-Za-z0-9" )
					;( skip-chars-backward "_A-Za-z0-9" )
      ;( skip-chars-backward "_A-Za-z0-9" )
  ( setq b1 ( point ) )
  ( skip-chars-forward "_A-Za-z0-9" )
  ( setq b2 ( point ) )
  ( insert " " )
  ( insert ( concat "U_" ( upcase ( buffer-substring-no-properties b1 b2 ) ) ) ) 
  ( insert "( /*AUTOINST*/" )
  ( insert "\n" )
  ( forward-line -1 )
  ( electric-verilog-tab )
  ( forward-line  1 )
  ( insert ");" )
  ( forward-line 0 )
  ( electric-verilog-tab )
  )
) 


(defun Aaron-verilog-inst-module-with-param ( ) 
"The function will instantializing a module with param  , which name is the same as the 
current word"
(interactive)
( let ( b1 b2 )  
  ( skip-chars-backward "_A-Za-z0-9" )
					;( skip-chars-backward "_A-Za-z0-9" )
      ;( skip-chars-backward "_A-Za-z0-9" )
  ( setq b1 ( point ) )
  ( skip-chars-forward "_A-Za-z0-9" )
  ( setq b2 ( point ) )
  ( insert " " )
  ( insert "#(/*AUTOINSTPARAM*/\n" )
  ( insert ( concat ") U_" ( upcase ( buffer-substring-no-properties b1 b2 ) ) ) ) 
  ( insert "( /*AUTOINST*/" )
  ( insert "\n" )
  ( forward-line -1 )
  ( electric-verilog-tab )
  ( forward-line  1 )
  ( insert ");" )
  ( forward-line 0 )
  ( electric-verilog-tab )
  )
) 


;; (defun Aaron-verilog-remove-module-signal( )
;;   "The function will delete the txt between the ( and ).  "
;;   (interactive)
;;   ( let( b1 )
;;     (search-forward "(")
;;     (setq b1 (point) )
;;     (search-forward ")")
;;     (backward-char 1 )
;;     (delete-region b1 (point))
;;   )
;; )


(defun Aaron-verilog-remove-module-signal()
  "The function will delete the txt between the ( and ).  "
  (interactive)
  ( while ( < (save-excursion ( search-forward "(" ) ) (save-excursion ( search-forward ";" ) ) )
    ( let( b1 )
      (search-forward "(")
      (setq b1 (point) )
      (search-forward ")")
      (backward-char 1 )
      (delete-region b1 (point))
      )
    )
)
    
(defun Aaron-verilog-insert-file-name ()
  ""
  (interactive)
  (insert ( file-name-sans-extension (file-name-nondirectory (buffer-file-name) ) ) )
)
(defun Aaron-verilog-find-word()
  "the function will find the word"
  ( let ( b1 b2 )
    ( skip-chars-backward "_A-Za-z0-9" )
    ( setq b1 ( point ) )
    ( skip-chars-forward "_A-Za-z0-9" )
    ( setq b2 ( point ) )
    ( buffer-substring-no-properties b1 b2 ) )
)

(defun Aaron-verilog-find-current-tag( )
  "find the tags where the point is"
  (interactive)
  ( find-tag (Aaron-verilog-find-word) )
)

(defun Aaron-anything-find-occur ( )
""
(interactive)
(anything 'anything-c-source-occur (Aaron-verilog-find-word ) )
)

(defun Aaron-tags-apropos ( )
""
(interactive)
(tags-apropos (Aaron-verilog-find-word ) )
)

(defun Aaron-verilog-cw-occur( )
  ""
  (interactive)
  (occur-1 (Aaron-get-word) 
	   1
	   (remove-if (lambda (buf)
			(set-buffer buf)
			(not (string=  major-mode "verilog-mode" )))
		      (buffer-list))
	   )
)
(defun Aaron-verilog-occur( regexp )
  ""
  (interactive ( list ( read-string "Regexp: ") ) )
  (occur-1 regexp
	   1
	   (remove-if (lambda (buf)
			(set-buffer buf)
			(not (string=  major-mode "verilog-mode" )))
		      (buffer-list))
	   )
)
  

;; (defun Aaron-verilog-indent-whole-file()
;;   ""
;;   (goto-line 1)
;;   (move-beginning-of-line)
;;   (electric-verilog-tab)
;;   (next-line 
;;   )



(defun Aaron-verilog-generate-tb () 
  "Call the gen_tb.pl to generate A TB for current file and open it"
  (interactive)
  ;( call-process "gen_tb.pl" nil "*Messages*" nil (buffer-file-name) )
  ( find-file ( shell-command-to-string (concat "~/bin/gen_tb.pl " (buffer-file-name)) ) )
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;set the compile-mode for the verilog module
(defun Aaron-verilog-sim-module ( &optional cmd )
  (interactive)
  "define a function to compile the verilog module"
(progn
  (let ( tb-name ( file-name-sans-extension ( file-relative-name ( buffer-file-name )) ) ) 
    ( set-buffer "UnitMakefile")
    (compile ( concat "make -k -f UnitMakefile " tb-name  ) )
    )
)
)

(global-set-key [f11] 'Aaron-verilog-sim-module )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Get the system verilog class name

(defun Aaron-verilog-get-class-name ()
  ( let ( oldpoint class-name ) 
    (save-excursion 
      (verilog-re-search-backward "class"  1  'NOERROR )
      (forward-word 1)
      (forward-char 1)
      (setq oldpoint (point) )
      (forward-word 1)
      (setq class-name (buffer-substring oldpoint (point)  ) )
      )
  )
) 


(defun Aaron-verilog-class-occur( )
  "Call occur to disp the class in the files"
  (interactive)
  (anything 'anything-c-source-occur "\\(\\W\\|^\\)class " )
)

(defun Aaron-verilog-function-occur () 
  (interactive)
  (anything 'anything-c-source-occur "")
  )

(defun Aaron-verilog-declaration-occur ()
  (interactive)
  (anything 'anything-c-source-occur "\\(\\W+\\|^\\)\\(bit\\|defparam\\|event\\|genvar\\|inout\\|input\\|integer\\|localparam\\|logic\\|output\\|parameter\\|real\\|realtime\\|reg\\|struct\\|time\\|typedef\\|vectored\\)\\W.*;" )
)

(defun Aaron-verilog-port-occur ()
  (interactive)
  (anything 'anything-c-source-occur "\\(\\W+\\|^\\)\\(inout\\|input\\|output\\)\\W.*;" )
)

(defun Aaron-verilog-entity-in-file-occur ()
  (interactive)
  (anything 'anything-c-source-occur "\\(\\W\\|^\\)\\(class\\|function\\|task\\)\\W")
  )

(defun Aaron-verilog-module-inst( )
  "Call occur to disp the module inst in the files"
  (interactive)
  (anything 'anything-c-source-occur "\\WU_.*(" )
)

(defun Aaron-verilog-inst-occur( )
  "Call occur to disp the inst in the files"
  (interactive)
  (anything 'anything-c-source-occur "\\(\\W\\|^\\)\\w+\\W+\\w+\\W*(" )
)

(defun Aaron-verilog-open-novas()
  "Open the file in novas"
  (interactive)
  )




;We define the command string sent to the novas

(defvar novas-server-port 30000)


(defun Aaron-verilog-novas-open()
  "Open the file in novas"
  (interactive)
  ( message 
    ( shell-command-to-string
      ( concat
	"verdiconnect "
	"\"" 
	"emacs_novas_open "  (buffer-file-name ) " " (number-to-string (line-number-at-pos) )
	"\"" " " 
	(number-to-string novas-server-port ) )
      )
    )
)

(defun Aaron-verilog-novas-reload ()
  "reload the project"
  (interactive)
  ( message 
    ( shell-command-to-string
      ( concat
	"verdiconnect "
	"\"" 
	"debReload " 
	"\"" " " 
	(number-to-string novas-server-port ) )
      )
    )
)

(defun Aaron-verilog-novas-raise-window ()
  "reload the project"
  (interactive)
  ( message 
    ( shell-command-to-string
      ( concat
	"verdiconnect "
	"\"" 
	"srcRaiseWindow "
	"\"" " " 
	(number-to-string novas-server-port ) )
      )
    )
)

(defun Aaron-verilog-novas-goto-definition ()
  "Open the file in novas"
  (interactive)
  ( message 
    ( shell-command-to-string
      ( concat
	"verdiconnect "
	"\"" 
	"emacs_novas_goto_definition "  (buffer-file-name ) " " (number-to-string (line-number-at-pos) ) " " (symbol-name (symbol-at-point))
	"\"" " " 
	(number-to-string novas-server-port ) )
      )
    )
)
;(defun )

;(add-to-list 'ac-modes 'verilog-mode)
