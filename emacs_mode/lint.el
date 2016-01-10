;######################################################################;
;  NOVAS SOFTWARE CONFIDENTIAL PROPRIETARY NOTE                        ;
;                                                                      ;
;  This software contains information confidential and proprietary     ;
;  to Novas Software Inc. It shall not be reproduced in whole          ;
;  or in part or transferred to other documents, or disclosed          ;
;  to third parties, or used for any purpose other than that           ;
;  for which it was obtained, without the prior written consent        ;
;  of Novas Software Inc.                                              ;
;                                                                      ;
;  (c) 1996-2000 Novas Software Inc.                                   ;
;                                                                      ;
;  All rights reserved                                                 ;
;######################################################################;
;; lint.el --- run lint as inferior of XEmacs, parse error messages.
;; Author: Sam Lu
;; Maintainer: Novas
;; Keywords: tools, processes
;; This file is part of nLint.
;; Synched up with: Novas 19.30.
;; Commentary:
;; Code:

;;;###autoload
(defvar lint-mode-hook nil
  "*List of hook functions run by `lint-mode' (see `run-hooks').")

;;;###autoload
(defvar lint-window-height nil
  "*Number of lines in a lint window.  If nil, use Emacs default.")

(defvar lint-error-list 'invalid ; only valid buffer-local
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a cons (or nil).  Its car is a marker pointing to
an error message.  If its cdr is a marker, it points to the text of the
line the message is about.  If its cdr is a cons, it is a list
\(\(DIRECTORY . FILE\) LINE [COLUMN]\).  Or its cdr may be nil if that
error is not interesting.

The value may be t instead of a list; this means that the buffer of
error messages should be reparsed the next time the list of errors is wanted.

Some other commands (like `diff') use this list to control the error
message tracking facilites; if you change its structure, you should make
sure you also change those packages.  Perhaps it is better not to change
it at all.")

(defvar lint-old-error-list nil
  "Value of `lint-error-list' after errors were parsed.")

(defvar lint-parse-errors-function 'lint-parse-errors 
  "Function to call to parse error messages from a lint.
It takes args LIMIT-SEARCH and FIND-AT-LEAST.
If LIMIT-SEARCH is non-nil, don't bother parsing past that location.
If FIND-AT-LEAST is non-nil, don't bother parsing after finding that 
many new errors.
It should read in the source files which have errors and set
`lint-error-list' to a list with an element for each error message
found.  See that variable for more info.")

;;;###autoload
(defvar lint-buffer-name-function nil
  "Function to compute the name of a lint buffer.
The function receives one argument, the name of the major mode of the
lint buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

;;;###autoload
(defvar lint-finish-function nil
  "*Function to call when a lint process finishes.
It is called with two arguments: the lint buffer, and a string
describing how the process finished.")

(defvar lint-last-buffer nil
  "The most recent lint buffer.
A buffer becomes most recent when its lint is started
or when it is used with \\[next-error] or \\[lint-goto-error].")

(defvar lint-in-progress nil
  "List of lint processes now running.")
(or (assq 'lint-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(lint-in-progress " Linting")
				 minor-mode-alist)))

(defvar lint-always-signal-completion nil
  "Always give an audible signal upon lint completion.
By default that signal is only given if the bottom of the lint
buffer is not visible in its window.")

(defvar lint-parsing-end nil
  "Position of end of buffer when last error messages were parsed.")

(defvar lint-error-message "No more errors"
  "Message to print when no more matches are found.")

(defvar lint-num-errors-found)

(defvar lint-error-regexp-alist
  '(
    ;; NOTE!  See also grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;; or GNU utilities with column (GNAT 1.82):
    ;;   foo.adb:2:1: Unit name does not match file name
    ;; 
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.
    ("\n\
\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)

    ;; Borland C++:
    ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning ping.c 68: Call to function 'func' with no prototype
    ;;("\n\\(Error\\|Warning\\) \\([^:( \t\n]+\\)\
    ;; \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 2 3)

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
    ;;("[ \t:]\\([^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$" 1 2)

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
    ;;("([ \t]*\\([^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)

    ;; Ultrix 3.0 f77:
    ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
    ;; Some SGI cc version:
    ;;  cfe: Warning 835: foo.c, line 2: something
    ;;("\n\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement    
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofel undefined
    ;; Absoft FORTRAN 77 Lintr 3.1.3
    ;;  error on line 19 of fplot.f: spelling error?
    ;;  warning on line 17 of fplot.f: data type is undefined for variable d
    ;;("\\(\n\\|on \\)[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
    ;;of[ \t]+\"?\\([^\":\n]+\\)\"?:" 3 2)

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Unknown lintr:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
    ;; Microtec mcc68k:
    ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
    ;; GNAT (as of July 94): 
    ;;  "foo.adb", line 2(11): warning: file name does not match ...
    ;;("\"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[:., (-]" 1 2)

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ;; DEC AXP OSF/1 cc
    ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah 
    ;;("rror: \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 1 3)

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
    ;;("in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    ;; Lucid Lintr, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
    ;;("\n[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)

    ;; GNU messages with program name and optional column number.
    ;;("\n[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
    ;;\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)

    ;; jwz:
    ;; IRIX 5.2
    ;; cfe: Warning 712: foo.c, line 2: illegal combination of pointer and ...
    ;;(" \\([^ \n,]+\\), line \\([0-9]+\\):" 1 2)
    ;; IRIX 5.2
    ;; cfe: Warning 600: xfe.c: 170: Not in a conditional directive while ...
    ;;(": \\([^ \n,]+\\): \\([0-9]+\\):" 1 2)

    ;; Cray C lintr error messages
    ;;("\n\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,\n]+, \\)* File = \\([^,\n]+\\), Line = \\([0-9]+\\)" 4 5)

    ;; IBM C/C++ Tools 2.01:
    ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
    ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
    ;;  foo.c(5:5) : error EDC0350: Syntax error.
    ;;("\n\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)

    ;; Sun ada (VADS, Solaris):
    ;;  /home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "," inserted
    ;;("\n\\([^, ]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)
    )
  "Alist that specifies how to match errors in lintr output.
Each elt has the form (REGEXP FILE-IDX LINE-IDX [COLUMN-IDX FILE-FORMAT...])
If REGEXP matches, the FILE-IDX'th subexpression gives the file name, and
the LINE-IDX'th subexpression gives the line number.  If COLUMN-IDX is
given, the COLUMN-IDX'th subexpression gives the column number on that line.
If any FILE-FORMAT is given, each is a format string to produce a file name to
try; %s in the string is replaced by the text matching the FILE-IDX'th
subexpression.")

(defvar lint-read-command t
  "If not nil, M-x lint reads the lint command to use.
Otherwise, M-x lint just uses the value of `lint-command'.")

(defvar lint-ask-about-save t
  "If not nil, M-x lint asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking.")

(defvar grep-regexp-alist
  '(("^\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match grep hits.  See `lint-error-regexp-alist'.")

(defvar grep-command "grep -n "
  "Last grep command used in \\[grep]; default for next grep.")

;;;###autoload
(defvar lint-search-path '(nil)
  "*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory.")

(defvar lint-command "nLint -f run.f"
  "Last shell command used to do a lint; default for next lint.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (setq c-mode-hook
      '(lambda () (or (file-exists-p \"run.f\")
		      (progn (make-local-variable 'lint-command)
			     (setq lint-command
				    (concat \"nLint -f \"
					    buffer-file-name))))))")

(defvar lint-enter-directory-regexp
  ": Entering directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate a new current directory.
This must contain one \\(, \\) pair around the directory name.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar lint-leave-directory-regexp
  ": Leaving directory `\\(.*\\)'$"
  "Regular expression matching lines that indicate restoring current directory.
This may contain one \\(, \\) pair around the name of the directory
being moved from.  If it does not, the last directory entered \(by a
line matching `lint-enter-directory-regexp'\) is assumed.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar lint-directory-stack nil
  "Stack of previous directories for `lint-leave-directory-regexp'.
The head element is the directory the lint was started in.")

(defvar lint-exit-message-function nil "\
If non-nil, called when a lint process dies to return a status message.
This should be a function a two arguments as passed to a process sentinel
\(see `set-process-sentinel\); it returns a cons (MESSAGE . MODELINE) of the
strings to write into the lint buffer, and to put in its mode line.")

;; History of lint commands.
(defvar lint-history nil)
;; History of grep commands.
(defvar grep-history nil)

(defconst lint-font-lock-keywords (purecopy
  (list
   '("^[^ \t]+.*\(-*[0-9]+\): Warning [0-9]+: .*\(\\(.+\\)\)$" 1
     gui-button-face t)
   '("^[^ \t]+.*\(-*[0-9]+\): \\(.+\\)\(.+\)$" 1 font-lock-doc-string-face t)
   '("\\(^[^ \t]+.*\\)\(-*[0-9]+\): " 1 blue t)
   '("^[^ \t]+.*\(\\(-*[0-9]+\\)\): " 1 blue t)
   '("^[^ \t]+.*\(-*[0-9]+\): \\(Warning [0-9]+\\): " 1
     font-lock-function-name-face t)
   '("^[^ \t]+.*\(-*[0-9]+\): \\(Error [0-9]+\\): " 1
     font-lock-function-name-face t)
   ))
  "Additional expressions to highlight in Lint mode.")

(put 'lint-mode 'font-lock-defaults
     '(lint-font-lock-keywords t))


;;;###autoload
(defun lint (command)
  "Lint the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*lint*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `lint-read-command' is
non-nil; otherwise uses `lint-command'.  With prefix arg, always prompts.

To run more than one lint at once, start one and rename the
\`*lint*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `lint-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive 
   (if (or lint-read-command current-prefix-arg)
       (list (read-shell-command "Lint command: "
                                 lint-command
                                 ;; #### minibuffer code should do this
                                 (if (equal (car lint-history)
                                            lint-command)
                                     '(lint-history . 1)
                                     'lint-history)))
       (list lint-command)))
  (setq lint-command command)
  (save-some-buffers (not lint-ask-about-save) nil)
  (lint-internal lint-command "No more errors"))

;;; run lint with the default command line
(defun relint ()
  "Re-lint the program including the current buffer."
  (interactive)
  (save-some-buffers (not lint-ask-about-save) nil)
  (lint-internal lint-command "No more errors"))

;; The system null device. (Should reference NULL_DEVICE from C.)
(defvar grep-null-device "/dev/null" "The system null device.")

;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   (list (read-shell-command "Run grep (like this): "
			     grep-command 'grep-history)))
  (let ((buf (lint-internal (concat command-args " " grep-null-device)
			       "No more grep hits" "grep"
			       ;; Give it a simpler regexp to match.
			       nil grep-regexp-alist)))
    (save-excursion
      (set-buffer buf)
      (set (make-local-variable 'lint-exit-message-function)
	   (lambda (proc msg)
	     (let ((code (process-exit-status proc)))
	       (if (eq (process-status proc) 'exit)
		   (cond ((zerop code)
			  '("finished (matches found)\n" . "matched"))
			 ((= code 1)
			  '("finished with no matches found\n" . "no match"))
			 (t
			  (cons msg code)))
		 (cons msg code))))))))

(defun lint-internal (command error-message
				 &optional name-of-mode parser regexp-alist
				 name-function)
  "Run lint command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  Third argument NAME-OF-MODE is the name
to display as the major mode in the lint buffer.

Fourth arg PARSER is the error parser function (nil means the default).  Fifth
arg REGEXP-ALIST is the error message regexp alist to use (nil means the
default).  Sixth arg NAME-FUNCTION is a function called to name the buffer (nil
means the default).  The defaults for these variables are the global values of
\`lint-parse-errors-function', `lint-error-regexp-alist', and
\`lint-buffer-name-function', respectively.

Returns the lint buffer created."
  (let (outbuf)
    (save-excursion
      (or name-of-mode
	  (setq name-of-mode "Lint"))
      (setq outbuf
	    (get-buffer-create
	     (funcall (or name-function lint-buffer-name-function
			  (function (lambda (mode)
				      (concat "*" (downcase mode) "*"))))
		      name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc (get-buffer-process (current-buffer))))
	(if comp-proc
	    (if (or (not (eq (process-status comp-proc) 'run))
		    (yes-or-no-p
		     (format "A %s process is running; kill it? "
			     name-of-mode)))
		(condition-case ()
		    (progn
		      (interrupt-process comp-proc)
		      (sit-for 1)
		      (delete-process comp-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
		     (buffer-name))
	      )))
      ;; In case the lint buffer is current, make sure we get the global
      ;; values of lint-error-regexp-alist, etc.
      (kill-all-local-variables))
    (let ((regexp-alist (or regexp-alist lint-error-regexp-alist))
	  (parser (or parser lint-parse-errors-function))
	  (thisdir default-directory)
	  (buffer-save (current-buffer))
	  outwin)
      
      ;; Pop up the lint buffer.
      (setq outwin (display-buffer outbuf))
      
      (unwind-protect
       (progn
	;; Clear out the lint buffer and make it writable.
	;; Change its default-directory to the directory where the lint
	;; will happen, and insert a `cd' command to indicate this.
	(set-buffer outbuf)
	
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))
	(setq default-directory thisdir)
	(insert "cd " thisdir "\n" command "\n")
	(set-buffer-modified-p nil)

	;; set it so the window will scroll to show lint output
	(save-window-excursion
	  (select-window outwin)
	  (goto-char (point-max)))
	
	(lint-mode name-of-mode)
	;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	(set (make-local-variable 'lint-command) command)
	(set (make-local-variable 'lint-parse-errors-function) parser)
	(set (make-local-variable 'lint-error-message) error-message)
	(set (make-local-variable 'lint-error-regexp-alist) regexp-alist)
	(setq default-directory thisdir
	      lint-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(setq mode-name name-of-mode)
;	(or (eq outwin (selected-window))
;	    (set-window-point outwin (point-min)))
	(lint-set-window-height outwin)

	;; Set up the menus

	;; Start the lint.
	(if (fboundp 'start-process)
	    (let* ((process-environment (cons "EMACS=t" process-environment))
		   (proc (start-process-shell-command (downcase mode-name)
						      outbuf
						      command)))
	      (set-process-sentinel proc 'lint-sentinel)
	      (set-process-filter proc 'lint-filter)
	      (set-marker (process-mark proc) (point) outbuf)
	      (setq lint-in-progress 
		    (cons proc lint-in-progress)))
	  ;; No asynchronous processes available
	  (message (format "Executing `%s'..." command))
	  (sit-for 0) ;; Force redisplay
	  (let ((status (call-process shell-file-name nil outbuf nil "-c"
				      command))))
	  (message (format "Executing `%s'...done" command))))
       (set-buffer buffer-save)))

    ;; Make it so the next C-x ` will use this buffer.
    (setq lint-last-buffer outbuf)))

;; Set the height of WINDOW according to lint-window-height.
(defun lint-set-window-height (window)
  (and lint-window-height
       (= (window-width window) (frame-width (window-frame window)))
       ;; If window is alone in its frame, aside from a minibuffer,
       ;; don't change its height.
       (not (eq window (frame-root-window (window-frame window))))
       ;; This save-excursion prevents us from changing the current buffer,
       ;; which might not be the same as the selected window's buffer.
       (save-excursion
	 (let ((w (selected-window)))
	   (unwind-protect
	       (progn
		 (select-window window)
		 (enlarge-window (- lint-window-height
				    (window-height))))
	     (select-window w))))))

(defvar lint-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(set-keymap-name map 'lint-minor-mode-map)
    (define-key map "\C-c\C-c" 'lint-goto-error)
    (define-key map "\C-m" 'lint-goto-error)
    (define-key map "\C-c\C-k" 'kill-lint)
    (define-key map "\M-n" 'lint-next-error)
    (define-key map "\M-p" 'lint-previous-error)
    (define-key map "\M-{" 'lint-previous-file)
    (define-key map "\M-}" 'lint-next-file)
    map)
  "Keymap for `lint-minor-mode'.")

(defvar lint-mode-map
  (let ((map (make-sparse-keymap)))
    ;;(set-keymap-parents map (list lint-minor-mode-map))
    ;;(set-keymap-name map 'lint-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    ;(define-key map 'button2 'lint-mouse-goto-error)
    map)
  "Keymap for lint log buffers.
`lint-minor-mode-map' is a parent of this.")

;;; XEmacs menus

(defun lint-errors-exist-p (&optional buffer)
  "Whether we are in a state where the `next-error' command will work,
that is, whether there exist (or may exist) error targets in the *lint*
or *grep* buffers."
  (or buffer
      (setq buffer (condition-case nil
			  (lint-find-buffer)
			(error nil))))
  (and buffer
       (lint-buffer-p buffer)
       (save-excursion
	 (set-buffer buffer)
	 ;; Has errors on the list, or needs to be parsed.
	 ;; But don't parse it now!
	 (or (not (null lint-error-list))
	     (< lint-parsing-end (point-max))))))

(defvar Lint-mode-popup-menu
  '("Lint Mode Commands"
    :filter lint-menu-filter
    ["Lint..."	lint t]
    ["Relint"	relint t]
    ["Kill Lint"	kill-lint (get-buffer-process (current-buffer))]
    "---"
    ["Goto Error"	lint-goto-error	(lint-errors-exist-p)]
    ["Next Error" 	next-error		(lint-errors-exist-p)]
    ["Previous Error"	previous-error		(lint-errors-exist-p)]
    ["First Error"	first-error		(lint-errors-exist-p)]
    ))

(defvar Lint-mode-menubar-menu
  (cons "Lint" (cdr Lint-mode-popup-menu)))
  
(defvar grep-mode-popup-menu
  '("Grep Mode Commands"
    :filter grep-menu-filter
    ["Grep..."		grep t]
    ["Repeat Grep"	relint t]
    ["Kill Grep"	kill-lint (get-buffer-process (current-buffer))]
    "---"
    ["Goto Match" lint-goto-error (default-value 'lint-error-list)]
    ["Next Match"	next-error (default-value 'lint-error-list)]
    ["Previous Match"	previous-error (default-value 'lint-error-list)]
    ["First Match"	first-error (default-value 'lint-error-list)]
    ))

(defvar grep-mode-menubar-menu
  (cons "Grep" (cdr grep-mode-popup-menu)))
  
(defun lint-menu-filter-1 (menu history-list item-name command-name)
  (let ((submenu (mapcar #'(lambda (string)
			     (vector string
				     (list command-name string)
				     t))
			  history-list))
	(existing (assoc item-name menu)))
    (if existing
	(progn
	  (setcdr existing submenu)
	  menu)
      (nconc menu (list (cons item-name submenu))))))

(defun lint-menu-filter (menu)
  (lint-menu-filter-1 menu lint-history "Lint History" 'lint))

(defun grep-menu-filter (menu)
  (lint-menu-filter-1 menu grep-history "Grep History" 'grep))

(defun lint-mode (&optional name-of-mode)
  "Major mode for lint log buffers.
\\<lint-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[lint-goto-error],
or click on the line with \\[lint-mouse-goto-error].
There is a menu of commands on \\[lint-popup-menu].
To kill the lint, type \\[kill-lint].

Runs `lint-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lint-mode-map)
  (setq major-mode 'lint-mode
	mode-name "Lint")
  (lint-setup)
  (font-lock-set-defaults)
  (if (not name-of-mode) nil
    (let ((sym (intern (concat name-of-mode "-mode-popup-menu"))))
      (if (boundp sym)
	  (setq mode-popup-menu (symbol-value sym))))
    (if (featurep 'menubar)
	(progn
	  ;; make a local copy of the menubar, so our modes don't
	  ;; change the global menubar
	  (set-buffer-menubar current-menubar)
	  (let ((sym (intern (concat name-of-mode "-mode-menubar-menu"))))
	    (if (boundp sym)
		(add-submenu nil (symbol-value sym)))))))
  (run-hooks 'lint-mode-hook))

;; XEmacs addition, hacked by Mly
(defun lint-mode-motion-hook (event)
  (mode-motion-highlight-internal
    event
    #'beginning-of-line
    #'(lambda ()
        (let* ((p (point))
               (e (progn (end-of-line) (point)))
               (l (progn
                    (if (or (eq lint-error-list 't)
                            (>= p lint-parsing-end))
                        ;; #### Does it suck too badly to have mouse-movement
                        ;; #### over a buffer parse errors in that buffer??
                        (save-window-excursion
                          (lint-reinitialize-errors nil p)))
                    (if (and lint-error-list
                             (<= (car (car lint-error-list)) p))
                        ;; Perhaps save time by only searching tail
                        lint-error-list
                        lint-old-error-list))))
          (if (catch 'found
                (while l
                  (let ((x (marker-position (car (car l)))))
                    (cond ((< x p)
                           (setq l (cdr l)))
                          ((<= x e)
                           (throw 'found t))
                          (t
                           (throw 'found nil)))))
                nil)
              (goto-char e)
              (goto-char p))))))

;; Prepare the buffer for the lint parsing commands to work.
(defun lint-setup ()
  ;; Make the buffer's mode line show process state.
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'lint-error-list) nil)
  (set (make-local-variable 'lint-old-error-list) nil)
  (set (make-local-variable 'lint-parsing-end) 1)
  (set (make-local-variable 'lint-directory-stack) nil)
  (setq lint-last-buffer (current-buffer))
  ;; XEmacs change: highlight lines, install menubar.
  (require 'mode-motion)
  (setq mode-motion-hook 'lint-mode-motion-hook)
  (make-local-variable 'mouse-track-click-hook)
  (add-hook 'mouse-track-click-hook 'lint-mouse-maybe-goto-error)
  )

(defvar lint-minor-mode nil
  "Non-nil when in lint-minor-mode.
In this minor mode, all the error-parsing commands of the
Lint major mode are available.")
(make-variable-buffer-local 'lint-minor-mode)

(or (assq 'lint-minor-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lint-minor-mode " Lint")
				 minor-mode-alist)))
(or (assq 'lint-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'lint-minor-mode
					   lint-minor-mode-map)
				     minor-mode-map-alist)))

;;;###autoload
(defun lint-minor-mode (&optional arg)
  "Toggle lint minor mode.
With arg, turn lint mode on if and only if arg is positive.
See `lint-mode'.
! \\{lint-mode-map}"
  (interactive "P")
  (if (setq lint-minor-mode (if (null arg)
				       (null lint-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (lint-setup)))

;; Called when lint process changes state.
(defun lint-sentinel (proc msg)
  "Sentinel for lint buffers."
  (let* ((buffer (process-buffer proc))
	 (window (get-buffer-window buffer)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer))
		  omax opoint estatus)
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the lint buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (let ((buffer-read-only nil)
			  (status (if lint-exit-message-function
				      (funcall lint-exit-message-function
					       proc msg)
				    (cons msg (process-exit-status proc)))))
		      (setq omax (point-max)
			    opoint (point))
		      (goto-char omax)
		      ;; Record where we put the message, so we can ignore it
		      ;; later on.
		      (insert ?\n mode-name " " (car status))
		      (forward-char -1)
		      (insert " at " (substring (current-time-string) 0 19))
		      (forward-char 1)
		      (setq mode-line-process
			    (concat ":"
				    (symbol-name (process-status proc))
				    (if (zerop (process-exit-status proc))
					" OK"
					(setq estatus
					      (format " [exit-status %d]"
						      (process-exit-status proc))))
				    ))
		      ;; XEmacs - tedium should let you know when it's ended...
		      (if (and (not lint-always-signal-completion)
			       window
			       (pos-visible-in-window-p (point-max) window))
			  nil		; assume that the user will see it...
			(ding t 'ready)
			(message "Lint process completed%s."
				 (or estatus " successfully")
				 ))
		      ;; Since the buffer and mode line will show that the
		      ;; process is dead, we can delete it now.  Otherwise it
		      ;; will stay around until M-x list-processes.
		      (delete-process proc)
		      ;; Force mode line redisplay soon.
		      (redraw-modeline))
		    (if (and opoint (< opoint omax))
			(goto-char opoint))
		    (if lint-finish-function
			(funcall lint-finish-function buffer msg)))
		(set-buffer obuf))))
	  (setq lint-in-progress (delq proc lint-in-progress))
	  ))))

(defun lint-filter (proc string)
  "Process filter for lint buffers.
Just inserts the text, but uses `insert-before-markers'."
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(let ((buffer-read-only nil))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    (set-marker (process-mark proc) (point)))))))

;; Return the cdr of lint-old-error-list for the error containing point.
(defun lint-error-at-point ()
  (lint-reinitialize-errors nil (point))
  (let ((errors lint-old-error-list))
    (while (and errors
		(> (point) (car (car errors))))
      (setq errors (cdr errors)))
    errors))

(defun lint-buffer-p (buffer)
  (save-excursion
    (set-buffer buffer)
    (or lint-minor-mode (eq major-mode 'lint-mode))))

(defun lint-next-error (n)
  "Move point to the next error in the lint buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (or (lint-buffer-p (current-buffer))
      (error "Not in a lint buffer."))
  (setq lint-last-buffer (current-buffer))

  (let ((errors (lint-error-at-point)))

    ;; Move to the error after the one containing point.
    (goto-char (car (if (< n 0)
			(let ((i 0)
			      (e lint-old-error-list))
			  ;; See how many cdrs away ERRORS is from the start.
			  (while (not (eq e errors))
			    (setq i (1+ i)
				  e (cdr e)))
			  (if (> (- n) i)
			      (error "Moved back past first error")
			    (nth (+ i n) lint-old-error-list)))
		      (let ((lint-error-list (cdr errors)))
			(lint-reinitialize-errors nil nil n)
			(if lint-error-list
			    (nth (1- n) lint-error-list)
			  (error "Moved past last error"))))))))

(defun lint-previous-error (n)
  "Move point to the previous error in the lint buffer.
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (lint-next-error (- n)))


;; Given an elt of `lint-error-list', return an object representing
;; the referenced file which is equal to (but not necessarily eq to) what
;; this function would return for another error in the same file.
(defsubst lint-error-filedata (data)
  (setq data (cdr data))
  (if (markerp data)
      (marker-buffer data)
    (car data)))

;; Return a string describing a value from lint-error-filedata.
;; This value is not necessarily useful as a file name, but should be
;; indicative to the user of what file's errors are being referred to.
(defsubst lint-error-filedata-file-name (filedata)
  (if (bufferp filedata)
      (buffer-file-name filedata)
    (car filedata)))

(defun lint-next-file (n)
  "Move point to the next error for a different file than the current one."
  (interactive "p")
  (or (lint-buffer-p (current-buffer))
      (error "Not in a lint buffer."))
  (setq lint-last-buffer (current-buffer))

  (let ((reversed (< n 0))
	errors filedata)

    (if (not reversed)
	(setq errors (or (lint-error-at-point)
			 (error "Moved past last error")))

      ;; Get a reversed list of the errors up through the one containing point.
      (lint-reinitialize-errors nil (point))
      (setq errors (reverse lint-old-error-list)
	    n (- n))

      ;; Ignore errors after point.  (car ERRORS) will be the error
      ;; containing point, (cadr ERRORS) the one before it.
      (while (and errors
		  (< (point) (car (car errors))))
	(setq errors (cdr errors))))

    (while (> n 0)
      (setq filedata (lint-error-filedata (car errors)))

      ;; Skip past the following errors for this file.
      (while (equal filedata
		    (lint-error-filedata
		     (car (or errors
			      (if reversed
				  (error "%s the first erring file"
					 (lint-error-filedata-file-name
					  filedata))
				(let ((lint-error-list nil))
				  ;; Parse some more.
				  (lint-reinitialize-errors nil nil 2)
				  (setq errors lint-error-list)))
			      (error "%s is the last erring file" 
				     (lint-error-filedata-file-name
				      filedata))))))
	(setq errors (cdr errors)))

      (setq n (1- n)))

    ;; Move to the following error.
    (goto-char (car (car (or errors
			     (if reversed
				 (error "This is the first erring file")
			       (let ((lint-error-list nil))
				 ;; Parse the last one.
				 (lint-reinitialize-errors nil nil 1)
				 lint-error-list))))))))

(defun lint-previous-file (n)
  "Move point to the previous error for a different file than the current one."
  (interactive "p")
  (lint-next-file (- n)))


(defun kill-lint ()
  "Kill the process made by the \\[lint] command."
  (interactive)
  (let ((buffer (lint-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The lint process is not running."))))


;; Parse any new errors in the lint buffer,
;; or reparse from the beginning if the user has asked for that.
(defun lint-reinitialize-errors (reparse
                                    &optional limit-search find-at-least)
  (save-excursion
    ;; XEmacs change: Below we made a change to possibly change the
    ;; selected window.  If we don't save and restore the old window
    ;; then if we get an error such as 'no more errors' we'll end up
    ;; in the lint buffer.
    (save-window-excursion
      (set-buffer lint-last-buffer)
      ;; If we are out of errors, or if user says "reparse",
      ;; discard the info we have, to force reparsing.
      (if (or (eq lint-error-list t)
	      reparse)
	  (lint-forget-errors))
      (if (and lint-error-list
	       (or (not limit-search)
		 (> lint-parsing-end limit-search))
	     (or (not find-at-least)
		 (>= (length lint-error-list) find-at-least)))
	;; Since lint-error-list is non-nil, it points to a specific
	;; error the user wanted.  So don't move it around.
	nil

      ;; XEmacs change: if the lint buffer is already visible
      ;; in a window, use that instead of thrashing the display.
      (let ((w (get-buffer-window lint-last-buffer)))
	(if w
	    (select-window w)
	  (switch-to-buffer lint-last-buffer)))

      (set-buffer-modified-p nil)
      (if (< lint-parsing-end (point-max))
	  ;; lint-error-list might be non-nil if we have a non-nil
	  ;; LIMIT-SEARCH or FIND-AT-LEAST arg.  In that case its value
	  ;; records the current position in the error list, and we must
	  ;; preserve that after reparsing.
	  (let ((error-list-pos lint-error-list))
	    (funcall lint-parse-errors-function
		     limit-search
		     (and find-at-least
			  ;; We only need enough new parsed errors to reach
			  ;; FIND-AT-LEAST errors past the current
			  ;; position.
			  (- find-at-least (length lint-error-list))))
	    ;; Remember the entire list for lint-forget-errors.  If
	    ;; this is an incremental parse, append to previous list.  If
	    ;; we are parsing anew, lint-forget-errors cleared
	    ;; lint-old-error-list above.
	    (setq lint-old-error-list
		  (nconc lint-old-error-list lint-error-list))
	    (if error-list-pos
		;; We started in the middle of an existing list of parsed
		;; errors before parsing more; restore that position.
		(setq lint-error-list error-list-pos))
	    ))))))

(defun lint-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a lint log buffer.  Sets the mark at point there.
\\[universal-argument] as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (or (lint-buffer-p (current-buffer))
      (error "Not in a lint buffer."))
  (setq lint-last-buffer (current-buffer))
  (lint-reinitialize-errors (consp argp) (point))

  ;; Move to bol; the marker for the error on this line will point there.
  (beginning-of-line)

  ;; Move lint-error-list to the elt of lint-old-error-list
  ;; we want.
  (setq lint-error-list lint-old-error-list)
  (while (and lint-error-list
	      (> (point) (car (car lint-error-list))))
    (setq lint-error-list (cdr lint-error-list)))

  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (progn
	(other-window -1)
	;; other-window changed the selected buffer,
	;; but we didn't want to do that.
	(set-buffer lint-last-buffer)))

  (push-mark)
  (next-error 1))

;; XEmacs addition
(defun lint-mouse-goto-error (event)
  "Visit the source for the error under the mouse.
Use this command in a lint log buffer."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (lint-goto-error))

;; XEmacs addition
(defun lint-mouse-maybe-goto-error (event &optional click-count)
  (interactive "e")
  (if (equal (event-button event) 2)
      (let ((buffer (current-buffer))
	    (point (point))
	    (config (current-window-configuration)))
	(condition-case nil
	    (progn
	      (lint-mouse-goto-error event)
	      t)
	  (error
	   (set-window-configuration config)
	   (set-buffer buffer)
	   (goto-char point)
	   nil)))))

;; Return a lint buffer.
;; If the current buffer is a lint buffer, return it.
;; If lint-last-buffer is set to a live buffer, use that.
;; Otherwise, look for a lint buffer and signal an error
;; if there are none.
(defun lint-find-buffer (&optional other-buffer)
  (if (and (not other-buffer)
	   (lint-buffer-p (current-buffer)))
      ;; The current buffer is a lint buffer.
      (current-buffer)
    (if (and lint-last-buffer (buffer-name lint-last-buffer)
	     (or (not other-buffer) (not (eq lint-last-buffer
					     (current-buffer)))))
	lint-last-buffer
      (let ((buffers (buffer-list)))
	(while (and buffers (or (not (lint-buffer-p (car buffers)))
				(and other-buffer
				     (eq (car buffers) (current-buffer)))))
	  (setq buffers (cdr buffers)))
	(if buffers
	    (car buffers)
	  (or (and other-buffer
		   (lint-buffer-p (current-buffer))
		   ;; The current buffer is a lint buffer.
		   (progn
		     (if other-buffer
			 (message "This is the only lint buffer."))
		     (current-buffer)))
	      (error "No lint started!")))))))

;;;###autoload
(defun next-error (&optional argp)
  "Visit next lint error message and corresponding source code.
This operates on the output from the \\[lint] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally applies to the most recent lint started,
but as long as you are in the middle of parsing errors from one lint
output buffer, you stay with that lint output buffer.

Use \\[next-error] in a lint output buffer to switch to
processing errors from that lint.

See variables `lint-parse-errors-function' and
\`lint-error-regexp-alist' for customization ideas."
  (interactive "P")
  (setq lint-last-buffer (lint-find-buffer))
  (lint-goto-locus (lint-next-error-locus
			   ;; We want to pass a number here only if
			   ;; we got a numeric prefix arg, not just C-u.
			   (and (not (consp argp))
				(prefix-numeric-value argp))
			   (consp argp))))

;; XEmacs change
;;;###autoload
(defun previous-error (&optional argp)
  "Visit previous lint error message and corresponding source code.
This operates on the output from the \\[lint] command."
  (interactive "P")
  (next-error (cond ((null argp) -1)
		    ((numberp argp) (- argp))
		    (t argp))))

;;;###autoload
(defun first-error ()
  "Reparse the error message buffer and start at the first error
Visit corresponding source code.
This operates on the output from the \\[lint] command."
  (interactive)
  (next-error '(4)))

(defun lint-next-error-locus (&optional move reparse silent)
  "Visit next lint error and return locus in corresponding source code.
This operates on the output from the \\[lint] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

Returns a cons (ERROR . SOURCE) of two markers: ERROR is a marker at the
location of the error message in the lint buffer, and SOURCE is a
marker at the location in the source code indicated by the error message.

Optional first arg MOVE says how many error messages to move forwards (or
backwards, if negative); default is 1.  Optional second arg REPARSE, if
non-nil, says to reparse the error message buffer and reset to the first
error (plus MOVE - 1).  If optional third argument SILENT is non-nil, return 
nil instead of raising an error if there are no more errors.

The current buffer should be the desired lint output buffer."
  (or move (setq move 1))
  (lint-reinitialize-errors reparse nil (and (not reparse)
						(if (< move 1) 0 (1- move))))
  (let (next-errors next-error)
    (catch 'no-next-error
      (save-excursion
	(set-buffer lint-last-buffer)
	;; lint-error-list points to the "current" error.
	(setq next-errors 
	      (if (> move 0)
		  (nthcdr (1- move)
			  lint-error-list)
                ;; Zero or negative arg; we need to move back in the list.
                (let ((n (1- move))
                      (i 0)
                      (e lint-old-error-list))
                  ;; See how many cdrs away the current error is from the start.
                  (while (not (eq e lint-error-list))
                    (setq i (1+ i)
                          e (cdr e)))
                  (if (> (- n) i)
                      (error "Moved back past first error")
		    (nthcdr (+ i n) lint-old-error-list))))
	      next-error (car next-errors))
	(while
	    (if (null next-error)
		(progn
		  (and move (/= move 1)
		       (error (if (> move 0)
				  "Moved past last error"
                                "Moved back past first error")))
;; Forget existing error messages if lint has finished.
;;; XEmacs change by Barry Warsaw.
;;; Without this, if you get a "no more errors" error, then you can't do
;;; previous-error or goto-error until you kill the buffer.
;		  (if (not (and (get-buffer-process (current-buffer))
;				(eq (process-status
;				     (get-buffer-process
;				      (current-buffer)))
;				    'run)))
;		      (lint-forget-errors))
		  (if silent
		      (throw 'no-next-error nil)
		    (error (concat lint-error-message
				   (and (get-buffer-process (current-buffer))
					(eq (process-status (get-buffer-process
							     (current-buffer)))
					    'run)
					" yet")))))
	      (setq lint-error-list (cdr next-errors))
	      (if (null (cdr next-error))
		  ;; This error is boring.  Go to the next.
		  t
		(or (markerp (cdr next-error))
		    ;; This error has a filename/lineno pair.
		    ;; Find the file and turn it into a marker.
		    (let* ((fileinfo (car (cdr next-error)))
			   (cbuf (current-buffer)) ;XEmacs addition
			   (buffer (apply 'lint-find-file
					  (car next-error) fileinfo)))
		      (if (null buffer)
			  ;; We can't find this error's file.
			  ;; Remove all errors in the same file.
			  (progn
			    (setq next-errors lint-old-error-list)
			    (while next-errors
			      (and (consp (cdr (car next-errors)))
				   (equal (car (cdr (car next-errors)))
					  fileinfo)
				   (progn
				     (set-marker (car (car next-errors)) nil)
				     (setcdr (car next-errors) nil)))
			      (setq next-errors (cdr next-errors)))
			    ;; Look for the next error.
			    t)
			;; We found the file.  Get a marker for this error.
			;; lint-old-error-list is a buffer-local
			;; variable, so we must be careful to extract its value
			;; before switching to the source file buffer.
			(let ((errors lint-old-error-list)
			      (last-line (nth 1 (cdr next-error)))
			      (column (nth 2 (cdr next-error))))
			  (set-buffer buffer)
			  (save-excursion
			    (save-restriction
			      (widen)
			      (goto-line last-line)
			      (if (and column (> column 0))
				  ;; Columns in error msgs are 1-origin.
				  (move-to-column (1- column))
				(beginning-of-line))
			      (setcdr next-error (point-marker))
			      ;; Make all the other error messages referring
			      ;; to the same file have markers into the buffer.
			      (while errors
				(and (consp (cdr (car errors)))
				     (equal (car (cdr (car errors))) fileinfo)
				     (let* ((this (nth 1 (cdr (car errors))))
					    (column (nth 2 (cdr (car errors))))
					    (lines (- this last-line)))
				       (if (eq selective-display t)
					   ;; When selective-display is t,
					   ;; each C-m is a line boundary,
					   ;; as well as each newline.
					   (if (< lines 0)
					       (re-search-backward "[\n\C-m]"
								   nil 'end
								   (- lines))
                                             (re-search-forward "[\n\C-m]"
                                                                nil 'end
                                                                lines))
                                         (forward-line lines))
				       (if (and column (> column 1))
					   (move-to-column (1- column))
					 (beginning-of-line))
				       (setq last-line this)
				       (setcdr (car errors) (point-marker))))
				(setq errors (cdr errors)))))
			  ;; XEmacs addition
			  (set-buffer cbuf)))))
                ;; If we didn't get a marker for this error, or this
                ;; marker's buffer was killed, go on to the next one.
                (or (not (markerp (cdr next-error)))
                    (not (marker-buffer (cdr next-error))))))
	  (setq next-errors lint-error-list
		next-error (car next-errors)))

	;; XEmacs -- move this inside save-excursion
	;; Skip over multiple error messages for the same source location,
	;; so the next C-x ` won't go to an error in the same place.
	(while (and lint-error-list
		    (equal (cdr (car lint-error-list))
			   (cdr next-error)))
	  (setq lint-error-list (cdr lint-error-list)))
	))

    ;; XEmacs change: If a new window has to be displayed, select the other
    ;; window to avoid swapping the position of the lint error buffer.
    (and next-error (get-buffer-window (marker-buffer (car next-error)))
         (progn
           (select-window (get-buffer-window (marker-buffer (car next-error))))
           (other-window -1)))
	  
    ;; We now have a marker for the position of the error source code.
    ;; NEXT-ERROR is a cons (ERROR . SOURCE) of two markers.
    next-error))

(defun lint-goto-locus (next-error)
  "Jump to an error locus returned by `lint-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
;; XEmacs: this code is horrendous, and makes windows do all sorts of
;; weird things when you're using separate frames for the lint
;; and source buffer.
;  (if (and (window-dedicated-p (selected-window))
;	   (eq (selected-window) (frame-root-window)))
;      (switch-to-buffer-other-frame (marker-buffer (cdr next-error)))
;    (switch-to-buffer (marker-buffer (cdr next-error))))
;  (goto-char (cdr next-error))
;  ;; If narrowing got in the way of
;  ;; going to the right place, widen.
;  (or (= (point) (marker-position (cdr next-error)))
;      (progn
;        (widen)
;        (goto-char (cdr next-error))))
;
;  ;; Show lint buffer in other window, scrolled to this error.
;  (let* ((pop-up-windows t)
;	 (w (or (get-buffer-window (marker-buffer (car next-error)) 'visible)
;		(display-buffer (marker-buffer (car next-error))))))
;    (set-window-point w (car next-error))
;    (set-window-start w (car next-error))
;    (lint-set-window-height w)))

  (let* ((pop-up-windows t)
	 (lint-buffer (marker-buffer (car next-error)))
	 (source-buffer (marker-buffer (cdr next-error)))
	 ;; make sure lint buffer is visible ...
	 (lint-window
	 ;; Use an existing window if it is in a visible frame.
	  (or (get-buffer-window lint-buffer 'visible)
	      ;; Pop up a window.
	      (display-buffer lint-buffer))))

    ;; now, make the lint buffer **STAY WHERE IT IS** and
    ;; make sure the source buffer is visible

    (select-window lint-window)
    (pop-to-buffer source-buffer)

    ;; now put things aright in the lint window.
    (set-window-point lint-window (car next-error))
    (set-window-start lint-window (car next-error))
    (lint-set-window-height lint-window)

    ;; now put things aright in the source window.

    (set-buffer source-buffer)
    (goto-char (cdr next-error))
    ;; If narrowing got in the way of
    ;; going to the right place, widen.
    (or (= (point) (marker-position (cdr next-error)))
	(progn
	  (widen)
	  (goto-char (cdr next-error))))))

;;;###autoload (define-key ctl-x-map "`" 'next-error)

;; Find a buffer for file FILENAME.
;; Search the directories in lint-search-path.
;; A nil in lint-search-path means to try the
;; current directory, which is passed in DIR.
;; If FILENAME is not found at all, ask the user where to find it.
;; Pop up the buffer containing MARKER and scroll to MARKER if we ask the user.
(defun lint-find-file (marker filename dir &rest formats)
  (or formats (setq formats '("%s")))
  (let ((dirs lint-search-path)
	buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
	;; The file name is absolute.  Use its explicit directory as
	;; the first in the search path, and strip it from FILENAME.
	(setq filename (abbreviate-file-name (expand-file-name filename))
	      dirs (cons (file-name-directory filename) dirs)
	      filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) dir)
	    fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
	(setq name (expand-file-name (format (car fmts) filename) thisdir)
	      buffer (and (file-exists-p name)
			  (find-file-noselect name))
	      fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (or buffer
	;; The file doesn't exist.
	;; Ask the user where to find it.
	;; If he hits C-g, then the next time he does
	;; next-error, he'll skip past it.
	(let* ((pop-up-windows t)
	       (w (display-buffer (marker-buffer marker))))
	  (set-window-point w marker)
	  (set-window-start w marker)
	  (let ((name (expand-file-name
		       (read-file-name
			(format "Find this error in: (default %s) "
				filename)
			dir filename t))))
	    (if (file-directory-p name)
		(setq name (expand-file-name filename name)))
	    (and (file-exists-p name)
		 (find-file-noselect name)))))))

;; Set lint-error-list to nil, and unchain the markers that point to the
;; error messages and their text, so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection, but it is better to
;; do it right away.
(defun lint-forget-errors ()
  (while lint-old-error-list
    (let ((next-error (car lint-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
	  (set-marker (cdr next-error) nil)))
    (setq lint-old-error-list (cdr lint-old-error-list)))
  (setq lint-error-list nil
	lint-directory-stack nil
	lint-parsing-end 1))


(defun count-regexp-groupings (regexp)
  "Return the number of \\( ... \\) groupings in REGEXP (a string)."
  (let ((groupings 0)
	(len (length regexp))
	(i 0)
	c)
    (while (< i len)
      (setq c (aref regexp i)
	    i (1+ i))
      (cond ((= c ?\[)
	     ;; Find the end of this [...].
	     (while (and (< i len)
			 (not (= (aref regexp i) ?\])))
	       (setq i (1+ i))))
	    ((= c ?\\)
	     (if (< i len)
		 (progn
		   (setq c (aref regexp i)
			 i (1+ i))
		   (if (= c ?\))
		       ;; We found the end of a grouping,
		       ;; so bump our counter.
		       (setq groupings (1+ groupings))))))))
    groupings))

(defun lint-parse-errors (limit-search find-at-least)
  "Parse the current buffer as grep, cc or lint error messages.
See variable `lint-parse-errors-function' for the interface it uses."
  (setq lint-error-list nil)
  (message "Parsing error messages...")
  (let (;;text-buffer -- unused
	orig orig-expanded parent-expanded
	regexp enter-group leave-group error-group
	alist subexpr error-regexp-groups
	(found-desired nil)
	(lint-num-errors-found 0))

    ;; Don't reparse messages already seen at last parse.
    (goto-char lint-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(progn
	  (forward-line 2)
	  ;; Move back so point is before the newline.
	  ;; This matters because some error regexps use \n instead of ^
	  ;; to be faster.
	  (forward-char -1)))

    ;; Lint all the regexps we want to search for into one.
    (setq regexp (concat "\\(" lint-enter-directory-regexp "\\)\\|"
			 "\\(" lint-leave-directory-regexp "\\)\\|"
			 "\\(" (mapconcat (function
					   (lambda (elt)
					     (concat "\\(" (car elt) "\\)")))
					  lint-error-regexp-alist
					  "\\|") "\\)"))

    ;; Find out how many \(...\) groupings are in each of the regexps, and set
    ;; *-GROUP to the grouping containing each constituent regexp (whose
    ;; subgroups will come immediately thereafter) of the big regexp we have
    ;; just constructed.
    (setq enter-group 1
	  leave-group (+ enter-group
			 (count-regexp-groupings
			  lint-enter-directory-regexp)
			 1)
	  error-group (+ leave-group
			 (count-regexp-groupings
			  lint-leave-directory-regexp)
			 1))

    ;; Lint an alist (IDX FILE LINE [COL]), where IDX is the number of
    ;; the subexpression for an entire error-regexp, and FILE and LINE (and
    ;; possibly COL) are the numbers for the subexpressions giving the file
    ;; name and line number (and possibly column number).
    (setq alist (or lint-error-regexp-alist
		    (error "lint-error-regexp-alist is empty!"))
	  subexpr (1+ error-group))
    (while alist
      (setq error-regexp-groups (cons (list subexpr
					    (+ subexpr (nth 1 (car alist)))
					    (+ subexpr (nth 2 (car alist)))
                                            ;;#### This is buggy in FSFmacs
					    (let ((col (nth 3 (car alist))))
					      (and col
						   (+ subexpr col))))
				      error-regexp-groups))
      (setq subexpr (+ subexpr 1 (count-regexp-groupings (car (car alist)))))
      (setq alist (cdr alist)))

    ;; Set up now the expanded, abbreviated directory variables
    ;; that lint-abbreviate-directory will need, so we can
    ;; compute them just once here.
    (setq orig (abbreviate-file-name default-directory)
	  orig-expanded (abbreviate-file-name
			 (file-truename default-directory))
	  parent-expanded (abbreviate-file-name
			   (expand-file-name "../" orig-expanded)))

    (while (and (not found-desired)
		;; We don't just pass LIMIT-SEARCH to re-search-forward
		;; because we want to find matches containing LIMIT-SEARCH
		;; but which extend past it.
		(re-search-forward regexp nil t))

      ;; Figure out which constituent regexp matched.
      (cond ((match-beginning enter-group)
	     ;; The match was the enter-directory regexp.
	     (let ((dir
		    (file-name-as-directory
		     (expand-file-name
		      (buffer-substring (match-beginning (+ enter-group 1))
					(match-end (+ enter-group 1)))))))
	       ;; The directory name in the "entering" message
	       ;; is a truename.  Try to convert it to a form
	       ;; like what the user typed in.
	       (setq dir
		     (lint-abbreviate-directory dir orig orig-expanded
						   parent-expanded))
	       (setq lint-directory-stack
		     (cons dir lint-directory-stack))
	       (and (file-directory-p dir)
		    (setq default-directory dir)))

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the leave-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))
	    
	    ((match-beginning leave-group)
	     ;; The match was the leave-directory regexp.
	     (let ((beg (match-beginning (+ leave-group 1)))
		   (stack lint-directory-stack))
	       (if beg
		   (let ((dir
			  (file-name-as-directory
			   (expand-file-name
			    (buffer-substring beg
					      (match-end (+ leave-group
							    1)))))))
		     ;; The directory name in the "leaving" message
		     ;; is a truename.  Try to convert it to a form
		     ;; like what the user typed in.
		     (setq dir
			   (lint-abbreviate-directory dir orig orig-expanded
							 parent-expanded))
		     (while (and stack
				 (not (string-equal (car stack) dir)))
		       (setq stack (cdr stack)))))
	       (setq lint-directory-stack (cdr stack))
	       (setq stack (car lint-directory-stack))
	       (if stack
		   (setq default-directory stack))
	       )

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the enter-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))
	    
	    ((match-beginning error-group)
	     ;; The match was the composite error regexp.
	     ;; Find out which individual regexp matched.
	     (setq alist error-regexp-groups)
	     (while (and alist
			 (null (match-beginning (car (car alist)))))
	       (setq alist (cdr alist)))
	     (if alist
		 (setq alist (car alist))
	       (error "lint-parse-errors: impossible regexp match!"))
	     
	     ;; Extract the file name and line number from the error message.
	     (let ((beginning-of-match (match-beginning 0)) ;looking-at nukes
		   (filename (buffer-substring (match-beginning (nth 1 alist))
					       (match-end (nth 1 alist))))
		   (linenum (string-to-int
			     (buffer-substring
			      (match-beginning (nth 2 alist))
			      (match-end (nth 2 alist)))))
		   (column (and (nth 3 alist)
				(match-beginning (nth 3 alist))
				(string-to-int
				 (buffer-substring
				  (match-beginning (nth 3 alist))
				  (match-end (nth 3 alist)))))))

	       ;; Check for a comint-file-name-prefix and prepend it if
	       ;; appropriate.  (This is very useful for
	       ;; lint-minor-mode in an rlogin-mode buffer.)
	       (and (boundp 'comint-file-name-prefix)
		    ;; If the file name is relative, default-directory will
		    ;; already contain the comint-file-name-prefix (done by
		    ;; lint-abbreviate-directory).
		    (file-name-absolute-p filename)
		    (setq filename (concat comint-file-name-prefix filename)))
	       (setq filename (cons filename (cons default-directory
						   (nthcdr 4 alist))))
				     

	       ;; Locate the erring file and line.
	       ;; Cons a new elt onto lint-error-list,
	       ;; giving a marker for the current lint buffer
	       ;; location, and the file and line number of the error.
	       (save-excursion
		 ;; Save as the start of the error the beginning of the
		 ;; line containing the match unless the match starts at a
		 ;; newline, in which case the beginning of the next line.
		 (goto-char beginning-of-match)
		 (forward-line (if (eolp) 1 0))
		 (let ((this (cons (point-marker)
				   (list filename linenum column))))
		   ;; Don't add the same source line more than once.
		   (if (equal (cdr this) (cdr (car lint-error-list)))
		       nil
		     (setq lint-error-list
			   (cons this
				 lint-error-list))
		     (setq lint-num-errors-found
			   (1+ lint-num-errors-found)))))
	       (and (or (and find-at-least (> lint-num-errors-found
					      find-at-least))
			(and limit-search (>= (point) limit-search)))
		    ;; We have found as many new errors as the user wants,
		    ;; or past the buffer position he indicated.  We
		    ;; continue to parse until we have seen all the
		    ;; consecutive errors in the same file, so the error
                    ;; positions will be recorded as markers in this buffer
                    ;; that might change.
		    (cdr lint-error-list) ; Must check at least two.
		    (not (equal (car (cdr (nth 0 lint-error-list)))
				(car (cdr (nth 1 lint-error-list)))))
		    (progn
		      ;; Discard the error just parsed, so that the next
		      ;; parsing run can get it and the following errors in
		      ;; the same file all at once.  If we didn't do this, we
		      ;; would have the same problem we are trying to avoid
		      ;; with the test above, just delayed until the next run!
		      (setq lint-error-list
			    (cdr lint-error-list))
		      (goto-char beginning-of-match)
		      (setq found-desired t)))
	       )
	     )
	    (t
	     (error "lint-parse-errors: known groups didn't match!")))

      (message "Parsing error messages...%d (%d%% of buffer)"
	       lint-num-errors-found
	       (/ (* 100 (point)) (point-max)))

      (and limit-search (>= (point) limit-search)
	   ;; The user wanted a specific error, and we're past it.
	   (setq found-desired t)))
    (setq lint-parsing-end (if found-desired
				      (point)
				    ;; We have searched the whole buffer.
				    (point-max))))
  (setq lint-error-list (nreverse lint-error-list))
  (message "Parsing error messages...done"))

;; If directory DIR is a subdir of ORIG or of ORIG's parent,
;; return a relative name for it starting from ORIG or its parent.
;; ORIG-EXPANDED is an expanded version of ORIG.
;; PARENT-EXPANDED is an expanded version of ORIG's parent.
;; Those two args could be computed here, but we run faster by
;; having the caller compute them just once.
(defun lint-abbreviate-directory (dir orig orig-expanded parent-expanded)
  ;; Apply canonical abbreviations to DIR first thing.
  ;; Those abbreviations are already done in the other arguments passed.
  (setq dir (abbreviate-file-name dir))

  ;; Check for a comint-file-name-prefix and prepend it if appropriate.
  ;; (This is very useful for lint-minor-mode in an rlogin-mode
  ;; buffer.)
  (if (boundp 'comint-file-name-prefix)
      (setq dir (concat comint-file-name-prefix dir)))

  (if (and (> (length dir) (length orig-expanded))
	   (string= orig-expanded
		    (substring dir 0 (length orig-expanded))))
      (setq dir
	    (concat orig
		    (substring dir (length orig-expanded)))))
  (if (and (> (length dir) (length parent-expanded))
	   (string= parent-expanded
		    (substring dir 0 (length parent-expanded))))
    (setq dir
	  (concat (file-name-directory
		   (directory-file-name orig))
		  (substring dir (length parent-expanded)))))
  dir)


(provide 'lint)

;;; lint.el ends here
