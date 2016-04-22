;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;load the python rope 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/.emacs.d/emacs_mode/python/pymacs" load-path))
(setq load-path (cons "~/.emacs.d/emacs_mode/pysmell/" load-path))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH")
			     ":" (expand-file-name "~/.emacs.d/emacs_mode/python/pymacs")))
(setenv "PYTHONPATH" (concat (getenv "PYTHONPATH")
			     ":" (expand-file-name "~/.emacs.d/emacs_mode/python/ropemacs")))
(setenv "PYMACS_PYTHON" "python2.7") 
;(require 'pymacs)
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  )


(add-hook 'python-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) 
		   '(company-yasnippet  
		     company-bbdb
		      company-cmake
		      company-capf
		      ;company-pysmell
		      (company-dabbrev-code company-gtags company-etags company-keywords)
		      company-files company-dabbrev) )))


(global-set-key "\C-xpl" 'load-ropemacs)
