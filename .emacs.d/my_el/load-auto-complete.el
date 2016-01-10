;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/emacs_mode//ac-dict")
(ac-config-default)
(defmacro ac-define-dictionary-source (name list)
  "Define dictionary source named `NAME'.
`LIST' is a list of string.
This is useful if you just want to define a dictionary/keywords source."
  `(defvar ,name
     '((candidates . (lambda () (all-completions ac-prefix ,list))))))
(require 'auto-complete-verilog)

(global-auto-complete-mode 1)
;(auto-complete-mode)
(apply-define-key
   ac-complete-mode-map
   `(("<return>"   nil)
     ("RET"        nil)
     ("<return>" ac-complete)
     ("C-n"        ac-next)
     ("C-p"        ac-previous)))









