;;-*-coding: utf-8;-*-
(define-abbrev-table 'Buffer-menu-mode-abbrev-table '())

(define-abbrev-table 'Info-edit-mode-abbrev-table '())

(define-abbrev-table 'bookmark-bmenu-mode-abbrev-table '())

(define-abbrev-table 'bookmark-edit-annotation-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-edit-mode-abbrev-table '())

(define-abbrev-table 'browse-kill-ring-mode-abbrev-table '())

(define-abbrev-table 'comint-mode-abbrev-table '())

(define-abbrev-table 'completion-list-mode-abbrev-table '())

(define-abbrev-table 'csv-mode-abbrev-table '())

(define-abbrev-table 'dsssl-mode-abbrev-table '())

(define-abbrev-table 'dts-mode-abbrev-table '())

(define-abbrev-table 'edebug-eval-mode-abbrev-table '())

(define-abbrev-table 'edit-abbrevs-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-byte-code-mode-abbrev-table '())

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '())

(define-abbrev-table 'fundamental-mode-abbrev-table '())

(define-abbrev-table 'fvwm-mode-abbrev-table '())

(define-abbrev-table 'global-abbrev-table '())

(define-abbrev-table 'grep-mode-abbrev-table '())

(define-abbrev-table 'helm-grep-mode-abbrev-table '())

(define-abbrev-table 'helm-moccur-mode-abbrev-table '())

(define-abbrev-table 'help-mode-abbrev-table '())

(define-abbrev-table 'ibuffer-mode-abbrev-table '())

(define-abbrev-table 'image-dired-display-image-mode-abbrev-table '())

(define-abbrev-table 'image-dired-thumbnail-mode-abbrev-table '())

(define-abbrev-table 'inferior-lisp-mode-abbrev-table '())

(define-abbrev-table 'inferior-tcl-mode-abbrev-table '())

(define-abbrev-table 'lisp-mode-abbrev-table '())

(define-abbrev-table 'makefile-automake-mode-abbrev-table '())

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-gmake-mode-abbrev-table '())

(define-abbrev-table 'makefile-imake-mode-abbrev-table '())

(define-abbrev-table 'makefile-makepp-mode-abbrev-table '())

(define-abbrev-table 'makefile-mode-abbrev-table '())

(define-abbrev-table 'matlab-mode-abbrev-table '())

(define-abbrev-table 'matlab-shell-help-mode-abbrev-table '())

(define-abbrev-table 'matlab-shell-topic-mode-abbrev-table '())

(define-abbrev-table 'messages-buffer-mode-abbrev-table '())

(define-abbrev-table 'occur-edit-mode-abbrev-table '())

(define-abbrev-table 'occur-mode-abbrev-table '())

(define-abbrev-table 'package-menu-mode-abbrev-table '())

(define-abbrev-table 'process-menu-mode-abbrev-table '())

(define-abbrev-table 'prog-mode-abbrev-table '())

(define-abbrev-table 'sawfish-console-mode-abbrev-table '())

(define-abbrev-table 'sawfish-mode-abbrev-table '())

(define-abbrev-table 'scheme-mode-abbrev-table '())

(define-abbrev-table 'select-tags-table-mode-abbrev-table '())

(define-abbrev-table 'shell-mode-abbrev-table '())

(define-abbrev-table 'snippet-mode-abbrev-table '())

(define-abbrev-table 'special-mode-abbrev-table '())

(define-abbrev-table 'tabulated-list-mode-abbrev-table '())

(define-abbrev-table 'tcl-mode-abbrev-table '())

(define-abbrev-table 'text-mode-abbrev-table '())

(define-abbrev-table 'url-cookie-mode-abbrev-table '())

(define-abbrev-table 'vc-svn-log-view-mode-abbrev-table '())

(define-abbrev-table 'verilog-mode-abbrev-table
  '(
   ))

(define-abbrev-table 'vhdl-mode-abbrev-table
  '(
    ("--" "" vhdl-template-display-comment-hook 0)
    ("abs" "" vhdl-template-default-hook 0)
    ("access" "" vhdl-template-default-hook 0)
    ("after" "" vhdl-template-default-hook 0)
    ("alias" "" vhdl-template-alias-hook 0)
    ("all" "" vhdl-template-default-hook 0)
    ("and" "" vhdl-template-default-hook 0)
    ("arch" "" vhdl-template-architecture-hook 0)
    ("architecture" "" vhdl-template-architecture-hook 0)
    ("array" "" vhdl-template-default-hook 0)
    ("assert" "" vhdl-template-assert-hook 0)
    ("attr" "" vhdl-template-attribute-hook 0)
    ("attribute" "" vhdl-template-attribute-hook 0)
    ("begin" "" vhdl-template-default-indent-hook 0)
    ("block" "" vhdl-template-block-hook 0)
    ("body" "" vhdl-template-default-hook 0)
    ("buffer" "" vhdl-template-default-hook 0)
    ("bus" "" vhdl-template-default-hook 0)
    ("case" "" vhdl-template-case-hook 0)
    ("comp" "" vhdl-template-component-hook 0)
    ("component" "" vhdl-template-component-hook 0)
    ("cond" "" vhdl-template-conditional-signal-asst-hook 0)
    ("conditional" "" vhdl-template-conditional-signal-asst-hook 0)
    ("conf" "" vhdl-template-configuration-hook 0)
    ("configuration" "" vhdl-template-configuration-hook 0)
    ("cons" "" vhdl-template-constant-hook 0)
    ("constant" "" vhdl-template-constant-hook 0)
    ("disconnect" "" vhdl-template-disconnect-hook 0)
    ("downto" "" vhdl-template-default-hook 0)
    ("else" "" vhdl-template-else-hook 0)
    ("elseif" "" vhdl-template-elsif-hook 0)
    ("elsif" "" vhdl-template-elsif-hook 0)
    ("end" "" vhdl-template-default-indent-hook 0)
    ("entity" "" vhdl-template-entity-hook 0)
    ("exit" "" vhdl-template-exit-hook 0)
    ("file" "" vhdl-template-file-hook 0)
    ("for" "" vhdl-template-for-hook 0)
    ("func" "" vhdl-template-function-hook 0)
    ("function" "" vhdl-template-function-hook 0)
    ("generic" "" vhdl-template-generic-hook 0)
    ("group" "" vhdl-template-group-hook 0)
    ("guarded" "" vhdl-template-default-hook 0)
    ("if" "" vhdl-template-if-hook 0)
    ("impure" "" vhdl-template-default-hook 0)
    ("in" "" vhdl-template-default-hook 0)
    ("inertial" "" vhdl-template-default-hook 0)
    ("inout" "" vhdl-template-default-hook 0)
    ("inst" "" vhdl-template-instance-hook 0)
    ("instance" "" vhdl-template-instance-hook 0)
    ("is" "" vhdl-template-default-hook 0)
    ("label" "" vhdl-template-default-hook 0)
    ("library" "" vhdl-template-library-hook 0)
    ("linkage" "" vhdl-template-default-hook 0)
    ("literal" "" vhdl-template-default-hook 0)
    ("loop" "" vhdl-template-bare-loop-hook 0)
    ("map" "" vhdl-template-map-hook 0)
    ("mod" "" vhdl-template-default-hook 0)
    ("nand" "" vhdl-template-default-hook 0)
    ("new" "" vhdl-template-default-hook 0)
    ("next" "" vhdl-template-next-hook 0)
    ("nor" "" vhdl-template-default-hook 0)
    ("not" "" vhdl-template-default-hook 0)
    ("null" "" vhdl-template-default-hook 0)
    ("of" "" vhdl-template-default-hook 0)
    ("on" "" vhdl-template-default-hook 0)
    ("open" "" vhdl-template-default-hook 0)
    ("or" "" vhdl-template-default-hook 0)
    ("others" "" vhdl-template-others-hook 0)
    ("out" "" vhdl-template-default-hook 0)
    ("pack" "" vhdl-template-package-hook 0)
    ("package" "" vhdl-template-package-hook 0)
    ("port" "" vhdl-template-port-hook 0)
    ("postponed" "" vhdl-template-default-hook 0)
    ("procedure" "" vhdl-template-procedure-hook 0)
    ("process" "" vhdl-template-process-hook 0)
    ("protected" "" vhdl-template-default-hook 0)
    ("pure" "" vhdl-template-default-hook 0)
    ("range" "" vhdl-template-default-hook 0)
    ("record" "" vhdl-template-default-hook 0)
    ("register" "" vhdl-template-default-hook 0)
    ("reject" "" vhdl-template-default-hook 0)
    ("rem" "" vhdl-template-default-hook 0)
    ("report" "" vhdl-template-report-hook 0)
    ("return" "" vhdl-template-return-hook 0)
    ("rol" "" vhdl-template-default-hook 0)
    ("ror" "" vhdl-template-default-hook 0)
    ("select" "" vhdl-template-selected-signal-asst-hook 0)
    ("severity" "" vhdl-template-default-hook 0)
    ("shared" "" vhdl-template-default-hook 0)
    ("sig" "" vhdl-template-signal-hook 0)
    ("signal" "" vhdl-template-signal-hook 0)
    ("sla" "" vhdl-template-default-hook 0)
    ("sll" "" vhdl-template-default-hook 0)
    ("sra" "" vhdl-template-default-hook 0)
    ("srl" "" vhdl-template-default-hook 0)
    ("subtype" "" vhdl-template-subtype-hook 0)
    ("then" "" vhdl-template-default-hook 0)
    ("to" "" vhdl-template-default-hook 0)
    ("transport" "" vhdl-template-default-hook 0)
    ("type" "" vhdl-template-type-hook 0)
    ("unaffected" "" vhdl-template-default-hook 0)
    ("units" "" vhdl-template-default-hook 0)
    ("until" "" vhdl-template-default-hook 0)
    ("use" "" vhdl-template-use-hook 0)
    ("var" "" vhdl-template-variable-hook 0)
    ("variable" "" vhdl-template-variable-hook 0)
    ("wait" "" vhdl-template-wait-hook 0)
    ("when" "" vhdl-template-when-hook 0)
    ("while" "" vhdl-template-while-loop-hook 0)
    ("with" "" vhdl-template-with-hook 0)
    ("xnor" "" vhdl-template-default-hook 0)
    ("xor" "" vhdl-template-default-hook 0)
   ))

(define-abbrev-table 'xtcl-shell-mode-abbrev-table '())
