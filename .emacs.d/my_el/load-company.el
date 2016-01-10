(setq load-path (cons "~/.emacs.d/emacs_mode/company-mode/" load-path))
(autoload 'company-mode "company" nil t)

(company-mode t)

(global-company-mode '( Emacs-Lisp-mode verilog-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making the company and yasnippet work together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'company-yasnippet)
;; (add-to-list 'company-backends 'company-yasnippet)
(setq company-minimum-prefix-length 2)



 
;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (if (yas/expansion-at-point)
;;       (progn (company-abort)
;; 	     (yas/expand))
;;     (company-complete-common)))
 
;; (defun yas/expansion-at-point ()
;; "Tested with v0.6.1. Extracted from `yas/expand-1'"
;; (first (yas/current-key)))
;; (setq global-comany-modes nil)


(defun company-abort-and-goto-next-line ()
  (interactive)
  (company-abort)
  (next-line)
)
(defun company-abort-and-goto-prev-line ()
  (interactive)
  (company-abort)
  (previous-line)
)
  

;(global-set-key "\t" 'company-complete-common)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-n") 'company-abort-and-goto-next-line)
(define-key company-active-map (kbd "M-p") 'company-abort-and-goto-prev-line)
(define-key company-active-map (kbd "S-<SPC>") 'company-yasnippet)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)


;;bind company and bbyac
(setq load-path (cons "~/.emacs.d/emacs_mode/bbyac/" load-path))

(require 'bbyac)
(bbyac-global-mode 1)
(defun bbyac-company (&optional matcher buffer-filter)
  "用来作为company的补全"
  (interactive)
  (bbyac--clear-vars)
  (setq matcher (or matcher #'bbyac--matcher)
    buffer-filter (or buffer-filter #'bbyac--buffer-filter))
  (let* ((the-regexp (bbyac--symbol-bbyac-extracter))
     (case-fold-search (not bbyac--contains-upcase))
     matches
     match)
    (when (and the-regexp
           (setq matches (bbyac--get-matches the-regexp matcher buffer-filter)))
      (print matches)
      ;;(setq match (bbyac--display-matches matches))
      )))


(defun company-bbyac--prefix ()
  "用于执行prefix命令"
  (let ((prefix (company-grab-symbol)))
    (if (and prefix)
          prefix
      'stop)))


(defun company-bbyac-candidates (prefix)
  "获得补全数据集合"                    ; 就这么简单吗？
  (chong-debug (message "bbyac completing..."))
  (let ((ret (bbyac-company)))
    (if ret ret 0)
    ))


;;;###autoload
(defun company-bbyac (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Bbyac."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-bbyac))
    (prefix (and ;;(derived-mode-p 'emacs-lisp-mode 'inferior-emacs-lisp-mode)
             ;;(message "bbyac prefix")
             (if (company-in-string-or-comment)
                 nil
              (company-bbyac--prefix))  ;; 用来测试是否可以用bbyac的
             ))
    (candidates (company-bbyac-candidates arg)) ;; 获得补全数据
    ))








