;;; company-vivado-tcl.el --- company-mode completion back-end for Yasnippet

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Dmitry Gutov

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(require 'xtcl-shell-mode)

;; (defun company-vivado-tcl-option-p ()
;;   ( if (string= (buffer-substring (point) (save-excursion (backward-char)
;;                                                 (point)))

;;   )

(defun company-vivado-tcl--candidates (prefix )
  (message (concat "the prefix is" prefix) )
  (if (not (equal tcl-help-directory-list tcl-help-saved-dirs))
	  (tcl-reread-help-files))
  ( let ( (alist nil) )
    (dolist ( key tcl-help-alist  alist ) 
      (if  (string-prefix-p prefix (car key) )
	  (push (car key) alist )
      )
      
    )
  
    ;(list "a" "b")
)
)

;;;###autoload
(defun company-vivado-tcl (command &optional arg &rest ignore)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-vivado-tcl))
    (prefix
     ;; Should probably use `yas--current-key', but that's bound to be slower.
     ;; How many trigger keys start with non-symbol characters anyway?
     (and (eq major-mode 'xtcl-shell-mode)
          (company-grab-symbol)))

    ;; (annotation
    ;;  (concat
    ;;   (unless company-tooltip-align-annotations " -> ")
    ;;   (get-text-property 0 'yas-annotation arg)))
    
    ;; (doc-buffer (let ((symbol (intern arg)))
    ;;               (save-window-excursion
    ;;                 (ignore-errors
    ;;                   ;; (cond
    ;;                   ;;  ((fboundp symbol) (describe-function symbol))
    ;;                   ;;  ((boundp symbol) (describe-variable symbol))
    ;;                   ;;  ((featurep symbol) (describe-package symbol))
    ;;                   ;;  ((facep symbol) (describe-face symbol))
    ;;                   ;;  (t (signal 'user-error nil)))
    ;;                   (message "just a test")
    ;;                 (help-buffer)))))
    
    (candidates (company-vivado-tcl--candidates arg))
    ;(candidates '("a" "b"))
    ;; (post-completion
    ;;  (let ((template (get-text-property 0 'yas-template arg)))
    ;;    (yas-expand-snippet (yas--template-content template)
    ;;                        (- (point) (length arg))
    ;;                        (point)
    ;;                        (yas--template-expand-env template))))
))

(provide 'company-vivado-tcl)
;;; company-yasnippet.el ends here
