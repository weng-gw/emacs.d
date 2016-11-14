;;; fortran-conf.el --- 

;; Copyright 2012 Jin Shusong
;;
;; Author: jin.shusong@gmail.com
;; Version: $Id: fortran-conf.el,v 0.0 2012/10/22 13:27:09 jinss Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'fortran-conf)

;;; Code:

(provide 'fortran-conf)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(require 'f90)
(add-hook 'f90-mode-hook
          '(lambda () (setq f90-do-indent 3
                            f90-if-indent 3
                            f90-type-indent 3
                            f90-program-indent 2
                            f90-continuation-indent 5
                            f90-comment-region "!!$"
                            f90-directive-comment-re "!hpf\\$"
                            f90-indented-comment-re "!"
                            f90-break-delimiters "[-+\\*/,><=% \t]"
                            f90-break-before-delimiters t
                            f90-beginning-ampersand t
                            f90-smart-end 'blink
                            f90-auto-keyword-case 'upcase-word
                            f90-leave-line-no  nil
                            indent-tabs-mode nil
                            f90-font-lock-keywords f90-font-lock-keywords-2
                            outline-regexp  "\\<SUBROUTINE\\>\\|\\<REAL(8) FUNCTION\\>\\|\\<PROGRAM\\>\\|\\<MODULE\\>"
                            )
             ;;The rest is not default.
             (auto-fill-mode 1)
             (abbrev-mode 1)             ; turn on abbreviation mode
             ;;  (f90-add-imenu-menu)        ; extra menu with functions etc.
             (if f90-auto-keyword-case   ; change case of all keywords on startup
                 (f90-change-keywords f90-auto-keyword-case))
             ))
(define-key f90-mode-map "="
  (lambda () (interactive) (insert " = ")))
(define-key f90-mode-map "\C-cq"  'boxes-create-f90)
(define-key f90-mode-map "\C-cr"  'boxes-remove-f90)
(define-key f90-mode-map [f1] 'hide-entry)
(define-key f90-mode-map "\C-c\C-p" 'f90-move-to-prev-entry)
(defun boxes-create-f90 ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -d f90" nil 1 nil))
(defun boxes-remove-f90 ()
  (interactive)
  (shell-command-on-region (region-beginning)
         (region-end) "boxes -r -d f90" nil 1 nil))
(defun f90-move-to-prev-entry ()
  (interactive)
  ((f90-beginning-of-subprogram)
   (previous-line)))
(defun f90-move-to-next-entry ()
  (interactive)
  ('(f90-end-of-subprogram)
   '(next-line)))
;; in your .emacs file (the shown values are the defaults). You can also
;; change the values of the lists f90-keywords etc.
;; The auto-fill and abbreviation minor modes are accessible from the menu,
;; or by using M-x auto-fill-mode and M-x abbrev-mode, respectively.

;; Remarks
;; 1) Line numbers are by default left-justified. If f90-leave-line-no is
;;    non-nil, the line numbers are never touched.
;; 2) Multi-; statements like > do i=1,20 ; j=j+i ; end do < are not handled
;;    correctly, but I imagine them to be rare.
;;--------------------------------------------------------
;; For Fortran  model
;;--------------------------------------------------------
(require 'fortran)
(add-hook 'fortran-mode-hook
          '(lambda ()
             ;;The rest is not default.
             (auto-fill-mode 1)
             ))
(define-key fortran-mode-map "="
  (lambda () (interactive) (insert " = ")))
(define-key fortran-mode-map "\C-cq"  'boxes-create-f90)
(define-key fortran-mode-map "\C-cr"  'boxes-remove-f90)
(defun boxes-create-fortran ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -d f77" nil 1 nil))
(defun boxes-remove-fortran ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -r -d f77" nil 1 nil))
