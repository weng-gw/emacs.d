;;; cc-conf.el --- 

;; Copyright 2012 Jin Shusong
;;
;; Author: jin.shusong@gmail.com
;; Version: $Id: cc-conf.el,v 0.0 2012/10/22 13:21:19 jinss Exp $
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
;;   (require 'cc-conf)

;;; Code:

(provide 'cc-conf)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################
(require 'cc-mode)
;;(require 'filladapt)
(c-set-offset 'substatement-open 0)
(setq c-offsets-alist '((member-init-intro . ++)))
(setq c-default-style "gnu")
(setq c-hanging-braces-alist  '((substatement-open after)
                               (brace-list-open)))
(setq c-hanging-colons-alist  '((member-init-intro before)
                                (inher-intro)
                                (case-label after)
                                (label after)
                                (access-label after)))
(setq c-cleanup-list           '(scope-operator
                                 brace-else-brace
                                 brace-elseif-brace
                                 empty-defun-braces
                                 defun-close-semi))
(setq c-echo-syntactic-information-p t)
;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  (hs-minor-mode 1)
  (c-toggle-auto-hungry-state 1)
  (c-setup-filladapt)
  (filladapt-mode 1)
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-map "\C-c q" 'boxes-create-c)
  (define-key c-mode-map "\C-c r" 'boxes-remove-c)
  (define-key c-mode-map  (kbd "<f9> h") 'hs-hide-block)
  (define-key c-mode-map  (kbd "<f9> s") 'hs-show-block)

  )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(defun boxes-create-c ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -d c-cmt" nil 1 nil))
(defun boxes-remove-c ()
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -r -d c-cmt" nil 1 nil))





;;; cc-conf.el ends here
