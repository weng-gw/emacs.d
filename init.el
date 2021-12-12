(require 'package)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq user-full-name "Guangwei Weng"
      user-mail-address "wengx076@umn.edu")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


(add-to-list 'load-path "~/.emacs.d/elisp")

(setq usepackage-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(use-package auto-compile
  :defer t
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)


(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(server-start)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)

(setq visible-bell 1)
(setq column-number-mode t)
(setq-default fill-column 80)
;;(global-linum-mode t)
(setq inhibit-startup-message t)
(set-frame-font "Monospace-13" t t)
(show-paren-mode 1)
(use-package smart-mode-line)
(fset 'yes-or-no-p 'y-or-n-p)
(use-package window-numbering
  :init (window-numbering-mode 1))
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
(setq-default ispell-program-name "aspell")
(use-package miniedit
  :config (miniedit-install))
(time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(require 'template)
(template-initialize)

(use-package hc-zenburn-theme
  :config (load-theme 'hc-zenburn t))

(use-package ess
  :defer t
  )

(use-package auto-complete
  :config (ac-config-default))

;; auctex setting for MacOS with Skim
;; (use-package auctex
;;   :hook  (LaTeX-mode . flyspell-mode)
;;   :init
;;   (setq TeX-PDF-mode t)
;;   (setq TeX-output-view-style (quote (("^pdf$" "." "evince %o %(outpage)"))))
;;   (add-hook 'LaTeX-mode-hook
;; 	    (lambda()
;; 	      (latex-math-mode 1)
;; 	      (add-to-list
;; 	       'TeX-command-list' ("XeLaTeX" "%`xelatex -synctex=1%(mode)%' %t" TeX-run-TeX nil t))
;; 	      (setq TeX-command-default "XeLaTeX")
;; 	      (setq TeX-show-compilation nil)))
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   (setq TeX-source-correlate-method 'synctex)
;;   (setq TeX-source-correlate-mode t)
;;   (setq TeX-source-correlate-start-server t)
;;   )



;; auctex setting for Linux with evince
(use-package auctex
  :hook (LaTeX-mode . flyspell-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (setq TeX-PDF-mode t)
  (setq TeX-output-view-style (quote (("^pdf$" "." "evince %o %(outpage)"))))
  (add-hook 'LaTeX-mode-hook
	    (lambda()
	      (latex-math-mode 1)
	      (add-to-list
	       'TeX-command-list' ("XeLaTeX" "%`xelatex -synctex=1%(mode)%' %t" TeX-run-TeX nil t))
	      (setq TeX-command-default "XeLaTeX")
	      (setq TeX-show-compilation nil)))
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t))


(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))


;; (use-package elpy
;;   :after python  
;;   :config (elpy-enable)
;;   (setq python-shell-interpreter "jupyter"
;; 	python-shell-interpreter-args "console  --simple-prompt"
;; 	       python-shell-prompt-detect-failure-warning nil)
;; 	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
;; 	 (setq elpy-rpc-backend "jedi"))



;; (use-package ein
;;   :defer t
;;   :config (require 'ein)
;;   (setq ein:completion-backend 'ein:use-ac-jedi-backend)
;;   (require 'ein-loaddefs)
;;   (require 'ein-notebook)
;;   (require 'ein-subpackages)
;;   )
;; (use-package markdown-mode)  ;required by EIN

(use-package org
  :defer t
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  )

(use-package htmlize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(htmlize markdown-mode ein elpy magit auctex auto-complete ess hc-zenburn-theme miniedit exec-path-from-shell window-numbering smart-mode-line auto-compile use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
