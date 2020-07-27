(package-initialize)
(setq usepackage-always-ensure t)

(setq user-full-name "Guangwei Weng"
      user-mail-address "wengx076@umn.edu")

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


(add-to-list 'load-path "~/.emacs.d/elisp")
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
;;(global- linum-mode t)
(global-set-key (kbd "C-c l") 'goto-line)
(setq inhibit-startup-message t)
(set-frame-font "Menlo-13" t t)
(add-to-list 'default-frame-alist '(font . "Menlo-13" ))
(set-face-attribute 'default t :font "Menlo-13" )
(show-paren-mode 1)
(use-package smart-mode-line)
(fset 'yes-or-no-p 'y-or-n-p)
(use-package window-numbering
  :init (window-numbering-mode 1))
(progn
  (require 'windmove)
  ;; use Shift+arrow_keys to move cursor around split panes
  (windmove-default-keybindings)
  ;; when cursor is on edge, move to the other side, as in a torus space
  (setq windmove-wrap-around t )
)
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

;; (use-package color-theme-sanityinc-tomorrow
;;   :config (color-theme-sanityinc-tomorrow-day))

(use-package recentf
  :init (recentf-mode 1))

(use-package neotree
  :config (global-set-key (kbd "C-c t") 'neotree-toggle)
  )

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)


(use-package ess
  :defer t
  :bind ("C-c C-s" . ess-switch-process)
  )

(use-package polymode
  :defer t
  )

(use-package poly-R
  :defer t
  )


(use-package auto-complete
  :config (ac-config-default))

;; auctex setting for MacOS with Skim
(use-package auctex
  :hook  (LaTeX-mode . flyspell-mode)
  :init
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (setq TeX-PDF-mode t)
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-view-program-list
	'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
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
  (setq TeX-source-correlate-start-server t)
  )




(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))


(use-package elpy
  :after python  
  :config (elpy-enable)
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console  --simple-prompt"
	       python-shell-prompt-detect-failure-warning nil)
	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
	 (setq elpy-rpc-backend "jedi"))



(use-package ein
  :defer t
  :config (require 'ein)
  (setq ein:completion-backend 'ein:use-ac-jedi-backend)
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages)
  )
(use-package markdown-mode)  ;required by EIN

(use-package org
  :defer t
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  )

(use-package htmlize)

(add-hook 'markdown-mode-hook 'auto-fill-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#000000" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(beacon-color "#d54e53")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow window-numbering use-package smart-mode-line poly-R neotree miniedit material-theme magit htmlize hc-zenburn-theme exec-path-from-shell ess elpy ein auto-compile auctex)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight normal :height 130 :width normal)))))
