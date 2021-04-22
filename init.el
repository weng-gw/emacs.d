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
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
  ;;diable auto-complete-mode since it slows down editing and company is the default dependence
  (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
  ;; and note that you need to install jupyter console for this enviromnet with 
  ;; conda install -c anaconda jupyter_console
  (auto-complete-mode -1)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	       python-shell-prompt-detect-failure-warning nil)
	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
	 (setq elpy-rpc-backend "jedi")
	 (setq elpy-shell-echo-output nil);; this command is used for fixing ^G problem in MacOS
	 )

;; (use-package elpy
;;   :after python  
;;   :config (elpy-enable)
;;   (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
;;   ;; and note that you need to install jupyter console for this enviromnet with 
;;   ;; conda install -c anaconda jupyter_console
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (setq python-shell-interpreter "jupyter"
;; 	python-shell-interpreter-args "console --simple-prompt"
;; 	       python-shell-prompt-detect-failure-warning nil)
;; 	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
;; 	 (setq elpy-rpc-backend "jedi")
;; 	 (setq elpy-shell-echo-output nil);; this command is used for fixing ^G problem in MacOS
;; 	 )

;; (use-package conda
;;   :defer t
;;   :config (require 'conda)
;;   (setq 
;;    conda-env-home-directory (expand-file-name "~/opt/anaconda3/")
;;    ))
;; (custom-set-variables
;;    '(conda-anaconda-home (expand-file-name "~/opt/anaconda3/")))

;; (use-package anaconda-mode)

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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)))
  (setq org-publish-project-alist
      '(

  ("org-ianbarton"
          ;; Path to your org files.
          :base-directory "~/Documents/PersonalPage/"
          :base-extension "org"

          ;; Path to your Jekyll project.
          :publishing-directory "~/Documents/PersonalPage/"
          :recursive t
          :publishing-function org-html-publish-to-html
          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
    )


    ("org-static-ian"
          :base-directory "~/Documents/PersonalPage/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
          :publishing-directory "~/Documents/PersonalPage/"
          :recursive t
          :publishing-function org-publish-attachment)

    ("ian" :components ("org-ianbarton" "org-static-ian"))

))
  )


(use-package htmlize)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))



(use-package yasnippet
  :config (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'python-mode 'yas-minor-mode)
  ;;(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode-hook 'yas-minor-mode)
  (add-hook 'scala-mode-hook 'yas-minor-mode))
;; note the snippets bundle needs to be installed separately
;; use M-x package-list-packages to list all packages available and install yasnippet-snippets
