;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    General Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package-initialize)
(setq usepackage-always-ensure t)

(setq user-full-name "Guangwei Weng"
      user-mail-address "wengx076@umn.edu")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


(add-to-list 'load-path "~/.emacs.d/elisp") ;; path for customized pacakges
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
  :config (exec-path-from-shell-initialize))
(setq-default ispell-program-name "aspell")
(use-package miniedit
  :config (miniedit-install))
(time-stamp)
(add-hook 'write-file-hooks 'time-stamp)


(use-package hc-zenburn-theme
  :config (load-theme 'hc-zenburn t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config (color-theme-sanityinc-tomorrow-day))

;; Recentf is a minor mode that builds a list of recently opened files.
;; This list is is automatically saved across sessions on exiting Emacs
(use-package recentf
  :init (recentf-mode 1))

(use-package neotree
  :config
  (global-set-key (kbd "C-c t") 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package all-the-icons) ;; this is used by centaur-tabs
;; After adding this, need to run M-x all-the-icons-install-fonts to install the icons
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-style "bar")
  :bind
  ("<s-left>" . centaur-tabs-backward)
  ("<s-right>" . centaur-tabs-forward)
  ("<s-up>" . centaur-tabs-backward-group)
  ("<s-down>" . centaur-tabs-forward-group))

;; The library uniquify overrides Emacs’ default mechanism for making buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Code templating
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; template was deprecated and will use snippets for future templates
;; (require 'template)
;; (template-initialize)

(use-package yasnippet
  :config (yas-reload-all)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'ess-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode-hook 'yas-minor-mode)
  (add-hook 'scala-mode-hook 'yas-minor-mode)
  (add-hook 'lisp-mode-hook 'yas-minor-mode))
;; note the snippets bundle needs to be installed separately
;; use M-x package-list-packages to list all packages available and install yasnippet-snippets or yasnippet-classic-snippets


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Scientific Computing with R/ESS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ess
  :defer t
  :bind ("C-c C-s" . ess-switch-process)
  :config (setq ess-fancy-comments nil)
  (setq ess-use-company t)
  (add-hook 'ess-mode-hook 'company-mode)
  )

;; Use ploymode for R markdown
(use-package polymode
  :defer t
  )

(use-package poly-R
  :defer t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    LaTex with auctex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Git with magit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Python configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Desired features
;; 1. Auto complete in Python script buffer
;; 2. Send code line or region to buffer

;; (use-package python-mode
;;   :ensure t
;;   :init
;;   (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
;;   (setq py-shell-name "ipython")
  ;; (setq python-shell-interpreter "ipython"
  ;; 	python-shell-interpreter-args "-i"
  ;; 	python-shell-prompt-detect-failure-warning nil)
;;  )					
  
;; (use-package jupyter
;;   :ensure t
;;   :commands (jupyter-run-server-repl
;;              jupyter-run-repl
;;              jupyter-server-list-kernels)
;;   :init (eval-after-load 'jupyter-org-extensions ; conflicts with my helm config, I use <f2 #>
;;           '(unbind-key "C-c h" jupyter-org-interaction-mode-map)))  


;; Elpy was tried and deprecated because it becomes lagging with long code files and
;; data science project with large data tables
;;
;; (use-package elpy
;;   :ensure t
;;   :after python  
;;   :config (elpy-enable)
;;   (setq elpy-rpc-backend "jedi")  
;;   (setq company-idle-delay 0.5)
;;   (setq elpy-company-add-completion-from-shell nil)
;;   ;;(add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
;;   ;;diable auto-complete-mode since it slows down editing and company is the default dependence
;;   (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
;;   ;; and note that you need to install jupyter console for this enviromnet with 
;;   ;; conda install -c anaconda jupyter_console
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (setq python-shell-interpreter "jupyter"
;; 	python-shell-interpreter-args "console --simple-prompt"
;; 	       python-shell-prompt-detect-failure-warning nil)
;; 	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
;; 	 (setq elpy-shell-echo-output nil);; this command is used for fixing ^G problem in MacOS
;; 	 )
;;Note after install elpy, do not remove the custom-set-varaibles added by Emacs in the end

;; use company mode for auto completion in Python
;; see https://gist.github.com/yiufung/d8216038252f0488198e8b6af1e2ece4
(use-package company
 :ensure t
 :config
 (setq company-idle-delay 0
       company-minimum-prefix-length 2
       company-show-numbers t
       company-tooltip-limit 10
       company-tooltip-align-annotations t
       ;; invert the navigation direction if the the completion popup-isearch-match
       ;; is displayed on top (happens near the bottom of windows)
       company-tooltip-flip-when-above t)
 ;;(global-company-mode t)
 )

;; current use anaconda-mode for Python script mode
(use-package anaconda-mode
  :ensure t
  :after python
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (setq python-shell-interpreter "ipython")
  ;;(add-hook 'python-mode-hook 'company-mode)
  )
(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after (company)
  :config
  (add-to-list 'company-backends 'company-anaconda)
  )
;;make sure you install jedi and epc, e.g., conda/pip install jedi epc, in your current enviroment

;; Looks like the below code is not needed, but keep it for now
;;
;; If you really want to try with jedi+company, use below scripts it
;; (remove the :disabled tag)
;; (use-package jedi
;;   :disabled
;;   :after (epc pos-tip)
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   :config
;;   ;; For setup
;;   ;; http://d.hatena.ne.jp/n-channel/20131220/1387551080
;;   ;; and this:
;;   ;; http://kenbell.hatenablog.com/entry/2017/01/15/112344

;;   ;; Under windows, process might very long and EPC may fail.
;;   ;; Set it larger. What a bummer...
;;   ;;(if (memq system-type '(ms-dos windows-nt))
;;   ;;(setq epc:accept-process-timeout 1000))
;;   )
;; (use-package company-jedi
;;   :disabled
;;   :ensure t
;;   :config)


;; Implementing repl buffer with eval-in-repl, to have Ctrl-Enter to send code to buffer
;;
;; Still have issues 1. Code sent to reply have duplicated last char 2. Code region are not
;; copied to reply
;;
;; TODO: fixing issue 1&2
;; TODO: try jupyter mode to check implementing a VSCode style IDE pattern
(use-package eval-in-repl
  :config
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-python)
  (add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))

;; Show indentation level
(use-package highlight-indent-guides
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))


;; TODO: For pyvenv, currently Emacs session started from GUI app does not inherit 
;; the enviroment variable $WORKON_HOME (there is no isssue with session started from zsh).
;; Need to fix it for easier use of pyvenv-workon


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Emacs IPython Notebook
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ein
  :defer t
  :config (require 'ein)
  (setq ein:completion-backend 'ein:use-company-jedi-backend)
  (require 'ein-loaddefs)
  (require 'ein-notebook)
  (require 'ein-subpackages)
  )
;; to set for styling markdonw headings
;; M-x customize-group RET ein:markdown-faces RET toggle ein:markdown-header-scaling  to non-nil
;; To show inline images, select  then toggle ein:output-are-inlined-imoages to non-nil

(use-package markdown-mode)  ;required by EIN


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Orgmode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :defer t
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)))
  (setq org-src-window-setup 'other-frame)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Scala mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:markdown-header-scaling t)
 '(ein:output-area-inlined-images t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
 '(package-selected-packages
   '(highlight-indent-guides yasnippet-classic-snippets eval-in-repl-python eval-in-repl company-jedi jedi company-anaconda good-scroll good-scroll-mode smooth-scrolling smooth-scroll yasnippet-snippets window-numbering use-package smart-mode-line scala-mode poly-R neotree miniedit magit htmlize hc-zenburn-theme exec-path-from-shell ess ein centaur-tabs auto-compile auctex all-the-icons))
 '(pixel-scroll-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
