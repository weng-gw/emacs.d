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

;; (use-package neotree
;;   :config (global-set-key (kbd "C-c t") 'neotree-toggle)
;;   )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-width-is-initially-locked     t
          treemacs-workspace-switch-cleanup      nil)
     ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	;;change all C-x to C-c in order to not conflict template-mode
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile)
;;   :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-style "bar")
  :bind
  ("<M-left>" . centaur-tabs-backward)
  ("<M-right>" . centaur-tabs-forward)
  ("<M-up>" . centaur-tabs-backward-group)
  ("<M-down>" . centaur-tabs-forward-group))


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

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
;;   ;;diable auto-complete-mode since it slows down editing and company is the default dependence
;;   (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
;;   ;; and note that you need to install jupyter console for this enviromnet with 
;;   ;; conda install -c anaconda jupyter_console
;;   (auto-complete-mode -1)
;;   (remove-hook 'elpy-modules 'elpy-module-flymake)
;;   (setq python-shell-interpreter "jupyter"
;; 	python-shell-interpreter-args "console --simple-prompt"
;; 	       python-shell-prompt-detect-failure-warning nil)
;; 	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
;; 	 (setq elpy-rpc-backend "jedi")
;; 	 (setq elpy-shell-echo-output nil);; this command is used for fixing ^G problem in MacOS
;; 	 )

(use-package elpy
  :after python  
  :config (elpy-enable)
  (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
  ;;diable auto-complete-mode since it slows down editing and company is the default dependence
  (pyvenv-activate (expand-file-name "~/opt/anaconda3/envs/py38"))
  ;; and note that you need to install jupyter console for this enviromnet with 
  ;; conda install -c anaconda jupyter_console
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	       python-shell-prompt-detect-failure-warning nil)
	 (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
	 (setq elpy-rpc-backend "jedi")
	 (setq elpy-shell-echo-output nil);; this command is used for fixing ^G problem in MacOS
	 )

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

;; (use-package tabbar
;;   :init (tabbar-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons yasnippet-snippets window-numbering use-package treemacs-magit treemacs-icons-dired tabbar smart-mode-line scala-mode poly-R neotree miniedit htmlize hc-zenburn-theme fold-this ess elpy ein conda centaur-tabs auto-complete auto-compile auctex anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#313131")))))
