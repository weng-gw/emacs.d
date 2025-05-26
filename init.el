;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 180) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(setq user-full-name "Guangwei Weng"
      user-mail-address "weng.gw@outlook.com")
(server-start)

;; Add exec-path-from-shell to ensure Emacs started from Mac App (homebrew Cask)
;; can still inherit shell vars
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message nil)        ; Remove all startup message
;;(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
;;(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell 1)       ; Set up the visible bella

;; Set font
(set-face-attribute 'default nil :font "Menlo-13")    ;; Set default font

;; Title Bar
(setq-default frame-title-format '("" user-login-name "@" system-name " - %b"))

(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		latex-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use the default theme in VSCode
(use-package doom-themes
  :init (load-theme 'doom-dark+ t))
  ;;(load-theme 'doom-zenburn t))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
;;(use-package all-the-icons)
;; <2023-08-28 Mon> switch to nerd-icons since it's better supported now
;;M-x nerd-icons-install-fonts
(use-package nerd-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  ;;:init 
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package swiper)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package window-numbering
  :init (window-numbering-mode 1))

(use-package dashboard
  :ensure t
  :init (setq dashboard-items '((recents  . 5)
                    (bookmarks . 5)
                    (projects . 5)
                    (agenda . 5)))
  :custom ((dashboard-projects-backend  'projectile)
           (dashboard-center-content t)
           (dashboard-set-heading-icons t)
           (dashboard-set-file-icons t))
  :config
  (dashboard-setup-startup-hook))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package hydra
  :defer t)
(defhydra hydra-buffer-menu (:color pink
                                    :hint nil)
   "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

(defun wgw/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil  :weight 'bold :height (cdr face))))

(defun wgw/org-mode-setup ()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :init
  (add-hook 'org-mode-hook 'flyspell-mode)
  :hook (org-mode . wgw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-image-actual-width nil)
  (wgw/org-font-setup))

(use-package org-bullets
  ;;:after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun wgw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . wgw/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("elconf" . "src emacs-lisp :tangle ./init.el :mkdirp yes"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(defun wgw/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'wgw/org-babel-tangle-config)))

(use-package htmlize)

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package org-roam
    :ensure t
    :init
    (setq org-roam-v2-ack t)
    :custom 
    (org-roam-directory "/Users/wgw/Library/CloudStorage/Dropbox/RoamNotes")
    (org-roam-completion-everywhere t)
      (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n g" . org-roam-graph)
           ("C-c n c" . org-roam-capture)
           :map org-mode-map
           ("C-M-i" . completion-at-point)
           :map org-roam-dailies-map
           ("Y" . org-roam-dailies-capture-yesterday)
           ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (org-roam-setup)
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (require 'org-roam-export)
    (org-roam-db-autosync-mode))

  (use-package ox-jekyll-md)
;; This add  extra options of converstion org file to jekyll posts
;; in markdown format.

(defun wgw/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . wgw/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config 
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-enable-file-watchers nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/Projects/")
    (setq projectile-project-search-path '("~/Documents/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(setq-default ispell-program-name "aspell")

(use-package yasnippet
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-reload-all)
  ;;(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'ess-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode)
  (add-hook 'markdown-mode-hook 'yas-minor-mode)
  (add-hook 'scala-mode-hook 'yas-minor-mode)
  (add-hook 'lisp-mode-hook 'yas-minor-mode))
;; note the snippets bundle needs to be installed separately
;; use M-x package-list-packages to list all packages available and install
;; yasnippet-snippets or yasnippet-classic-snippets`

(defun wgw/configure-eshell ()
;; Save command history when commands are entered
(add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

;; Truncate buffer for performance
(add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

;; Bind some useful keys for evil-mode
;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
;; (evil-normalize-keymaps)

(setq eshell-history-size         10000
      eshell-buffer-maximum-lines 10000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
:hook (eshell-first-time-mode . wgw/configure-eshell)
:config
(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))  
(eshell-git-prompt-use-theme 'robbyrussell))

(use-package ess
  :defer t
  :bind ("C-c C-s" . ess-switch-process)
  ;:hook (ess-r-mode . company-mode)
  :hook (ess-r-mode . lsp)
  :config (setq ess-fancy-comments nil)
  ;(setq ess-use-company t)
  ;(add-hook 'ess-mode-hook 'company-mode)
  )
;; Use ploymode for R markdown
(use-package polymode
  :defer t
  )

(use-package poly-R
  :defer t
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()                      
    (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package python-mode
  :ensure t
  ;:hook (python-mode . lsp)
  :after python
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ((python-shell-interpreter "ipython")
   (python-shell-interpreter-args "-i --simple-prompt")     
   )
  :config (add-to-list 'python-shell-completion-native-disabled-interpreters
            "ipython")
  ;; (dap-python-executable "python3")
  ;(dap-python-debugger 'debugpy)
  ;:config
  ;(require 'dap-python)
  )

(use-package highlight-indent-guides
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'column))

(use-package eval-in-repl
  :config
  (setq eir-repl-placement 'right)
  (require 'eval-in-repl-python)
  (add-hook 'python-mode-hook
          (lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python))))

(use-package conda
  ;; :init (;(conda-env-initialize-interactive-shells)
  ;;        (conda-env-initialize-eshell))
  :custom ((conda-anaconda-home "/opt/homebrew/Caskroom/miniforge/base/")))

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

(use-package yaml-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(taxy-magit-section yasnippet yaml-mode window-numbering which-key visual-fill-column use-package smart-mode-line scala-mode rainbow-delimiters pyvenv python-mode poly-R page-break-lines ox-jekyll-md org-roam org-download org-bullets no-littering nerd-icons-dired neotree miniedit magit lsp-ui lsp-treemacs lsp-python-ms lsp-pyright lsp-ivy jupyter ivy-rich htmlize highlight-indent-guides helpful hc-zenburn-theme git-commit general exec-path-from-shell evil eval-in-repl eshell-git-prompt ein doom-themes doom-modeline discover-my-major dashboard counsel-projectile conda company-box company-anaconda centaur-tabs auto-package-update auto-compile auctex all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
