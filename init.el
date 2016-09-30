;;init.el

;; INSTALL REQUIRED PACKAGES
;;--------------------------------------------

(require 'package)
(setq package-list '( material-theme
					  ein
					  elpy
					  flycheck
					  py-autopep8
					  auto-complete
					  auctex
					  ess))


(add-to-list 'package-archives
			 '("melpa"."https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


;; BASIC CUSTOMIZATION
;;----------------------------------------------------
(server-start)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq column-number-mode t)
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally
(set-default-font "Monospace-16") ;; set font and font size
(setq-default auto-fill-function 'do-auto-fill) ;; enable auto-fill globally
(fset 'yes-or-no-p 'y-or-n-p)
(time-stamp)
;;PYTHON CONFIGURATION
;;-----------------------------------------------------

(elpy-enable)
(elpy-use-ipython)


;; use flycheck instead of flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;; ESS CONFIGURATION
;;-----------------------------------------------------
(require 'ess-site)
(setq ess-use-auto-complete 'script-only)
;; AUCTEX CONFIGURATION
;;-----------------------------------------------------
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-output-view-style (quote (("^pdf$" "." "evince %o %(outpage)"))))
(add-hook 'LaTeX-mode-hook
		  (lambda()
			(latex-math-mode 1)
			(add-to-list
			  'TeX-command-list' ("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-Tex nil t))
			(setq TeX-command-default "XeLaTeX")
			(setq TeX-show-compilation t)))
;; init.el ends here
