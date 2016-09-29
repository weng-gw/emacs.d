;;init.el

;; INSTALL REQUIRED PACKAGES
;;--------------------------------------------

(require 'package)
(setq package-list '(better-defaults
					  material-theme
					  ein
					  elpy
					  flycheck
					  py-autopep8))


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

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally


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

;; init.el ends here
