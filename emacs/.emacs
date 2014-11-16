(setq load-path (cons "/usr/local/Cellar/ccrypt/1.9/share/emacs/site-lisp/" load-path))
(require 'ps-ccrypt "ps-ccrypt.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(require 'nrepl)
(require 'inf-ruby)

(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
