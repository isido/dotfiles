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

;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; CCrypt from homebrew, adjust on linux
(setq load-path (cons "/usr/local/Cellar/ccrypt/1.9/share/emacs/site-lisp/" load-path))
(require 'ps-ccrypt "ps-ccrypt.el")

;; OS X tweaks
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Clojure
;(require 'nrepl)

;; Go
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Ruby
(require 'inf-ruby)

;; Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; OCaml

; Opam
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

(require 'ocp-indent)

; Merlin
(require 'merlin)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(setq merlin-use-auto-complete-mode 'easy)
(setq merlin-command 'opam)

; Utop
(autoload 'utop "utop" "Toplevel for OCaml" t)
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
