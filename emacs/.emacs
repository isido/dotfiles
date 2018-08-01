(setq inhibit-startup-screen t)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Packages
(require 'package)
(add-to-list 'package-archives
	'("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; OS X tweaks
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; Homebrew
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Auctex
(use-package tex
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook #'turn-on-flyspell))

;; C
(setq c-default-style "linux"
      c-basic-offset 4)

;; CCrypt (not in repositories)
(require 'ps-ccrypt)

;; Clojure
;(require 'nrepl)

;; Elm
(use-package elm-mode
  :ensure t
  :config
  (setq-default elm-format-on-save t))

;; Go
;(require 'go-mode)
;(defun go-mode-setup ()
;  (go-eldoc-setup)
;  (setq gofmt-command "goimports")
;  (local-set-key (kbd "M-.") 'godef-jump)
;  (setq compile-command "go build -v && go test -v && go vet")
;  (define-key (current-local-map) "\C-c\C-c" 'compile)
;  (add-hook 'before-save-hook 'gofmt-before-save))

;(add-hook 'go-mode-hook 'go-mode-setup)

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

;; Move-text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; OCaml

; Opam
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;(require 'ocp-indent)

; Merlin
;(require 'merlin)
;(add-hook 'tuareg-mode-hook 'merlin-mode t)
;(add-hook 'caml-mode-hook 'merlin-mode t)
;(setq merlin-use-auto-complete-mode 'easy)
;(setq merlin-command 'opam)

; Utop
;(autoload 'utop "utop" "Toplevel for OCaml" t)
;(autoload 'utop-minor-mode "utop" "Toplevel for OCaml" t)
;(add-hook 'tuareg-mode-hook 'utop-minor-mode)

;; Ruby
;(require 'inf-ruby)

;; Scala
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

