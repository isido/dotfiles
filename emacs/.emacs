(setq inhibit-startup-screen t)

(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

(setq compilation-ask-about-save nil)

;; Packages
(require 'package)
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

;; C, linux kernel c-style
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun activate-linux-kernel-c-style ()
  "Settings for Linux Kernel development, see https://www.kernel.org/doc/html/latest/process/coding-style.html"
  (interactive)
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "linux-tabs-only"))

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))
	    (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
	    (define-key c-mode-base-map (kbd "C-c C-l") 'activate-linux-kernel-c-style)))

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
; $ go get golang.org/x/tools/cmd/goimports
(use-package go-mode
  :ensure t
  :config
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v && go test -v && go vet")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :bind 
  (:map go-mode-map
    ("C-C C-C" . compile)))

; Do I need these?
;  (go-eldoc-setup)
;  (local-set-key (kbd "M-.") 'godef-jump)

;; Haskell
(use-package haskell-mode
  :ensure t
  :bind (:map haskell-mode-map
	      ("C-C C-l" . haskell-process-load-file)))

(use-package company-ghci
  :ensure t
  :config
  (push 'company-ghci company-backends)
  (add-hook 'haskell-mode-hook 'company-mode))

;(use-package intero
;  :ensure t
;  :config
;  (add-hook 'haskell-mode-hook 'intero-mode))

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

;; Python
; pip3 install jedi rope flake8 autopep8 yapf black
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq py-python-command "python3"))


;; Racket
(use-package racket-mode
  :ensure t)

;; Ruby
;(require 'inf-ruby)

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

;; Scala
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

