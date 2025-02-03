;; Packages
(require 'package)
(add-to-list 'package-archives
	'("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; Misc-configuration
(use-package emacs
  :init
  ;; macOS-tweaks
  (setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)
  ;; Homebrew
  (when (file-directory-p "/usr/local/share/emacs/site-lisp")
    (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
      (normal-top-level-add-subdirs-to-load-path)))

  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
;  (set-default-font "Inconsolata-12")


  (setq custom-file "~/.emacs.d/custom-settings.el")
  (load custom-file t)

  (setq make-backup-files nil)

  (setq compilation-ask-about-save nil)
  ;; remember to install
  (setq ispell-program-name (executable-find "hunspell")
	ispell-dictionary "en_GB"))

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

;; Julia
(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :after vterm
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (julia-repl-set-terminal-backend 'vterm)
  (setq vterm-kill-buffer-on-exit nil))


;; Lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package paredit
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :diminish nil)


;; Move-text
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; OCaml

; Opam
;(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
;(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

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

;; Org
; Org-Babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

; Org-fragtog
(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))


;; Python
; pip3 install jedi rope flake8 autopep8 yapf black
; sudo apt install python3-jedi black python3-autopep8 yapf3 python3-yapf
; sudo zypper install pythonX-jedi pythonX-black pythonX-autopep8 pythonX-yapf
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq py-python-command "python3"))

;; Racket
(use-package racket-mode
  :ensure t)

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (setq cargo-process--enable-rust-backtrace t))

;; vterm
; apt install cmake libvterm-dev libtool-bin
(use-package vterm
  :ensure t)

