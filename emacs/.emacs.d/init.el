(setq inhibit-startup-screen t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
        '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; OS X tweaks
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

;; For Homebrew
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Load snippets from directory
(defun load-directory (dir)
      (let ((load-it (lambda (f)
		       (load-file (concat (file-name-as-directory dir) f)))
		     ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/snippets")


