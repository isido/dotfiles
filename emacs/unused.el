;; Clojure
;(require 'nrepl)

;; Elm
;(use-package elm-mode
;  :ensure t
;  :config
;  (setq-default elm-format-on-save t))

;; Go

; Do I need these?
;  (go-eldoc-setup)
;  (local-set-key (kbd "M-.") 'godef-jump)

;; Ruby
;(require 'inf-ruby)

;; Scala
;(require 'ensime)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

; Org-Roam
;(use-package org-roam
;  :ensure t
;  :after org
;  :init
;  (setq org-roam-directory (file-truename "~/Dropbox/notes"))
;  (setq org-roam-v2-ack t)
;  (org-roam-db-autosync-mode)
;  :bind (("C-c n l" . org-roam-buffer-toggle)
;         ("C-c n f" . org-roam-node-find)
;         ("C-c n i" . org-roam-node-insert))
;  :config
;  (org-roam-setup)
;    :bind (("C-c n f" . org-roam-node-find)
;           ("C-c n r" . org-roam-node-random)		    
;           (:map org-mode-map
;                 (("C-c n i" . org-roam-node-insert)
;                  ("C-c n o" . org-id-get-create)
;                  ("C-c n t" . org-roam-tag-add)
;                  ("C-c n a" . org-roam-alias-add)
;                  ("C-c n l" . org-roam-buffer-toggle)))))
