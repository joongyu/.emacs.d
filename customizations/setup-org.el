;; Org mode

(require `org)
(setq org-agenda-files (list "~/.deft"))
(setq org-agenda-todo-list-sublevels nil)
; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
