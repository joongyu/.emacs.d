(require `deft)
(setq deft-extensions `("txt" "tex" "org"))
(setq deft-directory "~/.deft")
(setq deft-recursive t)
(global-set-key [f9] `deft)
(setq deft-use-filename-as-title nil)
(setq deft-auto-save-buffers nil)
(setq deft-auto-save-interval 0)
(setq deft-file-naming-rules '((nospace . "_")
                               (case-fn . downcase)))