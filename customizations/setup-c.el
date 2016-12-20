(fset 'find-next-tag "\C-u\256")        ; macro for C-u M-.
(fset 'find-prev-tag "\C-u-\256")       ; macro for C-u - M-.
(global-set-key "\M-]" 'find-next-tag)
(global-set-key "\M-[" 'find-prev-tag)

(setenv "GOPATH" "/home/joongyu/workspace/gocode")
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/home/joongyu/workspace/gocode/bin")
