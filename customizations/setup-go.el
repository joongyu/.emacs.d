;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))
;;(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/joongyu/workspace/gocode")
(setq exec-path (cons "/usr/local/go/bin" exec-path))
(add-to-list 'exec-path "/home/joongyu/workspace/gocode/bin")

(defun my-go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

(load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")

(load-file "$GOPATH/src/github.com/dougm/goflymake/go-flymake.el")
(load-file "$GOPATH/src/github.com/dougm/goflymake/go-flycheck.el")
(require 'go-autocomplete)
(require 'auto-complete-config)

