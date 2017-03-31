;; winner

(add-hook 'winner-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c o v") 'delete-other-windows-vertically)
            (local-set-key (kbd "C-c o a") 'delete-other-windows)
            (local-set-key (kbd "C-c o n") 'winner-redo)
            (local-set-key (kbd "C-c o p") 'winner-undo)
            ))
