;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
;; (ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
;; (setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
;; (setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
;; (setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
;; (setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
;; (ido-ubiquitous-mode 1)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
;; (setq smex-save-file (concat user-emacs-directory ".smex-items"))
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
(windmove-default-keybindings)
(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))
(global-set-key (kbd "C-<tab>") `other-window)
(global-set-key (kbd "S-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))

;; escreen
(load "escreen")
(escreen-install)
(setq escreen-prefix-char "\C-z")
(global-set-key escreen-prefix-char 'escreen-prefix)

(add-to-list 'load-path "/home/joongyu/.emacs.d/github.com/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
;; (setq projectile-switch-project-action 'neotree-projectile-action)
;; (setq projectile-switch-project-action 'projectile-dired)

;; winner mode allow to undo and redo with respective to C-c <left> and C-c <right>
(use-package winner
  :defer t)

;; if you wan to use flx-ido as a projectile-completion-system, uncomment it
;; (require 'flx-ido)
;; (ido-mode 1)
;; (ido-everywhere 1)
;; (flx-ido-mode 1)

;; ;; disable ido faces to see flx highlights.
;; (setq ido-enable-flex-matching t)
;; (setq ido-use-faces nil)

;; use helm-projectile as a projectile-completion-system

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x C-f" . helm-find-files)
         ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-mode 1)
;; (setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (helm-autoresize-mode 1)
;; (setq helm-autoresize-max-height 40)
;; (setq helm-autoresize-min-height 40)

(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))
 ;; disable popwin-mode in an active Helm session It should be disabled
 ;; otherwise it will conflict with other window opened by Helm persistent
 ;; action, such as *Help* window.
;; (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
;; (add-hook 'helm-after-initialize-hook (lambda ()
;;                                           (popwin:display-buffer helm-buffer t)
;;                                           (popwin-mode -1)))

;;  ;;  Restore popwin-mode after a Helm session finishes.
;;  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))


;; remap bindings to split windows
(global-set-key (kbd "C-x -") `split-window-below)
(global-set-key (kbd "C-x |") `split-window-right)


;; visualize undos and branches
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))


;; imenu anywhere maybe in a project bassis?
(unless (package-installed-p 'imenu-anywhere)
  (package-refresh-contents)
  (package-install 'imenu-anywhere))
(global-set-key (kbd "C-.") #'helm-imenu-anywhere)

(use-package guide-key
  :defer t
  :diminish guide-key-mode
  :config
  (progn
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  (guide-key-mode 1)))  ; Enable guide-key-mode

;; getting back to previous place
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; fast way to find something in buffers
(use-package helm-swoop
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-s s" . helm-swoop)
  ("M-s M-s" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  )
 :config
 (progn
   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
)


;; move to beginning of line
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(require 'helm-cscope)
;; Enable helm-cscope-mode
(add-hook 'c-mode-hook 'helm-cscope-mode)
(add-hook 'c++-mode-hook 'helm-cscope-mode)
;; Set key bindings
(eval-after-load "helm-cscope"
  '(progn
     (define-key helm-cscope-mode-map (kbd "M-]") 'cscope-find-global-definition-no-prompting)
     (define-key helm-cscope-mode-map (kbd "M-[") 'cscope-pop-mark)
     (define-key helm-cscope-mode-map (kbd "C-c s g") 'helm-cscope-find-definition)
     (define-key helm-cscope-mode-map (kbd "C-c s s") 'helm-cscope-find-this-symbol)
     (define-key helm-cscope-mode-map (kbd "C-c s C") 'helm-cscope-find-called-function)
     (define-key helm-cscope-mode-map (kbd "C-c s c") 'helm-cscope-find-calling-this-funtcion)
     (define-key helm-cscope-mode-map (kbd "M-s") 'helm-cscope-select)))
