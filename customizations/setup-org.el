;; Org mode

(require `org-install)
(require `org)
(setq org-agenda-files (list "~/.deft"))
(setq org-agenda-todo-list-sublevels 2)
(setq org-log-done t)
; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-startup-indented t)
(global-set-key (kbd "C-<tab>") `other-window)
(local-set-key (kbd "C-<tab>") `other-window)
;; (define-key org-mode-map [remap org-force-cycle-archived] nil)
(define-key org-mode-map (kbd "C-<tab>") nil)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/.deft")
(setq org-default-notes-file "~/.deft/organizer.org")
(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'my/yank-more)

(setq org-src-window-setup 'current-window)

(require `htmlize)
(setq org-adapt-indentation nil)

(defun my/org-contacts-template-email (&optional return-value)
  "Try to return the contact email for a template.
  If not found return RETURN-VALUE or something that would ask the user."
  (or (cadr (if (gnus-alive-p)
                (gnus-with-article-headers
                 (mail-extract-address-components
                  (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
      return-value
      (concat "%^{" org-contacts-email-property "}p")))


;; customized org-capture-template 
(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured: %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")
(setq org-capture-templates
      `(("t" "Tasks" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         ,my/org-basic-task-template)
        ("T" "Quick task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* TODO %^{Task}\nSCHEDULED: %t\n"
         :immediate-finish t)
        ("i" "Interrupting task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* STARTED %^{Task}"
         :clock-in :clock-resume)
        ;; ("e" "Emacs idea" entry
        ;;  (file+headline "~/code/emacs-notes/tasks.org" "Emacs")
        ;;  "* TODO %^{Task}"
        ;;  :immediate-finish t)
        ;; ("E" "Energy" table-line
        ;;  (file+headline "~/personal/organizer.org" "Track energy")
        ;;  "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
        ;;  :immediate-finish t
        ;;  )
        ("b" "Business task" entry
         (file+headline "~/personal/business.org" "Tasks")
         ,my/org-basic-task-template)
        ("p" "People task" entry
         (file+headline "~/personal/people.org" "Tasks")
         ,my/org-basic-task-template)
        ("j" "Journal entry" plain
         (file+datetree "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("J" "Journal entry with date" plain
         (file+datetree+prompt "~/personal/journal.org")
         "%K - %a\n%i\n%?\n"
         :unnarrowed t)
        ("s" "Journal entry with date, scheduled" entry
         (file+datetree+prompt "~/personal/journal.org")
         "* \n%K - %a\n%t\t%i\n%?\n"
         :unnarrowed t)
        ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
         "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
        ("db" "Done - Business" entry
         (file+headline "~/personal/business.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dp" "Done - People" entry
         (file+headline "~/personal/people.org" "Tasks")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("dt" "Done - Task" entry
         (file+headline "~/personal/organizer.org" "Inbox")
         "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
        ("q" "Quick note" item
         (file+headline "~/personal/organizer.org" "Quick notes"))
        ;; ("l" "Ledger entries")
  ;;       ("lm" "MBNA" plain
  ;;        (file "~/personal/ledger")
  ;;        "%(org-read-date) %^{Payee}
  ;;   Liabilities:MBNA
  ;;   Expenses:%^{Account}  $%^{Amount}
  ;; " :immediate-finish t)
        ;; ("ln" "No Frills" plain
        ;;  (file "~/personal/ledger")
         ;; "%(let ((org-read-date-prefer-future nil)) (org-read-date)) * No Frills
    ;; Liabilities:MBNA
    ;; Assets:Wayne:Groceries  $%^{Amount}
  ;; " :immediate-finish t)
        ;; ("lc" "Cash" plain
         ;; (file "~/personal/ledger")
         ;; "%(org-read-date) * %^{Payee}
    ;; Expenses:Cash
    ;; Expenses:%^{Account}  %^{Amount}
  ;; ")
        ("B" "Book" entry
         (file+datetree "~/personal/books.org" "Inbox")
         "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
         :clock-in :clock-resume)
        ("C" "Contact" entry (file "~/personal/contacts.org")
         "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")
        ("n" "Daily note" table-line (file+olp "~/personal/organizer.org" "Inbox")
         "| %u | %^{Note} |"
         :immediate-finish t)
        ("r" "Notes" entry
         (file+datetree "~/personal/organizer.org")
         "* %?\n\n%i\n"
         )))

;; keyboard shortcut for org block structure. just type <s or <e for example.
(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

;; settings for org-agenda 
(setq org-agenda-span 2)
(setq org-agenda-tags-column -100) ; take advantage of the screen width
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
       "----------------"
       (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS")

(global-set-key (kbd "C-c t") 'insert-time-date)
(defun insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d %H:%M:%S")
                   ((equal prefix '(4)) "%Y-%m-%d %H:%M:%S"))))
      (insert (format-time-string format))))

(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
