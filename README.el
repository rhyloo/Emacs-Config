;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package) ;; Initialize package sources
(setq package-archives
      '(;; ("org"     .       "https://orgmode.org/elpa/")
        ("gnu"     .       "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; (package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Use-package for civilized configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq user-full-name "Jorge L. Benavides M."
      user-real-login-name "Rhyloo"
      user-mail-address "rhyloot@gmail.com")

(scroll-bar-mode -1)         ;; Disable visible scrollbar
(tool-bar-mode -1)           ;; Disable the toolbar
(tooltip-mode -1)            ;; Disable tooltips
(menu-bar-mode -1)           ;; Disable the menu bar
(set-fringe-mode 15)         ;; Give some breathing room (borders)
(setq-default frame-title-format '("%b [%m]")) ;; Title bar name
(setq inhibit-startup-message t) ;; Avoid startup message
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq display-time-format "%H:%M %d %b %y" display-time-default-load-average nil) ;; Show hour minute day month and year
(setq display-time-day-and-date t display-time-24hr-format t) ;; Change format
(display-time)               ;; Show the time in the bar

(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(column-number-mode)                  ;; Enable column mode

(show-paren-mode 1)          ;; Show parens
(global-hl-line-mode 1)      ;; Highlight lines
(global-visual-line-mode 1)  ;; Better than fix the lines with set-fill-column
(windmove-default-keybindings 'M) ;; Move windows

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))  ;; Set frame transparency
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))   ;; Set frame transparency
(set-frame-parameter (selected-frame) 'fullscreen 'maximized) ;; maximize windows by default.
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize windows by default.
(use-package vscode-dark-plus-theme                         ;; Set theme VScode
  :defer t
  :init
  (add-hook 'after-init-hook (load-theme 'vscode-dark-plus t)))

(setq org-startup-folded t)
(setq org-return-follows-link 1)
(use-package org
  :defer t
  :pin gnu
  :hook
  ((before-save . zp/org-set-last-modified))
  :config)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
        (sequence "EXPERIMENTAL(e)" "FAIL(f)" "|" "WORKS(w)")))

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)")
;;         (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;         (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:weight normal :box (:line-width 1 :color (\, yellow) :style nil) :foreground "yellow"))
        ("EXPERIMENTAL" . (:weight normal :box (:line-width 1 :color (\, white) :style nil) :foreground "white"))
        ("WORKS" . (:weight normal :box (:line-width 1 :color (\, green) :style nil) :foreground "green"))
        ("FAIL" . (:weight normal :box (:line-width 1 :color (\, red) :style nil) :foreground "red"))
        ))

(global-set-key (kbd "C-c C-c") 'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil) ;; Stop the confirmation to evaluate org babel
(setq org-src-tab-acts-natively t)    ;; Indent code in org-babel
(org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (org . t)
     (octave . t)
     (css . t)
     (dot . t)
     (latex . t)
     (lua . t)
     (shell . t)
     (python . t)
     (matlab . t)
     (emacs-lisp . t)))
;; (add-to-list 'org-structure-template-alist ;; Add #+begin_structure
;; 	      '(("ec" . "emacs-lisp")
;; 		("py" . "python")))

(setq org-adapt-indentation t         ;; Modifies paragraph filling
      org-hide-leading-stars t              ;; Leading stars invisible
      org-odd-levels-only nil               ;; Org use only odd levels (disable)
      org-src-preserve-indentation nil      ;; Preserves the indentation of the source code in the src edit buffer
      org-edit-src-content-indentation 0)   ;; Respect parent buffer indentation


(add-hook 'org-mode-hook 'org-indent-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                matlab-mode-hook
                conf-mode-hook
                lisp-mode-hook))
  (add-hook mode (lambda () 
                   (display-line-numbers-mode 1))))    

;; Override modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () 
                   (display-line-numbers-mode -1))))   

;; Startup with inline images (disable)
(setq org-startup-with-inline-images nil)
(setq org-image-actual-width nil)

(setq-default tab-width 2) ;; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width) ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
(delete-selection-mode 1) ;; Let you select and replace with yank or write

(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory
(setq read-file-name-completion-ignore-case t) ;; Insensitive letter case
(setq large-file-warning-threshold nil)        ;; Dont warn for large files
(fset 'yes-or-no-p 'y-or-n-p)                  ;; Replace yes or no for y or n
(setq dired-listing-switches "-ls")
(setq dired-dwim-target t) ;; Allow you move files splitting the window

(global-auto-revert-mode 1)  ;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t)    ;; Revert Dired and other buffers

(defun my-clear ()
  (interactive)
  (comint-clear-buffer))

(defun my-shell-hook ()
  (local-set-key "\C-l" 'my-clear))

(add-hook 'shell-mode-hook 'my-shell-hook)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)) ;; Open pdfs by default with emacs

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/load-blog-configuration ()
  (interactive)
  (load-file "~/.emacs.d/blog.el"))

(defun my/find-emacs-configuration ()
  (interactive)
  (find-file (concat user-emacs-directory my-user-init-file)))

(defun my/find-file (filename)
  "Open a file in the background"
  (interactive "FFind file: ")
  (set-buffer (find-file-noselect filename)))

(defun my/pwd ()
  "Put the current file name (include directory) on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
		                  default-directory
		                (buffer-file-name))))
    (when filename
      (with-temp-buffer
	      (insert filename)
	      (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun my/create-temp-directory ()
  "This function let you create directories or files in the tmp directory for testing"
  (interactive)
  (let (
        (choices '("directory" "files"))
        (name (read-string "Enter name temporary file: ")))

    (find-file (concat "/tmp/" name))
    (message name)))

;; --------------------------
;; Handling file properties for 'CREATED' & 'LAST_MODIFIED'
;; --------------------------

(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
  When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.
  As a special case, return -1 if the time file PROPERTY exists but
  is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))

(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.
  When ANYWHERE is non-nil, search beyond the preamble.
  If the position of the file PROPERTY has already been computed,
  it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

(eval-after-load 'pdf-tools
  '(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp)) ;; Set C-s for searching in pdf-tools

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-x wti")  'display-time-world)

(global-set-key (kbd "C-c l") 'my/svg-to-pdf)
(global-set-key (kbd "C-x q") 'compile)

(global-set-key (kbd "<f1>") 'my/find-emacs-configuration)
(global-set-key (kbd "<f4>") 'org-publish-all)
(global-set-key (kbd "<f5>") 'my/reload-emacs-configuration)
(global-set-key (kbd "<f6>") 'org-publish-current-file)
(global-set-key (kbd "<f9>") 'my/pwd)
(global-set-key (kbd "<f8>") 'my/upload-doc)
(global-set-key (kbd "<f7>") 'my/actualization-repo)
(global-set-key (kbd "<f12>") 'flyspell-auto-correct-word)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
(global-set-key (kbd "M-+") 'dired-create-empty-file)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "\C-c M-+") 'my/create-temp-directory)

;; ;; FUNCION PARA CREAR ARCHIVOS TEMPORALES, PARA PROBAR COSAS O ESCRIBIR x COSAS
;; (lambda ()
;;   (with-temp-buffer
;;     (setq temp-file-name (read-string "Temporary file name: "))
;;     (message temp-file-name)
;;     (find-file (concat "/tmp/" temp-file-name))))
;; (global-set-key (kbd "M-o") 'ace-window)

;; If there were no compilation errors, delete the compilation window
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;; then bury the *compilation* buffer, so that C-x b doesn't go there
          (bury-buffer "*compilation*")
          ;; and return to whatever were looking at before
          (replace-buffer-in-windows "*compilation*"))
        ;; Always return the anticipated result of compilation-exit-message-function
        (cons msg code)))


;; Experimental from here, I am not sure whats do with compilations buffers
(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                ;;no errors, make the compilation window go away in a few seconds
                (progn
                  (run-at-time
                   "2 sec" nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (message "No Compilation Errors!")))))
(setq compilation-window-height 10)

(defun ct/create-proper-compilation-window ()
  "Setup the *compilation* window with custom settings."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")

          ;; Reduce window height
          (shrink-window (- h compilation-window-height))

          ;; Prevent other buffers from displaying inside
          (set-window-dedicated-p w t)
          )))))
(add-hook 'compilation-mode-hook 'ct/create-proper-compilation-window)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (run-at-time nil 300 'mu4e-update-index) 
(use-package mu4e
  :defer t
  :ensure nil
  :config
  (setq mail-user-agent 'mu4e-user-agent)

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  (setq
   send-mail-function 'smtpmail-send-it
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

  ;; (setq mu4e-hide-index-messages t)
  (setq mu4e-headers-include-related nil)
  (setq mu4e-update-interval 120)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-attachment-dir "/tmp/")
  (setq mu4e-maildir "~/mail")


  (setq message-kill-buffer-on-exit t)
  (setq mu4e-sent-messages-behavior 'sent)

  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "jbenma"
            :enter-func (lambda () (mu4e-message "Gmail mode"))
            :leave-func (lambda () (mu4e-message "Leaving Gmail mode"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg '(:from :to :cc :bcc) "jorgebenma@gmail.com")))

            :vars '((user-mail-address . "jorgebenma@gmail.com")
                    (user-full-name    . "Jorge Benavides M.")
                    (mu4e-drafts-folder  . "/jorgebenma/[Gmail].Borradores")
                    (mu4e-sent-folder  . "/jorgebenma/[Gmail].Enviados")
                    (mu4e-refile-folder  . "/jorgebenma/INBOX")
                    (mu4e-trash-folder  . "/jorgebenma/[Gmail].Papelera")
                    (mu4e-compose-signature . (concat
                                               "Jorge Benavides M.\n"
                                               "Estudiante de Ingeniería en electrónica, robótica y mecatrónica\n"
                                               "\n"))
                    (mu4e-sent-messages-behavior . sent)
                    (mu4e-maildir-shortcuts . ( ("/jorgebenma/INBOX"    . ?i)
                                                ("/jorgebenma/[Gmail].Enviados" . ?s)
                                                ("/jorgebenma/[Gmail].Papelera"    . ?t)
                                                ("/jorgebenma/[Gmail].Borradores"   . ?d)
                                                ))))

          ,(make-mu4e-context
            :name "rhyloot"
            :enter-func (lambda () (mu4e-message "Gmail mode"))
            :leave-func (lambda () (mu4e-message "Leaving Gmail mode"))
            :match-func (lambda (msg)
                          (when msg
                            (mu4e-message-contact-field-matches
                             msg '(:from :to :cc :bcc) "rhyloot@gmail.com")))

            :vars '((user-mail-address . "rhyloot@gmail.com")
                    (user-full-name    . "rhyloot")
                    (mu4e-drafts-folder  . "/rhyloot/[Gmail].Borradores")
                    (mu4e-sent-folder  . "/rhyloot/[Gmail].Enviados")
                    (mu4e-refile-folder  . "/rhyloot/INBOX")
                    (mu4e-trash-folder  . "/rhyloot/[Gmail].Papelera")
                    (mu4e-compose-signature . (concat
                                               "Rhyloot\n"
                                               "Estudiante de Ingeniería en electrónica, robótica y mecatrónica\n"
                                               "\n"))
                    (mu4e-sent-messages-behavior . sent)
                    (mu4e-maildir-shortcuts . ( ("/rhyloot/INBOX"    . ?i)
                                                ("/rhyloot/[Gmail].Enviados" . ?s)
                                                ("/rhyloot/[Gmail].Papelera"    . ?t)
                                                ("/rhyloot/[Gmail].Borradores"   . ?d)
                                                ))))))
  (setq mu4e-context-policy 'pick-first)
  (setq mail-user-agent 'mu4e-user-agent)
  (mu4e t))

;; (use-package mu4e-send-delay
;;   :ensure nil
;;   :load-path "~/.emacs.d/private/packages/mu4e-send-delay"
;;   :after (mu4e)
;;   :config
;;   (mu4e-send-delay-setup) ; sets up headers to show up by default
;;   (add-hook 'mu4e-main-mode-hook 'mu4e-send-delay-initialize-send-queue-timer)
;;   (add-hook 'mu4e-main-mode-hook (lambda ()
;;                                    (define-key mu4e-compose-mode-map
;;                                      (kbd "C-c C-c")
;;                                      'mu4e-send-delay-send-and-exit))))

;; (mu4e-alert-set-default-style 'notifications)
(mu4e-alert-set-default-style 'libnotify)
(setq mu4e-alert-max-messages-to-process 2000)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(use-package mu4e-alert
  :defer t
  :after mu4e
  :init
  ;; (setq mu4e-alert-interesting-mail-query
  ;;       (concat
  ;;        "flag:unread maildir:/INBOX"))
  ;; (mu4e-alert-enable-mode-line-display)
  ;; (defun my/mu4e-alert ()
  ;;   (interactive)
  ;;   (mu4e~proc-kill)
  ;;   (mu4e-alert-enable-mode-line-display)
  ;;   )
  ;; (run-with-timer 0 2700 'my/mu4e-alert)
  ;; ;; (setq mu4e-alert-enable-notifications t)
  ;; ;; :config
  ;; ;; (mu4e-alert-set-default-style 'libnotify)
  ;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  ;; (setq mu4e-alert-notify-repeated-mails t)
  ;; (setq mu4e-alert-enable-notifications t)
  ;; (mu4e-alert-enable-mode-line-display)
  )

;; (fset 'sync-tasks
;;       (kmacro-lambda-form [?\M-x ?o ?r ?g ?- ?g ?t ?a ?s ?k ?s return return return return] 0 "%d"))

;; (setq tasks-names '("/home/rhyloo/.emacs.d/gtasks/Mis tareas.org"))

;; (defun my/sync-tasks ()
;;   (if (member (buffer-file-name) tasks-names)
;;       'sync-tasks)
;;   )


;; (add-hook 'after-save-hook 'my/sync-tasks)

;; Delete macro
;; (fmakunbound 'name-of-macro)

;; (use-package elmacro
;;   :defer t
;;   :config
;;   (elmacro-mode))

(put 'dired-find-alternate-file 'disabled nil)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(setq auth-sources '("~/.authinfo"))
(use-package forge
  :defer t
  :after (magit))

(use-package magit-pretty-graph
  :ensure nil
  :load-path "~/.emacs.d/private/packages/magit-pretty-graph"
  :after (magit))

(use-package minions
  :defer t
  :config
  (add-hook 'after-init-hook (minions-mode 1)))

(use-package doom-modeline
  :defer t
  :hook 
  (after-init . doom-modeline-mode))
  :config
(setq doom-modeline-bar-width 4)
(setq doom-modeline-window-width-limit 35)
(setq doom-modeline-buffer-name t)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-lsp t)
(setq doom-modeline-github-interval (* 30 60))
;; (setq doom-modeline-height 20)
;; (setq doom-modeline-mu4e nil)
;; (mu4e-alert-enable-mode-line-display)
;; (setq doom-modeline-gnus t)
;; (setq doom-modeline-gnus-timer 2)

(use-package all-the-icons
  :defer t
  :if (display-graphic-p))

(use-package undo-tree
  :defer t
  :hook 
  (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "/tmp/")))
  (undo-tree-visualizer-timestamps t))

(use-package swiper
    :defer t
    :bind 
    ("C-s" . swiper-isearch)
    :hook 
    (after-init . ivy-mode)
    :config
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))

(use-package counsel
  :defer t
  :bind     
  ("M-x" . counsel-M-x))

(use-package lsp-ltex
  :defer t
  :hook (tex-mode . (lambda ()
                       ;; (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0"))  ; make sure you have set this, see below

(use-package arduino-mode
  :defer t)
(use-package company-arduino
  :defer t)

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package vhdl-mode
  :defer t)

(flycheck-define-checker vhdl-tool
  "A VHDL syntax checker, type checker and linter using VHDL-Tool.

See URL `http://vhdltool.com'."
  :command ("vhdl-tool" "client" "lint" "--compact" "--stdin" "-f" source
            )
  :standard-input t
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":w:" (message) line-end)
   (error line-start (file-name) ":" line ":" column ":e:" (message) line-end))
  :modes (vhdl-mode))

(add-to-list 'flycheck-checkers 'vhdl-tool)

(defun efs/lsp-mode-setup()
  (setq lsp-headerline-breadcrumb-sefments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-modeline-diagnostics-enable t)
  ;; (setq lsp-vhdl-server-path "/home/rhyloo/.local/Software/vhdl-tool")
  (add-hook 'vhdl-mode-hook 'lsp))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lua-mode
  :defer t)

(use-package pyvenv
  :defer t
  :config
  (pyvenv-mode 1))

(use-package python-mode
  :defer t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))

(use-package matlab-mode
  :defer t
  :mode "\\.m\\'")

(setq matlab-shell-command-switches '("-nodesktop" "-softwareopengl"))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex))

;; ;;Auctex highlight syntax
(use-package auctex
  :defer t)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package json-mode
:defer t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))
(custom-set-variables
 '(markdown-command "/usr/bin/markdown")
 )

;; (use-package org-gtasks
;;   :defer t
;;   :load-path "~/.emacs.d/private/packages/org-gtasks"
;;   :config
;;   (let*
;;       ((autent
;;         (car
;;          (auth-source-search :host "www.gmail.com"
;;                            :requires '(client-id client-secret))))
;;   (mi-usuario (plist-get autent :client-id))
;;    (mi-password  (plist-get autent :client-secret)))
;;    (setq clientid mi-usuario)
;;    (setq clientsecret mi-password))

;;   (org-gtasks-register-account :name "jorgebenma"
;;                                :directory "~/.emacs.d/gtasks/"
;;                                :login "jorgebenma@gmail.com"
;;                                :client-id clientid
;;                                :client-secret clientsecret))

(add-hook 'after-init-hook 'global-company-mode)

;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(global-set-key (kbd "C-x 4") 'window-toggle-split-direction)

;; Session evaluation of MATLAB in org-babel is broken, this goes some
;; way towards addressing the problem.
;;
;;- I replaced a `delq' with `delete', the `eq' test was failing on
;; blank strings
;;
;;- For results of type `output', concatenate all statements in the
;; block with appropriate separators (";", "," etc) and run one long
;; statment instead. Remove this statement from the raw result. This
;; produces much cleaner output.

(defun org-babel-octave-evaluate-session
    (session body result-type &optional matlabp)
  "Evaluate BODY in SESSION."
  (let* ((tmp-file (org-babel-temp-file (if matlabp "matlab-" "octave-")))
     (wait-file (org-babel-temp-file "matlab-emacs-link-wait-signal-"))
     (full-body
      (pcase result-type
        (`output
         (mapconcat
          #'org-babel-chomp
          (list (if matlabp
                        (multi-replace-regexp-in-string
                         '(("%.*$"                      . "")    ;Remove comments
                           (";\\s-*\n+"                 . "; ")  ;Concatenate lines
                           ("\\(\\.\\)\\{3\\}\\s-*\n+"  . " ")   ;Handle continuations
                           (",*\\s-*\n+"                . ", ")) ;Concatenate lines
                         body)
                      body)
                    org-babel-octave-eoe-indicator) "\n"))
        (`value
         (if (and matlabp org-babel-matlab-with-emacs-link)
         (concat
          (format org-babel-matlab-emacs-link-wrapper-method
              body
              (org-babel-process-file-name tmp-file 'noquote)
              (org-babel-process-file-name tmp-file 'noquote) wait-file) "\n")
           (mapconcat
        #'org-babel-chomp
        (list (format org-babel-octave-wrapper-method
                  body
                  (org-babel-process-file-name tmp-file 'noquote)
                  (org-babel-process-file-name tmp-file 'noquote))
              org-babel-octave-eoe-indicator) "\n")))))
     (raw (if (and matlabp org-babel-matlab-with-emacs-link)
          (save-window-excursion
            (with-temp-buffer
              (insert full-body)
              (write-region "" 'ignored wait-file nil nil nil 'excl)
              (matlab-shell-run-region (point-min) (point-max))
              (message "Waiting for Matlab Emacs Link")
              (while (file-exists-p wait-file) (sit-for 0.01))
              "")) ;; matlab-shell-run-region doesn't seem to
        ;; make *matlab* buffer contents easily
        ;; available, so :results output currently
        ;; won't work
        (org-babel-comint-with-output
            (session
             (if matlabp
             org-babel-octave-eoe-indicator
               org-babel-octave-eoe-output)
             t full-body)
          (insert full-body) (comint-send-input nil t)))) results)
    (pcase result-type
      (`value
       (org-babel-octave-import-elisp-from-file tmp-file))
      (`output
       (setq results
         (if matlabp
         (cdr (reverse (delete "" (mapcar #'org-strip-quotes
                          (mapcar #'org-trim (remove-car-upto-newline raw))))))
           (cdr (member org-babel-octave-eoe-output
                (reverse (mapcar #'org-strip-quotes
                         (mapcar #'org-trim raw)))))))
       (mapconcat #'identity (reverse results) "\n")))))

(defun remove-car-upto-newline (raw)
  "Truncate the first string in a list of strings `RAW' up to the first newline"
  (cons (mapconcat #'identity
                   (cdr (split-string-and-unquote (car raw) "\n"))
                   "\n") (cdr raw)))

(defun multi-replace-regexp-in-string (replacements-list string &optional rest)
  (interactive)
  "Replace multiple regexps in a string. Order matters."
  (if (null replacements-list)
      string
    (let ((regex (caar replacements-list))
          (replacement (cdar replacements-list)))
      (multi-replace-regexp-in-string (cdr replacements-list)
                                      (replace-regexp-in-string regex replacement
                                                                string rest)))))
