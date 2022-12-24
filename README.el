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

(package-initialize)

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
      user-mail-address "jorge2@uma.es")

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
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")))

;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)")
;;         (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;;         (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:weight normal :box (:line-width 1 :color (\, red) :style nil) :foreground "yellow"))
        ))

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

(setq-default tab-width 2) ;; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width) ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
(delete-selection-mode 1) ;; Let you select and replace with yank or write

(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory
(setq read-file-name-completion-ignore-case t) ;; Insensitive letter case
(setq large-file-warning-threshold nil)        ;; Dont warn for large files
(fset 'yes-or-no-p 'y-or-n-p)                  ;; Replace yes or no for y or n

(global-auto-revert-mode 1)  ;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t)    ;; Revert Dired and other buffers

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

(put 'dired-find-alternate-file 'disabled nil)

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
  (after-init . global-undo-tree-mode))

(use-package swiper
    :defer t
    :bind 
    ("C-s" . swiper-isearch)
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t))

(use-package counsel
  :defer t
  :bind     
  ("M-x" . counsel-M-x))

(use-package lsp-ltex
  :defer t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0"))  ; make sure you have set this, see below

(use-package arduino-mode
  :defer t)
(use-package company-arduino
  :defer t)

(use-package flycheck
  :defer t
  :ensure t
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
  (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-vhdl-server-path "/home/rhyloo/.local/Software/vhdl-tool")
  (add-hook 'vhdl-mode-hook 'lsp))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lua-mode
  :defer t)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

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


