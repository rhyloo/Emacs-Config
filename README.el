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

(setq org-confirm-babel-evaluate nil) ;; Stop the confirmation to evaluate org babel
(setq org-src-tab-acts-natively t)    ;; Indent code in org-babel

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

;; ;; FUNCION PARA CREAR ARCHIVOS TEMPORALES, PARA PROBAR COSAS O ESCRIBIR x COSAS
;; (lambda ()
;;   (with-temp-buffer
;;     (setq temp-file-name (read-string "Temporary file name: "))
;;     (message temp-file-name)
;;     (find-file (concat "/tmp/" temp-file-name))))

(use-package minions
  :defer t
  :init
  (add-hook 'after-init-hook (minions-mode 1)))


