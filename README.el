;; -*- lexical-binding: t; -*-

(add-hook 'emacs-startup-hook
	        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
	                   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)))

;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold most-positive-fixnum)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restaurar valores tras el inicio
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  file-name-handler-alist default-file-name-handler-alist)))

(setq user-full-name "Rhyloo"  ;; Set my name
      user-mail-address "me@rhyloo.com"
      user-real-login-name "Rhyloo")    ;; Set my user

(setq my-user-init-file "README.org")
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" "~/.emacs.d/.authinfo"))
(setq backup-directory-alist `(("." . "~/.cache/emacs/backups"))) ;;Backup directory

(defun my/find-emacs-configuration ()
  (interactive)
  (find-file (concat user-emacs-directory my-user-init-file)))

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/get-pwd ()
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

(defun my/create-temp ()
  "Permite crear directorios o archivos en /tmp con completado de estilo counsel/find-file."
  (interactive)
  (let* ((choices '("file" "directory"))
         (type (completing-read "Create (file/directory): " choices nil t))
         ;; read-file-name permite navegar y autocompletar
         (path (read-file-name "Name: " "/tmp/")))

    (if (string-equal type "directory")
        (progn
          (make-directory path t)
          (dired path))
      (find-file path))
    (message "Created %s: %s" type path)))

(defun my/find-file (filename)
  "Open a file in the background"
  (interactive "FFind file: ")
  (set-buffer (find-file-noselect filename)))

(defun my/enable-line-numbers ()
  "Activar números de línea solo en GUI y modos específicos."
  (when (display-graphic-p)
    (display-line-numbers-mode 1)))

(defun my/update-last-modified ()
  "Actualizar la clave 'last_modified' en el encabezado de Org-mode al guardar."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified:.*" nil t)
        (replace-match (format "#+last_modified: %s" (format-time-string "%Y-%m-%d %H:%M:%S")))))))

(defun my/enable-undo-tree-once ()
  (when buffer-file-name
    (global-undo-tree-mode 1)
    (remove-hook 'find-file-hook #'my/enable-undo-tree-once)))

;;; -----------------------------------------------------------
(defun y-or-n-p-with-return (orig-func &rest args)
  (let ((query-replace-map (copy-keymap query-replace-map)))
    (define-key query-replace-map (kbd "RET") 'act)
    (apply orig-func args)))

(defun ar/compile-autoclose (buffer string)
  "Handle compilation window based on success or failure."
  (if (string-match "finished" string)
      ;; SUCCESS CASE
      (progn
        (message "Build finished :)")
        (run-with-timer 1 nil
                        (lambda ()
                          (when-let* ((multi-window (> (count-windows) 1))
                                      (live (buffer-live-p buffer))
                                      (window (get-buffer-window buffer t)))
                            (delete-window window)))))
    ;; FAILURE CASE
    (progn
      (message "Compilation Failed: %s" string)
      (when-let ((window (get-buffer-window buffer t)))
        (select-window window)      ;; This moves your cursor to the window
        (goto-char (point-max)))))) ;; Ensure you are looking at the error output

(defun ar/colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

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

;; https://adamoudad.github.io/posts/emacs/remote-command-ssh/
;; https://oremacs.com/2015/01/12/dired-file-size/
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      ;; Obtener el nombre del host remoto
      (let ((remote-hostname (shell-command-to-string "hostname")))

        ;; Eliminar posibles saltos de línea al final
        (setq remote-hostname (string-trim remote-hostname))

        ;; Dependiendo del nombre de la máquina, ejecutamos diferentes comandos
        (cond
         ;; Caso 1: Si estamos en la máquina local con nombre "DESKTOP-O45GL2P"
         ((or           (string= remote-hostname "DESKTOP-AGD6PUD") 
                        (string= remote-hostname "thinkpad")) 
          (apply 'call-process "du" nil t nil "-sch" files)
          (message "Output of du: %s" (buffer-string)))

         ;; Caso 2: Si estamos en el servidor remoto "debian"
         ((string= remote-hostname "debian")
          (let ((default-directory (expand-file-name my-remote-path)))
            (let* ((cleaned-files
                    (mapcar (lambda (file)
                              (replace-regexp-in-string my-remote-path-mod "" file))
                            files))
                   (du-output (shell-command-to-string (concat "du -sch " (mapconcat 'identity cleaned-files " ")))))
              ;; Mostrar la salida en el buffer de mensajes
              (message "Output of du: %s" du-output))))

         ;; Caso 3: Si estamos en otra máquina, por ejemplo, "other-server"
         ((string= remote-hostname "other-server")
          (let ((default-directory (expand-file-name "/ssh:user@other-server:/path/to/directory")))
            (let ((du-output (shell-command-to-string "du -sch /path/to/directory")))
              ;; Mostrar la salida en el buffer de mensajes
              (message "Output of du: %s" du-output))))

         ;; Si el nombre del host no coincide con los anteriores
         (t
          (message "No se ha definido un comando para esta máquina.")))))))

(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

(setq inhibit-startup-message t)                        ;; Avoid startup message
(setq frame-title-format '("%b [%m]"))                  ;; Title bar name
(setq ring-bell-function 'ignore)                       ;; Remove bell ring
(if (display-graphic-p)                                 ;; Highlight lines
    (global-hl-line-mode 1)      
  (global-hl-line-mode 0))

(when (display-graphic-p) 
  (scroll-bar-mode -1)                                  ;; Disable visible scrollbar
  (tool-bar-mode -1)                                    ;; Disable the toolbar
  (tooltip-mode -1)                                     ;; Disable tooltips
  (set-fringe-mode 3)                                   ;; Give some breathing room (borders)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(menu-bar-mode -1)                                      ;; Disable the menu bar terminal and display mode

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))     ;; one line at a time
(setq mouse-wheel-progressive-speed nil)                ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                      ;; scroll window under mouse
(setq scroll-step 1)                                    ;; keyboard scroll one line at a time

(show-paren-mode 1)                                     ;; Show parens
(global-visual-line-mode 1)                             ;; Better than fix the lines with set-fill-column
(delete-selection-mode 1)                               ;; Let you select and replace with yank or write
(add-hook 'prog-mode-hook #'subword-mode) 
(add-hook 'org-mode-hook #'subword-mode)

(add-hook 'shell-mode-hook
          (lambda () (local-set-key (kbd "C-l") #'comint-clear-buffer)))

(global-auto-revert-mode 1)                          ;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t)         ;; Revert Dired and other buffers
;; (setq auto-revert-remote-files nil)                    ;; Revert buffer in remote (SLOW)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-dwim-target t)                             ;; Allow you move files splitting the window
(setq dired-listing-switches "-la")
(setq read-file-name-completion-ignore-case t)        ;; Insensitive letter case
(setq large-file-warning-threshold nil)               ;; Dont warn for large files

;; Show file size
(if (display-graphic-p)
    (setq dired-listing-switches "-alh")
  (setq dired-listing-switches "-alhF"))

(custom-set-faces
 '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold)))))

;; Dired always use the same buffer. 
(when (>= emacs-major-version 28)
  (setq dired-kill-when-opening-new-dired-buffer t))
(when (< emacs-major-version 28)
  (progn
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
    ))


(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "z") 'dired-get-size))

(advice-add 'y-or-n-p :around #'y-or-n-p-with-return)
(fset 'yes-or-no-p 'y-or-n-p)                           ;; Replace yes or no for y or n
(setq enable-recursive-minibuffers t)
(setq electric-pair-mode t)

(setq display-time-format "%H:%M %d %b %y" display-time-default-load-average nil) ;; Show hour minute day month and year
(setq display-time-day-and-date t display-time-24hr-format t) ;; Change format to 24h
(display-time)               ;; Show the time in the bar

(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(setq-default mode-line-format
              (append mode-line-format
                      (list
                       '(:eval (if (use-region-p)
                                   (format " W:%d C:%d L:%d"
                                           (count-words-region (region-beginning) (region-end))
                                           (- (region-end) (region-beginning))
                                           (count-lines (region-beginning) (region-end)))
                                 "")))))
(column-number-mode)                                    ;; Show collumn in modeline

(which-function-mode 1)
(custom-set-faces
 '(which-func
   ((((class color)
      (min-colors 88)
      (background light))
     (:inherit
      (font-lock-function-name-face)))
    (((class grayscale mono)
      (background dark))
     (:inherit
      (font-lock-function-name-face)))
    (((class color)
      (background light))
     (:inherit
      (font-lock-function-name-face)))
    (((class color)
      (min-colors 88)
      (background dark))
     (:foreground "green"))
    (((background dark))
     (:foreground "red"))
    (t
     (:foreground "red")))))

(setq-default tab-width 2)                           ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil)                  ;; Use spaces instead of tabs for indentation

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)
(setq compilation-max-output-line-length nil)
(setq compilation-window-height 10)
(setq compilation-finish-functions #'ar/compile-autoclose)
(add-hook 'compilation-filter-hook 'ar/colorize-compilation-buffer)
(add-hook 'compilation-mode-hook 'ct/create-proper-compilation-window)

(setq gnus-home-directory "~/.emacs.d/")
(setq mail-signature-file "~/.emacs.d/.signature")
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)
;; (connection-local-set-profile-variables
;;  'remote-direct-async-process
;;  '((tramp-direct-async-process . t)))

;; (connection-local-set-profiles
;;  '(:application tramp :protocol "scp")
;;  'remote-direct-async-process)

;; (setq magit-tramp-pipe-stty-settings 'pty)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "<f1>") 'my/find-emacs-configuration)
(global-set-key (kbd "<f5>") 'my/reload-emacs-configuration)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f9>") 'my/get-pwd)
(global-set-key (kbd "\C-c M-+") 'my/create-temp)

(global-set-key (kbd "C-c l") 'counsel-locate)
(global-set-key (kbd "C-x q") 'compile)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "M-+") 'dired-create-empty-file)

(setq use-package-always-ensure nil
      use-package-always-defer t)

(use-package emacs
  :demand t
  :init
  (prefer-coding-system 'utf-8)
  :hook ((prog-mode text-mode conf-mode matlab-mode lisp-mode) . my/enable-line-numbers))

(use-package vscode-dark-plus-theme
  :ensure t
  :demand t
  :init
  (when (display-graphic-p)
    (load-theme 'vscode-dark-plus t)))

(use-package org
  :ensure t
  :demand t
  :bind (("C-c a" . org-agenda))
  :hook ((org-mode . (lambda () 
                       (display-line-numbers-mode -1)
                       (when (display-graphic-p) (org-indent-mode 1)))))
  :config
  ;; --- General Org Settings ---
  (setq org-startup-folded t
        org-adapt-indentation t
        org-odd-levels-only nil
        org-cycle-separator-lines 0
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-hide-leading-stars t
        org-fold-core-style 'overlays)

  ;; --- Images and Source Code ---
  (setq org-startup-with-inline-images nil
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 1
        org-confirm-babel-evaluate nil)

  ;; --- Keyword and states ---
  (setq org-todo-keywords
	      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
	        (sequence "EXPERIMENTAL(e)" "FAIL(f)" "|" "WORKS(w)")))

  (setq org-todo-keyword-faces
	      '(("IN-PROGRESS" . (:weight normal :box (:line-width 1 :color (\, yellow) :style nil) :foreground "yellow"))
	        ("WAITING" . (:weight normal :box (:line-width 1 :color (\, pink) :style nil) :foreground "pink"))
	        ("EXPERIMENTAL" . (:weight normal :box (:line-width 1 :color (\, white) :style nil) :foreground "white"))
	        ("WORKS" . (:weight normal :box (:line-width 1 :color (\, green) :style nil) :foreground "green"))
	        ("FAIL" . (:weight normal :box (:line-width 1 :color (\, red) :style nil) :foreground "red"))))  
  
  ;; --- Agenda Settings ---
  (setq org-agenda-files '("~/Documents/org-mode-files/Agenda.org")
        org-agenda-block-separator 61
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'only-window)
  ;; Cambia el color de la agenda al día de hoy.
  (custom-set-faces
   '(org-agenda-date-today ((t (:background "yellow" :weight bold)))))


  ;; --- LaTeX Export (XeLaTeX + Minted) ---
  (with-eval-after-load 'ox-latex
    (setq org-latex-compiler "xelatex"
          org-latex-listings 'minted
          org-latex-packages-alist '(("outputdir=./build" "minted" nil))
          org-latex-pdf-process 
          '("mkdir -p build"
            "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o/build %f"
            "mv %o/build/%b.pdf %O"))

    (add-to-list 'org-latex-classes
                 '("reporti"
                   "\\documentclass{reporti} [NO-DEFAULT-PACKAGES] [NO-PACKAGES]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  ;; --- Automatic Updates ---
  (add-hook 'before-save-hook #'my/update-last-modified))

(use-package ob-python
  :ensure nil
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-shell
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash))

(use-package ob-js
  :ensure nil
  :defer t
  :commands (org-babel-execute:js))

(use-package ob-octave
  :ensure nil
  :defer t
  :commands (org-babel-execute:octave))

(use-package ob-css
  :ensure nil
  :defer t
  :commands (org-babel-execute:css))

(use-package ob-dot
  :ensure nil  
  :defer t
  :commands (org-babel-execute:dot))

(use-package ob-latex
  :ensure nil
  :defer t
  :commands (org-babel-execute:latex))

(use-package ob-lua
  :ensure nil  
  :defer t
  :commands (org-babel-execute:lua))

(use-package ob-C
  :ensure nil  
  :defer t
  :commands
  (org-babel-execute:C
   org-babel-expand-body:C
   org-babel-execute:C++
   org-babel-expand-body:C++))

(use-package ob-matlab
  :ensure nil
  :defer t
  :commands (org-babel-execute:matlab))

(use-package ivy
  :ensure t
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "))

(use-package swiper
  :ensure t
  :demand t
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable))
  :config
  (setq counsel-find-file-ignore-regexp "\\.elc\\|\\.pyc"))

(use-package llama
  :defer t
  :ensure t)

(use-package magit
  :ensure t
  :after (llama transient)
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

(use-package undo-tree
  :demand t
  :ensure t
  :defer t
  :commands (global-undo-tree-mode)
  :init
  (add-hook 'find-file-hook #'my/enable-undo-tree-once)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "/tmp/")))
  (undo-tree-visualizer-timestamps t))

;; -----------------------------------------------------------------------------
;; PAQUETES GLOBALES (Usamos :demand t para que funcionen siempre)
;; -----------------------------------------------------------------------------

;; Cierre automático de paréntesis (Arregla tu error anterior)
(use-package elec-pair
  :ensure nil
  :demand t
  :init (electric-pair-mode 1))

(use-package yasnippet
  :ensure t
  :demand t
  :config (yas-global-mode 1))

(use-package company
  :ensure t
  :demand t
  :init (global-company-mode 1)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1))

(use-package flycheck
  :ensure t
  :demand t
  :init (global-flycheck-mode 1))

;; -----------------------------------------------------------------------------
;; MODOS DE LENGUAJE (Carga diferida automática para velocidad)
;; -----------------------------------------------------------------------------

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map ("RET" . newline-and-indent))
  :config
  (setq-default c-basic-offset 4)
  (setq c-default-style "linux")
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4))

(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("bashrc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 4))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; -----------------------------------------------------------------------------
;; LSP (Se activa por hooks, no necesita :demand t)
;; -----------------------------------------------------------------------------

(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp)
         (sh-mode . lsp))
  :config
  (setq lsp-enable-snippet t
        lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(setq gc-cons-threshold (* 2 1000 1000)) ;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-percentage 0.5)

(run-with-idle-timer 60 t #'garbage-collect)
