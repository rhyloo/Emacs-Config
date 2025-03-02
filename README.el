(add-hook 'emacs-startup-hook
	        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
	                   (format "%.2f seconds"
		                         (float-time
			                        (time-subtract after-init-time before-init-time)))
	                   gcs-done)))

(setq gc-cons-threshold (* 50 1000 1000)) ;; Minimize garbage collection during startup

(setq user-full-name "J. L. Benavides"  ;; Set my name
      user-real-login-name "Rhyloo")    ;; Set my user

(setq inhibit-startup-message t)                        ;; Avoid startup message
(setq frame-title-format '("%b [%m]"))                  ;; Title bar name
(when (display-graphic-p) 
  (scroll-bar-mode -1)                                  ;; Disable visible scrollbar
  (tool-bar-mode -1)                                    ;; Disable the toolbar
  (tooltip-mode -1)                                     ;; Disable tooltips
  (set-fringe-mode 3)                                   ;; Give some breathing room (borders)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  )
(menu-bar-mode -1)                                      ;; Disable the menu bar terminal and display mode

(setq display-time-format "%H:%M %d %b %y" display-time-default-load-average nil) ;; Show hour minute day month and year
(setq display-time-day-and-date t display-time-24hr-format t) ;; Change format to 24h
(display-time)               ;; Show the time in the bar

(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(column-number-mode)                                    ;; Show collumn in modeline
(setq-default mode-line-format
              (append mode-line-format
                      (list
                       '(:eval (if (use-region-p)
                                   (format "W:%d, C:%d"
                                           (count-words-region (region-beginning) (region-end))
                                           (- (region-end) (region-beginning)))
                                 "")))))

(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

(setq ring-bell-function 'ignore)                       ;; Remove bell ring
(if (display-graphic-p)                                 ;; Highlight lines
    (global-hl-line-mode 1)      
  (global-hl-line-mode 0))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))     ;; one line at a time
(setq mouse-wheel-progressive-speed nil)                ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                      ;; scroll window under mouse
(setq scroll-step 1)                                    ;; keyboard scroll one line at a time

(show-paren-mode 1)                                     ;; Show parens
(global-visual-line-mode 1)  ;; Better than fix the lines with set-fill-column
(delete-selection-mode 1) ;; Let you select and replace with yank or write

(put 'dired-find-alternate-file 'disabled nil)

(setq package-archives                                           ;; Init package repositories.
      '(("gnu"          . "https://elpa.gnu.org/packages/")      ;; Set GNU repository
        ("melpa"        . "https://melpa.org/packages/")))       ;; Set Melpa repository
(unless package-archive-contents (package-refresh-contents))     ;; Are package archives up to date?
(unless (package-installed-p 'use-package)                       ;; Is 'use-package' installed?
  (package-install 'use-package)
  (setq use-package-always-ensure t))

;; Configuración optimizada de números de línea
(use-package emacs
  :init
  ;; ---------------------------
  ;; 1. Habilitar en modos padres (evita redundancia)
  ;; ---------------------------
  ;; text-mode-hook ya cubre org-mode, markdown-mode, etc.
  ;; prog-mode-hook cubre todos los lenguajes de programación
  (defun my/enable-line-numbers ()
    "Activar números de línea solo en GUI y modos específicos."
    (when (display-graphic-p)
    (display-line-numbers-mode 1)))

  ;; Añadir solo a los hooks principales
  (dolist (hook '(text-mode-hook prog-mode-hook conf-mode-hook))
    (add-hook hook #'my/enable-line-numbers :append))

  ;; ---------------------------
  ;; 2. Deshabilitar en modos específicos (más eficiente)
  ;; ---------------------------
  (defun my/disable-line-numbers ()
    "Desactivar números de línea donde no se necesitan."
    (display-line-numbers-mode -1))

  ;; Org Mode (sobrescribe text-mode-hook)
  (add-hook 'org-mode-hook #'my/disable-line-numbers :append))

(when (display-graphic-p)
  (use-package vscode-dark-plus-theme
    :defer t
    :init
    (add-hook 'after-init-hook (load-theme 'vscode-dark-plus t))))

(setq org-startup-folded t)
(setq org-return-follows-link 1)
(setq org-src-tab-acts-natively t)    ;; Indent code in org-babel

(use-package org
  :defer t
  :config
  (setq org-adapt-indentation t
        org-odd-levels-only nil
        org-src-preserve-indentation t
        org-edit-src-content-indentation 1
	org-startup-with-inline-images nil ;; Startup with inline images (disable)
	org-image-actual-width nil)
  ;; Carga org-indent-mode solo en GUI usando eval-after-load
  (when (display-graphic-p)
    (setq org-hide-leading-stars t)
    (with-eval-after-load 'org  ; Espera a que Org esté cargado
      (add-hook 'org-mode-hook 'org-indent-mode))))


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



(setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
	  (sequence "EXPERIMENTAL(e)" "FAIL(f)" "|" "WORKS(w)")))
(setq org-todo-keyword-faces
	'(("IN-PROGRESS" . (:weight normal :box (:line-width 1 :color (\, yellow) :style nil) :foreground "yellow"))
	  ("WAITING" . (:weight normal :box (:line-width 1 :color (\, pink) :style nil) :foreground "pink"))
	  ("EXPERIMENTAL" . (:weight normal :box (:line-width 1 :color (\, white) :style nil) :foreground "white"))
	  ("WORKS" . (:weight normal :box (:line-width 1 :color (\, green) :style nil) :foreground "green"))
	  ("FAIL" . (:weight normal :box (:line-width 1 :color (\, red) :style nil) :foreground "red"))))

(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil) ;; Stop the confirmation to evaluate org babel
(use-package ob-python
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-shell
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-js
  :defer t
  :commands (org-babel-execute:js))

(use-package ob-octave
  :defer t
  :commands (org-babel-execute:octave))

(use-package ob-css
  :defer t
  :commands (org-babel-execute:css))

(use-package ob-dot
  :defer t
  :commands (org-babel-execute:dot))

(use-package ob-latex
  :defer t
  :commands (org-babel-execute:latex))

(use-package ob-lua
  :defer t
  :commands (org-babel-execute:lua))

(use-package ob-C
  :defer t
  :commands
  (org-babel-execute:C
   org-babel-expand-body:C
   org-babel-execute:C++
   org-babel-expand-body:C++))

(use-package ob-matlab
  :defer t
  :commands (org-babel-execute:matlab))

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
(global-set-key (kbd "<f12>") 'list-bookmarks)
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

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; (setq auth-sources '("~/.authinfo"))
;; (use-package forge
;;   :defer t
;;   :after (magit))

;; (use-package magit-pretty-graph
;;   :ensure nil
;;   :load-path "~/.emacs.d/private/packages/magit-pretty-graph"
;;   :after (magit))

(use-package minions
  :defer t
  :hook (after-init . minions-mode))

(use-package undo-tree
  :defer t
  :commands (global-undo-tree-mode)
  :init
  (defun my/enable-undo-tree-once ()
    (when buffer-file-name
      (global-undo-tree-mode 1)
      (remove-hook 'find-file-hook #'my/enable-undo-tree-once)))
  (add-hook 'find-file-hook #'my/enable-undo-tree-once)
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

;; (use-package flycheck
;;   :defer t
;;   :init
;;   (global-flycheck-mode) ; Activa Flycheck globalmente
;;   :config
;;   ;; Añadir Proselint como checker para org-mode
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               ;; Opción 1: Usar solo Proselint (reemplaza otros checkers)
;;               (setq-local flycheck-checkers '(proselint))
              
;;               ;; Opción 2: Combinar Proselint con otros checkers (ej: spell)
;;               ;;(add-to-list 'flycheck-checkers 'proselint)
              
;;               (flycheck-mode))) ; Asegura que Flycheck esté activo
;;   )

(use-package writegood-mode  
  :ensure t)

(setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"))
(setq languagetool-console-command "~/fastText/LanguageTool-20250202-snapshot/LanguageTool-6.6-SNAPSHOT/languagetool-commandline.jar"
      languagetool-server-command "~/fastText/LanguageTool-20250202-snapshot/LanguageTool-6.6-SNAPSHOT/languagetool-server.jar")
(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop))

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
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Disable company-mode in shell-mode
  (add-hook 'shell-mode-hook (lambda () (company-mode -1)))
  (setq company-dabbrev-downcase nil)    ; Don't lowercase completions
  (setq company-dabbrev-ignore-case nil) ; Respect case
  )
