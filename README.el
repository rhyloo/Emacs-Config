(add-hook 'emacs-startup-hook
	        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
	                   (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)))

(setq gc-cons-threshold (* 50 1000 1000)) ;; Minimize garbage collection during startup

(setq user-full-name "Rhyloo"  ;; Set my name
      user-mail-address "me@rhyloo.com"
      user-real-login-name "Rhyloo")    ;; Set my user

(fset 'yes-or-no-p 'y-or-n-p)                           ;; Replace yes or no for y or n
(setq inhibit-startup-message t)                        ;; Avoid startup message
(setq frame-title-format '("%b [%m]"))                  ;; Title bar name
(when (display-graphic-p) 
  (scroll-bar-mode -1)                                  ;; Disable visible scrollbar
  (tool-bar-mode -1)                                    ;; Disable the toolbar
  (tooltip-mode -1)                                     ;; Disable tooltips
  (set-fringe-mode 3)                                   ;; Give some breathing room (borders)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))
(menu-bar-mode -1)                                      ;; Disable the menu bar terminal and display mode

(setq display-time-format "%H:%M %d %b %y" display-time-default-load-average nil) ;; Show hour minute day month and year
(setq display-time-day-and-date t display-time-24hr-format t) ;; Change format to 24h
(display-time)               ;; Show the time in the bar

(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(setq-default mode-line-format
              (append mode-line-format
                      (list
                       '(:eval (if (use-region-p)
                                   (format "W:%d, C:%d"
                                           (count-words-region (region-beginning) (region-end))
                                           (- (region-end) (region-beginning)))
                                 "")))))
(column-number-mode)                                    ;; Show collumn in modeline

(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc" "~/.emacs.d/.authinfo")) ;; Check this later

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
(add-hook 'prog-mode-hook #'subword-mode) 
(add-hook 'org-mode-hook #'subword-mode)

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
    :ensure t
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
	org-cycle-separator-lines 0
	org-src-tab-acts-natively t	
        org-src-preserve-indentation t
        org-edit-src-content-indentation 1
	org-startup-with-inline-images nil ;; Startup with inline images (disable)
	org-image-actual-width nil)
  ;; Carga org-indent-mode solo en GUI usando eval-after-load
  (when (display-graphic-p)
    (setq org-hide-leading-stars t)
    (with-eval-after-load 'org  ; Espera a que Org esté cargado
      (add-hook 'org-mode-hook 'org-indent-mode)))
  (with-eval-after-load 'ox-latex  
    (add-to-list 'org-latex-classes
		 '("reporti"
                   "\\documentclass{reporti}
                  [NO-DEFAULT-PACKAGES]
                  [NO-PACKAGES]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  
;; Prefer minted for source code export in LaTeX.
(setq org-latex-listings (quote minted))

;; Prefex `xelatex' as the LaTeX processor.
(setq org-latex-compiler "xelatex")

;; Make sure that LaTeX knows about the `minted' package: we take care
;; of it in `org-latex-packages-alist' and we do *NOT* want to include
;; it explicitly as a #+LATEX_HEADER, since the options may differ, in
;; which case the two inclusions will conflict.
(setq org-latex-packages-alist '(("outputdir=./build" "minted" nil)))

;; `org-latex-pdf-process' is a list of shell commands. We take advantage of that
;; to:
;;   - create the `build' subdirectory if it is not present
;;   - run `latexmk' with the proper options (in particular `-shell-escape' which i
;;     necessary in order to allow the LaTeX processor to run an external program,
;;     like `pygmentize' in the case of `minted'; and `-output-directory' to allow
;;     all the artifacts to be sent there)
;;   - finally, move the `.pdf' file to the parent directory of the `build' subdirectory
;;     so that the exporter will be able to find it and not complain.
;; Note also that `%latex' is replaced by the value of `org-latex-compiler' so we use
;; `xelatex' as our LaTeX processor.
(setq org-latex-pdf-process '("mkdir -p build"
                              "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o/build %f"
                              "mv %o/build/%b.pdf %O")))


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
;; Fix bug open tree
(setq org-fold-core-style 'overlays)

(setq org-agenda-files '("~/Documents/org-mode-files/Agenda.org"))
(setq org-agenda-block-separator 61)
(setq org-agenda-restore-windows-after-quit t)            
(setq org-agenda-window-setup 'only-window)

(defun update-last-modified ()
  "Actualizar la clave 'last_modified' en el encabezado de Org-mode al guardar."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified:.*" nil t)
        (replace-match (format "#+last_modified: %s" (format-time-string "%Y-%m-%d %H:%M:%S")))))))

(add-hook 'before-save-hook 'update-last-modified)

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
   org-babel-execute:bash))

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

(setq backup-directory-alist `(("." . "~/.emacs.d/.backups"))) ;;Backup directory
(setq read-file-name-completion-ignore-case t)        ;; Insensitive letter case
(setq large-file-warning-threshold nil)               ;; Dont warn for large files
(setq dired-dwim-target t)                             ;; Allow you move files splitting the window
(setq dired-listing-switches "-la")

(global-auto-revert-mode 1)                          ;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t)         ;; Revert Dired and other buffers

(setq auto-revert-remote-files nil)                    ;; Revert buffer in remote (SLOW)

(add-hook 'shell-mode-hook
          (lambda () (local-set-key (kbd "C-l") #'comint-clear-buffer)))

(setq-default tab-width 2)                           ;; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width)            ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil)                  ;; Use spaces instead of tabs for indentation
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun my/org-table-install-formulas ()
  "Install formulas in cells starting with = or := at the bottom of the table as #+TBLFM line.
Do nothing when point is not inside a table."
  (interactive)
  (when (org-table-p)
    (save-excursion
      (goto-char (org-table-begin))
      (org-table-next-field)
      (while (progn
               (org-table-maybe-eval-formula)
               (looking-at "[^|\n]*|\\([[:space:]]*\n[[:space:]]*|\\)?[^|\n]*\\(|\\)"))
        (goto-char (match-beginning 2)))
      ))
  nil)

(add-hook #'org-ctrl-c-ctrl-c-hook #'my/org-table-install-formulas)
(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/load-blog-configuration ()
  (interactive)
  (load-file "~/.emacs.d/blog.el"))

(setq my-user-init-file "README.org")
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

;; REVISAR
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
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package minions
  :ensure t
  :defer t
  :hook (after-init . minions-mode))

(use-package undo-tree
  :ensure t
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
  :ensure t
  :defer t
  :bind 
  ("C-s" . swiper-isearch)
  :hook 
  (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers nil)
  ;; (setq enable-recursive-minibuffers t)
  ;; (setopt ivy-use-selectable-prompt t)
  )
(use-package counsel
  :ensure t
  :defer t
  :bind     
  ("M-x" . counsel-M-x))

(use-package writegood-mode  
  :ensure t
  :defer t)

(use-package vhdl-mode
  :defer t)

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
  :ensure t
  :defer t  ; Load when needed, not at startup
  :init     ; Execute immediately (before package loads)
  (add-hook 'after-init-hook #'global-company-mode)
  :config   ; Execute after package loads
  (add-hook 'shell-mode-hook (lambda () (company-mode -1)))
  (setq company-dabbrev-downcase nil)    ; Preserve case in completions
  (setq company-dabbrev-ignore-case nil) ; Case-sensitive matching
  )

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

(defun my/ros-colcon-build ()
  "build project 1"
  (interactive)
  (let ((buf-name '"*jea-compile-project1*")
        (working-dir '"~/Documents/Universidad/CyPR/ROS/dev_ws/"))
    (save-excursion
      (with-current-buffer (get-buffer-create buf-name)
        (barf-if-buffer-read-only)
        (erase-buffer))
      (cd working-dir)
      (call-process-shell-command "colcon build" nil buf-name 't)
      (cd "~/coppelia_ws/")
      (call-process-shell-command "colcon build" nil buf-name 't)
      (message "compile project 1 done")
      )))
(global-set-key [(f10)] 'my/ros-colcon-build)

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
                        (string= remote-hostname "DESKTOP-O45GL2P")) 
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

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "z") 'dired-get-size))

(setq gnus-home-directory "~/.emacs.d/")
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
