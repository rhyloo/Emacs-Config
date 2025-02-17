(setq gc-cons-threshold most-positive-fixnum) ;; Minimize garbage collection during startup

(setq gc-cons-threshold (expt 2 23)) ;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-percentage 0.5)

(run-with-idle-timer 5 t #'garbage-collect)

(add-hook 'emacs-startup-hook
	        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
	                   (format "%.2f seconds"
		                         (float-time
			                        (time-subtract after-init-time before-init-time)))
	                   gcs-done)))

;; Inicializa las fuentes de paquetes
(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")      ;; Repositorio oficial de GNU
        ("melpa-stable" . "https://stable.melpa.org/packages/") ;; Versión estable de MELPA
        ("ox-odt"       . "https://kjambunathan.github.io/elpa/") ;; Soporte adicional para Org (ox-odt)
        ("melpa"        . "https://melpa.org/packages/")))       ;; Repositorio principal de MELPA

;; Inicializa el sistema de paquetes
;; (package-initialize)

;; Asegúrate de que las fuentes estén actualizadas
(unless package-archive-contents
  (package-refresh-contents))

;; Instala y configura use-package para manejar los paquetes de forma modular
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq user-full-name "J. L. Benavides"
      user-real-login-name "Rhyloo")

(if (display-graphic-p)
  (progn 
  (scroll-bar-mode -1)         ;; Disable visible scrollbar
  (tool-bar-mode -1)           ;; Disable the toolbar
  (tooltip-mode -1)            ;; Disable tooltips
  (set-fringe-mode 15)         ;; Give some breathing room (borders)
  ))
(menu-bar-mode -1)           ;; Disable the menu bar
  (setq-default frame-title-format '("%b [%m]")) ;; Title bar name
  (setq inhibit-startup-message t) ;; Avoid startup message
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq ring-bell-function 'ignore) ;; Remove bell ring

(setq display-time-format "%H:%M %d %b %y" display-time-default-load-average nil) ;; Show hour minute day month and year
(setq display-time-day-and-date t display-time-24hr-format t) ;; Change format
(display-time)               ;; Show the time in the bar

(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(column-number-mode)                  ;; Enable column mode

(show-paren-mode 1)          ;; Show parens
(if (display-graphic-p)
    (progn
      (global-hl-line-mode 1)      ;; Highlight lines
      )
  (global-hl-line-mode 0))
(global-visual-line-mode 1)  ;; Better than fix the lines with set-fill-column
;; (windmove-default-keybindings 'M) ;; Move windows

(if (display-graphic-p)
    (progn
      (set-frame-parameter (selected-frame) 'alpha '(100 . 100))  ;; Set frame transparency
      (add-to-list 'default-frame-alist '(alpha . (100 . 100)))   ;; Set frame transparency
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized) ;; maximize windows by default.
      (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize windows by default.
      (use-package vscode-dark-plus-theme                         ;; Set theme VScode
        :defer t
        :init
        (add-hook 'after-init-hook (load-theme 'vscode-dark-plus t)))
      )
  )

(setq org-startup-folded t)
(setq org-return-follows-link 1)
;; (use-package org
;;   :defer t
;;   :pin gnu
;;   :hook
;;   ((before-save . zp/org-set-last-modified))
;;   :config)

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
	(sequence "EXPERIMENTAL(e)" "FAIL(f)" "|" "WORKS(w)")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . (:weight normal :box (:line-width 1 :color (\, yellow) :style nil) :foreground "yellow"))
	("WAITING" . (:weight normal :box (:line-width 1 :color (\, pink) :style nil) :foreground "pink"))
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
      org-odd-levels-only nil               ;; Org use only odd levels (disable)
      org-src-preserve-indentation nil      ;; Preserves the indentation of the source code in the src edit buffer
      org-edit-src-content-indentation 0)   ;; Respect parent buffer indentation

(if (display-graphic-p)
    (progn
      (add-hook 'org-mode-hook 'org-indent-mode)
      (setq org-hide-leading-stars t)               ;; Leading stars invisible
      ))


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

;; (use-package org-tidy
;;   :defer t
;;   :ensure nil
;;   :hook
;;   (org-mode . org-tidy-mode))

(setq-default tab-width 2) ;; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width) ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs for indentation
(delete-selection-mode 1) ;; Let you select and replace with yank or write
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory
(setq read-file-name-completion-ignore-case t) ;; Insensitive letter case
(setq large-file-warning-threshold nil)        ;; Dont warn for large files
(fset 'yes-or-no-p 'y-or-n-p)                  ;; Replace yes or no for y or n
(setq dired-listing-switches "-la")
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

(unless (display-graphic-p)
  (setq browse-url-browser-function 'eww-browse-url))

(put 'dired-find-alternate-file 'disabled nil)

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

(use-package pdf-tools
  :defer t
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

;; (defun window-toggle-split-direction ()
;;   "Switch window split from horizontally to vertically, or vice versa.
;; i.e. change right window to bottom, or change bottom window to right."
;;   (interactive)
;;   (require 'windmove)
;;   (let ((done))
;;     (dolist (dirs '((right . down) (down . right)))
;;       (unless done
;;         (let* ((win (selected-window))
;;                (nextdir (car dirs))
;;                (neighbour-dir (cdr dirs))
;;                (next-win (windmove-find-other-window nextdir win))
;;                (neighbour1 (windmove-find-other-window neighbour-dir win))
;;                (neighbour2 (if next-win (with-selected-window next-win
;;                                           (windmove-find-other-window neighbour-dir next-win)))))
;;           ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
;;           (setq done (and (eq neighbour1 neighbour2)
;;                           (not (eq (minibuffer-window) next-win))))
;;           (if done
;;               (let* ((other-buf (window-buffer next-win)))
;;                 (delete-window next-win)
;;                 (if (eq nextdir 'right)
;;                     (split-window-vertically)
;;                   (split-window-horizontally))
;;                 (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; (global-set-key (kbd "C-x 4") 'window-toggle-split-direction)

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

(with-eval-after-load "org"
  (define-key org-mode-map "\C-e" nil)
  (define-key org-mode-map [remap move-end-of-line] nil))


(setq org-tidy-protect-overlay nil)

;; (use-package languagetool
;;   :ensure t
;;   :defer t
;;   :commands (languagetool-check
;;              languagetool-clear-suggestions
;;              languagetool-correct-at-point
;;              languagetool-correct-buffer
;;              languagetool-set-language
;;              languagetool-server-mode
;;              languagetool-server-start
;;              languagetool-server-stop)
;;   :config
;;   (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;         languagetool-console-command "~/.local/bin/language-tools/LanguageTool-6.3-stable/languagetool-commandline.jar"
;;         languagetool-server-command "~/.local/bin/language-tools/LanguageTool-6.3-stable/languagetool-server.jar"))

(use-package multiple-cursors
  :ensure t
  :defer t)

(setq org-fold-core-style 'overlays)
(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)

        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@creative" . ?c)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)))

(bookmark-bmenu-list)
(switch-to-buffer "*Bookmark List*")

(setq-default mode-line-format
              (append mode-line-format
                      (list
                       '(:eval (if (use-region-p)
                                   (format "W:%d, C:%d"
                                           (count-words-region (region-beginning) (region-end))
                                           (- (region-end) (region-beginning)))
                                 "")))))

(use-package htmlize
  :ensure t)
(setq org-html-htmlize-output-type 'css)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)) ; Activar Yasnippet en todo Emacs
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"          ; Snippets personalizados
;;         yasnippet-snippets-dir))       ; Snippets de yasnippet-snippets

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
     (:foreground "white"))
    (((background dark))
     (:foreground "white"))
    (t
     (:foreground "white")))))

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'org-mode-hook #'subword-mode)



(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

;; Press [pause] key in each window you want to "freeze"
(global-set-key [(f10)] 'toggle-window-dedicated)

(setq display-buffer-alist
      '((".*" . ((display-buffer-reuse-window display-buffer-same-window)))))

(global-set-key (kbd "M-<f10>") (lambda () 
                                  (interactive)
                                  (setq window-size-fixed (not window-size-fixed))
                                  (if window-size-fixed
                                      (message "Window size is now fixed.")
                                    (message "Window size is now dynamic."))))

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
          (let ((default-directory (expand-file-name "/ssh:root@www.rhyloo.com:~/")))
            ;; Limpiar los nombres de archivos eliminando el prefijo "/ssh:root@www.rhyloo.com:"
            (let* ((cleaned-files
                    (mapcar (lambda (file)
                              (replace-regexp-in-string "^/ssh:root@www.rhyloo.com:" "" file))
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

(defun my/pdf-to-svg ()
  "Get as input a PDF file and return it as an SVG."
  (interactive)
  (shell-command (concat "inkscape " (read-file-name "File name: ") " --export-area-drawing --batch-process --export-type=svg --export-filename=" (read-from-minibuffer (concat "Name output file:")) ".svg&")))

(defun my/erc-start-or-switch ()
  "Conecta a ERC, o cambia al último búfer activo."
  (interactive)
  (if (get-buffer "irc.libera.chat:6667")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.libera.chat" :port 6667 :nick "rhyloo"))))

(defun my/erc-notify (nickname message)
  "Muestra un mensaje de notificación para ERC."
  (let* ((channel (buffer-name))
         (nick (erc-hl-nicks-trim-irc-nick nickname))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nick
                  (concat nick " (" channel ")")))
         (msg (s-trim (s-collapse-whitespace message))))
    (alert (concat nick ": " msg) :title title)))

(defun update-last-modified ()
  "Actualizar la clave 'last_modified' en el encabezado de Org-mode al guardar."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+last_modified:.*" nil t)
        (replace-match (format "#+last_modified: %s" (format-time-string "%Y-%m-%d %H:%M:%S")))))))

(add-hook 'before-save-hook 'update-last-modified)

(use-package writegood-mode  
  :ensure t)
