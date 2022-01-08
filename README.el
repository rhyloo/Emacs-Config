;; Minimize garbage collection during startup
;; (setq gc-cons-threshold most-positive-fixnum)

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
(lambda ()
(message "*** Emacs loaded in %s with %d garbage collections."
(format "%.2f seconds"
(float-time
(time-subtract after-init-time before-init-time)))
gcs-done)))

(setq user-full-name "Rhyloo"
user-mail-address "rhyloot@gmail.com")

(require 'package)
(setq package-archives
  '(
  ("gnu"     .       "https://elpa.gnu.org/packages/")
  ("nongnu"     .       "https://elpa.nongnu.org/nongnu/")
  ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
  ("melpa" . "http://melpa.org/packages/")))

  (package-initialize)

(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))
 
(require 'use-package)
(setq use-package-always-ensure t)

(use-package org-make-toc
:defer t
;; :hook (org-mode . org-make-toc-mode)
)

(use-package matlab-mode
    :defer t)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package swiper
:bind ("C-s" . swiper))

;; ;;Auctex highlight syntax
(use-package auctex
  :defer t)

(use-package simple-httpd
  :defer t)

;; ;;Company-mode
(use-package company
:config
(add-hook 'after-init-hook 'global-company-mode))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t
      TeX-source-correlate-method 'synctex))

(use-package org-tree-slide)

(use-package ox-reveal
:config
(setq org-reveal-root "./reveal.js"))

(use-package scihub
:defer t)

(use-package org-ref
:defer t)

(use-package xkcd
:defer t)

(use-package editorconfig
  :defer t
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package lua-mode
:defer t)

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (setq sp-highlight-pair-overlay nil))

(require 'ol)
  (org-link-set-parameters "hide-link"
                           :follow #'org-hide-link-open
                           :export #'org-hide-link-export
                           ;; :store #'org-hide-link-store-link
         :complete #'org-hide-link-complete-file)

  (defcustom org-hide-link-complete-file 'hide-link
    "The Emacs command to be used to display a man page."
    :group 'org-link
    :type 'string)

  (defun org-hide-link-open (path _)
    (find-file path))

  (defun org-hide-link-complete-file (&optional arg)
    "Create a file link using completion."
    (let ((file (read-file-name "File: "))
    (pwd (file-name-as-directory (expand-file-name ".")))
    (pwd1 (file-name-as-directory (abbreviate-file-name
                 (expand-file-name ".")))))
      (cond ((equal arg '(16))
       (concat "hide-link:"
         (abbreviate-file-name (expand-file-name file))))
      ((string-match
        (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
       (concat "hide-link:" (match-string 1 file)))
      ((string-match
        (concat "^" (regexp-quote pwd) "\\(.+\\)")
        (expand-file-name file))
       (concat "hide-link:"
         (match-string 1 (expand-file-name file))))
      (t (concat "hide-link:" file)))))

  (defun org-hide-link-export (link description format)
    "Export a man page link from Org files."
    (let ((path (format "¿Buscas algo?"))
          (desc (or description link)))
      (pcase format
        (`html (format "<span class = nolinks><a target=\"_blank\" href=\"%s\">%s</a></span>" path desc))
        (`latex (format "\\href{%s}{%s}" path desc))
        (`texinfo (format "@uref{%s,%s}" path desc))
        (`ascii (format "%s (%s)" desc path))
        (t path))))

         (defun my/blue-color-link (text)
           (org-insert-link nil "color:blue" text))

         (defun my/color-link-region ()
           (interactive)
           (if (region-active-p)
               (my/blue-color-link (buffer-substring-no-properties (region-beginning) (region-end)))
             (message "There is no active region.")))
  (org-add-link-type
   "color"
   (lambda (path)
     (message (concat "color "
                      (progn (add-text-properties
                              0 (length path)
                              (list 'face `((t (:foreground ,path))))
                              path) path))))
   (lambda (path desc format)
     (cond
      ((eq format 'html)
       (format "<span style=\"color:%s;\">%s</span>" path desc))
      ((eq format 'latex)
       (format "\\textcolor{%s}{%s}" path desc)))))

(defun my/svg-to-pdf ()
  "Get as input an image with svg format for return it as pdf"
  (interactive)
  (shell-command (concat "inkscape " (read-file-name "File name: ")  " --export-area-drawing --batch-process --export-type=pdf --export-filename=" (read-from-minibuffer (concat "Name output file:")) ".pdf&")))

(defun my/eps-to-pdf ()
  "Get as input an image with eps format for return it as pdf. It use gs script for do it may be just work in Windows systems."
  (interactive)
  (setq filename (read-file-name "File name: "))
  (setq outputname (read-from-minibuffer (concat "Name output file:")))
  (shell-command (concat "gswin32 -sDEVICE=pdfwrite -dEPSFitPage -o " outputname ".pdf " filename) ".pdf&"))

(defun my/pdf-to-svg ()
  "Get as input a file with pdf format for return it as svg image"
  (interactive)
  (shell-command (concat "pdftocairo -svg " (read-file-name "File name: ") " " (read-from-minibuffer (concat "Name output file:")) ".svg&")))

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my/load-blog-configuration ()
  (interactive)
  (load-file "~/.emacs.d/blog.el"))

(defun my/find-emacs-configuration ()
  (interactive)
  (find-file "~/.emacs.d/README.org"))

(defun my/upload-doc ()
(interactive)
(setq private_repository "~/Documents/Github/linux_connection/")
(setq filename (read-file-name "File name: "))
(copy-file filename private_repository)
(my/find-file private_repository)
(shell-command "~/Documents/Github/linux_connection/auto-git.sh")
(kill-buffer "*Shell Command Output*")
(delete-other-windows))

(defun my/actualization-repo ()
(interactive)
(shell-command "~/Documents/Github/linux_connection/auto-git.sh")
(kill-buffer "*Shell Command Output*")
(delete-other-windows))

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

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
       (change (if (string= dic "castellano") "english" "castellano")))
    (ispell-change-dictionary change)
    (message "Dicionario cambiado desde %s a %s" dic change)
    ))

(global-set-key (kbd "<f2>")   'fd-switch-dictionary)
(global-set-key (kbd "<f12>")   'flyspell-auto-correct-word)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(set-fringe-mode 10)        ;; Give some breathing room
(menu-bar-mode -1)          ;; Disable the menu bar
(show-paren-mode 1)
(global-hl-line-mode 0)     ;; Highlight lines
(global-visual-line-mode 1) ;;Better than fix the lines with set-fill-column
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case  t) ;;Tab completion in minibuffer: case insensitive
;; (setq read-buffer-completion-ignore-case t)
;; (setq visible-bell t) ;; Set up the visible bell
;; (add-hook 'split-window-right-hook 'my/theme-configuration)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(set-frame-parameter (selected-frame) 'alpha '(90 . 90)) 
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                python-mode-hook
                matlab-mode-hook
		prog-mode-hook
		conf-mode-hook))
		(add-hook mode (lambda () (display-line-numbers-mode 1))))
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode -1))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(setq-default frame-title-format '("%f [%m]")) ;;title bar name

(fset 'yes-or-no-p 'y-or-n-p) ;; Replace yes or no for y or n

(delete-selection-mode 1) ;;Let you select and replace with yank or write

(setq display-time-format "%H:%M %d %b %Y"
display-time-default-load-average nil)
(setq display-time-day-and-date t
display-time-24hr-format t)
(display-time)

(unless (equal "Battery status not available" (battery)) ;;;Show battery
(display-battery-mode 1))    ; On laptops it's nice to know how much power you have

(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(setq enable-local-variables 1)

(eval-after-load 'pdf-tools
'(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp))

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
(global-set-key (kbd "M-+")  'dired-create-empty-file)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(setq org-startup-folded t)
(setq org-return-follows-link 1)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(add-hook 'org-mode-hook 'org-indent-mode)

(org-babel-do-load-languages ;; list of babel languages
'org-babel-load-languages
'((matlab . t)
(ditaa . t)
;; (spice . t)
(gnuplot . t)
(org . t)
(shell . t)
(latex . t)
(python . t)
(asymptote . t)
))
(setq org-agenda-files'("~/Documents/Org/agenda.org"))

(global-set-key (kbd "C-c C-c") 'org-capture)

;; https://emacs.stackexchange.com/questions/16511/how-can-i-get-a-custom-org-drawer-to-open-close
;; https://www.emacswiki.org/emacs/ReplaceInString
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-11/msg00258.html
(setq org-export-with-drawers t)

(defun my/org-export-format-drawer (name content)
"Export :NOTES: and :LOGBOOK: drawers to HTML class
or LaTeX command"
(cond
((string-match "DETAILS" name)
(setq content (replace-regexp-in-string "<p>" "" content))
(setq content (replace-regexp-in-string "</p>" "" content))
(format "<pre class=\"example\">%s</pre>" content))))

(setq org-html-format-drawer-function 'my/org-export-format-drawer)

;; https://emacs-orgmode.gnu.narkive.com/EpuuKxSd/o-non-existent-agenda-file-file-txt-r-emove-from-list-or-a-bort#post11
;; https://amitp.blogspot.com/2021/04/automatically-generate-ids-for-emacs.html
(defun my/org-generate-custom-ids ()
"Generate CUSTOM_ID for any headings that are missing one"
(let ((existing-ids
;; (when (file-exists-p (buffer-file-name (current-buffer)))
(org-map-entries
(lambda ()  (org-entry-get nil "CUSTOM_ID")));; )
))

          ;; (when (file-exists-p (buffer-file-name (current-buffer)))
          (org-map-entries
           (lambda ()
             (let* ((custom-id (org-entry-get nil "CUSTOM_ID"))
                    (heading (org-heading-components))
                    (level (nth 0 heading))
                    (todo (nth 2 heading))
                    (headline (nth 4 heading))
                    (slug (my/title-to-filename headline))
                    (duplicate-id (member slug existing-ids)))
       (when (and ;; (not custom-id)
                  (< level 4)
                  ;; (not todo)
                  ;; (not duplicate-id)
                  )
                 (message "Adding entry %s to %s" slug headline)
                 (org-entry-put nil "CUSTOM_ID" slug))))));; )
)

(defun my/title-to-filename (title)
"Convert TITLE to a reasonable filename."
;; Based on the slug logic in org-roam, but org-roam also uses a
;; timestamp, and I use only the slug. BTW "slug" comes from
;; <https://en.wikipedia.org/wiki/Clean_URL#Slug>
(setq title (downcase-word title))
(setq title (s-replace-regexp "[^a-zA-Z0-9]+" "-" title))
(setq title (s-replace-regexp "-+" "-" title))
(setq title (s-replace-regexp "^-" "" title))
(setq title (s-replace-regexp "-$" "" title))
title)

(defun my/get-gcal-config-value (key)
  "Return the value of the json file gcal_secret for key"
  (cdr (assoc key (json-read-file "~/.emacs.d/gcal-secret.json")))
  )

;; https://emacs.stackexchange.com/questions/27982/export-code-blocks-in-org-mode-with-minted-environment

;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted"))
;;       org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; (setq org-latex-listings 'listings)
      ;; (setq org-agenda-start-with-log-mode t)
      ;; (setq org-log-done 'time)
      ;; (setq org-log-into-drawer t)
      ;; (setq org-image-actual-width 400)
      ;; ;; (require 'ox-extra)
      ;; ;; (ox-extras-activate '(ignore-headlines))
      ;; (setq org-clock-persist 'history)
      ;; (org-clock-persistence-insinuate)

      ;; (setq org-latex-listings 'minted
      ;;       org-latex-packages-alist '(("" "minted"))
      ;;       org-latex-pdf-process
      ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
      ;; ;; (setq org-latex-listings 'listings)
      ;; (setq org-src-preserve-indentation 1)

      ;; (org-add-link-type
      ;;  "color"
      ;;  (lambda (path)
      ;;    (message (concat "color "
      ;;                     (progn (add-text-properties
      ;;                             0 (length path)
      ;;                             (list 'face `((t (:foreground ,path))))
      ;;                             path) path))))
      ;;  (lambda (path desc format)
      ;;    (cond
      ;;     ((eq format 'html)
      ;;      (format "<span style=\"color:%s;\">%s</span>" path desc))
      ;;     ((eq format 'latex)
      ;;      (format "\\textcolor{%s}{%s}" path desc)))))

(add-hook 'matlab-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "matlab -batch %s" (shell-quote-argument
             (substring (buffer-name) 0  (- (length (buffer-name) ) 2)))))))
        ;; (add-hook 'org-mode-hook #'org-make-toc-mode) ;automtically update a file'sTOC with the save
        ;; (add-hook 'after-save-hook #'org-make-toc-mode) ;automtically update a file'sTOC with the save
        ;; (add-hook 'org-mode-hook 'my/org-generate-custom-ids) ;automatically custom_ids
    ;; puedes poner un (and (not (null (buffer-file-name ..) (file-exist-p ......))12:32
        (add-hook 'org-mode-hook
        (lambda ()
        (add-hook 'after-save-hook 'my/org-generate-custom-ids)))
        (dolist (hook '(text-mode-hook))
        (add-hook hook (lambda () (flyspell-mode 1))))
        (eval-after-load "flyspell"
        '(progn
        (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
        (define-key flyspell-mouse-map [mouse-3] #'undefined)))
        (setq-default ispell-program-name "aspell")
        (setq ispell-dictionary "castellano")
        (setq flyspell-default-dictionary "castellano")
          ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
          ;; (add-hook
          ;; 'minibuffer-setup-hook
          ;; (lambda ()
          ;; (if(string-match "TEXT: \\| search: " (minibuffer-prompt))
          ;; (flyspell-mode 1))))
          ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
          ;; (dolist (hook '(text-mode-hook))
          ;;   (add-hook hook (lambda () (flyspell-mode 1))))
          ;; (add-hook 'pdf-view-mode-hook #'pdf-links-minor-mode)
          ;; (add-hook 'org-mode 'display-line-numbers)
          ;; (add-hook 'dired-find-file 'pdf-tools-install)
          ;; ;; (add-hook 'org-publish-all 'my/load-blog-configuration)
          ;; (add-hook 'after-init-hook 'global-company-mode)
          ;; (add-hook 'matlab-mode-hook
          ;;           (lambda ()
          ;;             (set (make-local-variable 'compile-command)
          ;;                  (format "matlab -batch %s" (shell-quote-argument
          ;; 						 (substring (buffer-name) 0  (- (length (buffer-name) ) 2)))))))
          (add-hook 'python-mode-hook
                    (lambda ()
                      (set (make-local-variable 'compile-command)
                           (format "python %s" (shell-quote-argument (buffer-name))))))

          ;; (add-hook 'pdf-view-mode-hook
          ;;           (lambda ()
          ;;             (display-line-numbers-mode -1)))
          ;; (add-hook 'org-mode-hook
          ;;   (lambda ()
          ;; 	(local-set-key (kbd "C-c b") 'my/color-link-region)))
          ;; (add-hook 'text-mode-hook
          ;;   (lambda ()
          ;; 	(local-set-key (kbd "<f2>") 'table-split-cell-vertically)))
          ;; (add-hook 'text-mode-hook
          ;;   (lambda ()
          ;;    (local-set-key (kbd "<f3>") 'table-split-cell-horizontally)))

          ;; (add-hook 'message-mode-hook
          ;;           (lambda ()
          ;;             (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
          ;; (add-hook 'org-mode-hook
          ;;           (lambda ()
          ;;             (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)))

(defun efs/lsp-mode-setup()
(setq lsp-headerline-breadcrumb-sefments '(path-up-to-project file symbols))
(lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
:commands (lsp lsp-deferred)
:hook (lsp-mode . efs/lsp-mode-setup)
:init
(setq lsp-keymap-prefix "C-c l")
:config
(lsp-enable-which-key-integration t))

(use-package lsp-ui
:hook (lsp-mode . lsp-ui-mode)
:custom
(lsp-ui-doc-position 'bottom))

(use-package pyvenv
:config
(pyvenv-mode 1))

(use-package python-mode
:ensure t
:hook (python-mode . lsp-deferred)
:custom
(python-shell-interpreter "python3"))
(setq custom-theme-directory "~/.emacs.d/private/themes")
(load-theme 'minimal t)
