;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
  ;; ;; The default is 800 kilobytes.  Measured in bytes.
  (setq gc-cons-threshold (* 50 1000 1000))
  ;; Profile emacs startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "*** Emacs loaded in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

;; Initialize package sources
(require 'package)
(setq package-archives
  '(
  ("gnu"     .       "https://elpa.gnu.org/packages/")
  ("nongnu"     .       "https://elpa.nongnu.org/nongnu/")
  ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
  ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

;; Use-package for civilized configuration
(unless (package-installed-p 'use-package)
(package-refresh-contents)
(package-install 'use-package))

  (require 'use-package)
  (setq use-package-always-ensure t)

(use-package benchmark-init
 :ensure t
 :config
 ;; To disable collection of benchmark data after init is done.
 (add-hook 'after-init-hook 'benchmark-init/deactivate))

  ;; (use-package ispell-multi
  ;;   :defer t
  ;;   :ensure nil
  ;;   :load-path "~/.emacs.d/ispell-multi/ispell-multi.el")

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

(use-package yasnippet                  ; Snippets
  :ensure t
  :config
  (setq yas-snippet-dirs
  '("~/.emacs.d/elpa/yasnippet-snippets-20210910.1959/snippets/" ;;Latex-collection snippets
  "~/Documents/Github/yasnippets-latex/snippets/latex-mode/"
  ))
  ;; (validate-setq
  ;;  yas-verbosity 1                      ; No need to be so verbose
  ;;  yas-wrap-around-region t)
  ;;  (with-eval-after-load 'yasnippet
  ;;    (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  (yas-reload-all)
  (yas-global-mode 1))

(use-package emms
  :defer t
  :ensure nil
  :config
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emms/lisp")
  (require 'emms-setup)
  (require 'emms-player-mplayer)
  (emms-standard)
  (emms-default-players)
  (define-emms-simple-player mplayer '(file url)
  (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
  "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
  (setq emms-source-file-default-directory "~/Music/")

(defun track-title-from-file-name (file)
"For using with EMMS description functions. Extracts the track
 title from the file name FILE, which just means a) taking only
 the file component at the end of the path, and b) removing any
 file extension."
 (with-temp-buffer
 (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
 (ignore-error 'search-failed
 (search-forward-regexp (rx "." (+ alnum) eol))
 (delete-region (match-beginning 0) (match-end 0)))
 (buffer-string)))

 (defun my-emms-track-description (track)
 "Return a description of TRACK, for EMMS, but try to cut just
 the track name from the file name, and just use the file name too
 rather than the whole path."
 (let ((artist (emms-track-get track 'info-artist))
 (title (emms-track-get track 'info-title)))
 (cond ((and artist title)
 (concat artist " - " title))
 (title title)
 ((eq (emms-track-type track) 'file)
 (track-title-from-file-name (emms-track-name track)))
 (t (emms-track-simple-description track)))))

 (setq emms-track-description-function 'my-emms-track-description))

(use-package scihub
:defer t)

(use-package org-ref
:defer t)

(use-package google-translate
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

(setq user-full-name "Rhyloo"
      user-mail-address "rhyloot@gmail.com")

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
(setq title (s-downcase title))
(setq title (s-replace-regexp "[^a-zA-Z0-9]+" "-" title))
(setq title (s-replace-regexp "-+" "-" title))
(setq title (s-replace-regexp "^-" "" title))
(setq title (s-replace-regexp "-$" "" title))
title)

(defun my/get-gcal-config-value (key)
  "Return the value of the json file gcal_secret for key"
  (cdr (assoc key (json-read-file "~/.emacs.d/gcal-secret.json")))
  )

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

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
       (change (if (string= dic "castellano") "english" "castellano")))
    (ispell-change-dictionary change)
    (message "Dicionario cambiado desde %s a %s" dic change)
    ))

(global-set-key (kbd "<f2>")   'fd-switch-dictionary)
(global-set-key (kbd "<f12>")   'flyspell-auto-correct-word)

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(show-paren-mode 1)
(global-hl-line-mode 1) ;; Highlight lines
(global-visual-line-mode 1) ;;Better than fix the lines with set-fill-column
(setq read-file-name-completion-ignore-case t)
(add-hook 'split-window-right-hook 'my/theme-configuration)
;; (setq completion-ignore-case  t);;Tab completion in minibuffer: case insensitive
;; (setq read-buffer-completion-ignore-case t)
;; Set up the visible bell
;; (setq visible-bell t)

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

(use-package spacegray-theme :defer t)
(use-package doom-themes
:defer t
:hook
(after-init . (lambda () (load-theme 'doom-palenight t)))
)
;; (doom-themes-visual-bell-config)

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

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

(setq org-confirm-babel-evaluate nil)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(use-package blacken
:defer t
:config
(add-hook 'python-mode-hook 'blacken-mode))

(use-package elpy
  ;; :ensure t
  :defer t
  ;; :init
  ;; (advice-add 'python-mode :before 'elpy-enable)
  )

(setq display-time-world-list
    '(;; ("Etc/UTC" "UTC")
      ;; ("America/Los_Angeles" "Seattle")
      ;; ("America/New_York" "New York")
      ("America/Guayaquil" "Guayaquil")
      ;; ("Europe/Athens" "Athens")
      ;; ("Pacific/Auckland" "Auckland")
      ;; ("Asia/Shanghai" "Shanghai")
      ;; ("Asia/Kolkata" "Hyderabad")
      ))
(setq display-time-world-time-format "%Z\t%a %d %b %R")

(setq enable-local-variables 1)

(eval-after-load 'pdf-tools 
'(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp))

(global-set-key (kbd "C-c C-c") 'org-capture)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

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

(use-package smartparens
  ;; :init
  ;; (bind-key "C-M-f" #'sp-forward-sexp smartparens-mode-map)
  ;; (bind-key "C-M-b" #'sp-backward-sexp smartparens-mode-map)
  ;; (bind-key "C-)" #'sp-forward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "C-(" #'sp-backward-slurp-sexp smartparens-mode-map)
  ;; (bind-key "M-)" #'sp-forward-barf-sexp smartparens-mode-map)
  ;; (bind-key "M-(" #'sp-backward-barf-sexp smartparens-mode-map)
  ;; (bind-key "C-S-s" #'sp-splice-sexp)
  ;; (bind-key "C-M-<backspace>" #'backward-kill-sexp)
  ;; (bind-key "C-M-S-<SPC>" (lambda () (interactive) (mark-sexp -1)))

  :config
  (smartparens-global-mode t)

  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (setq sp-highlight-pair-overlay nil))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; https://emacs.stackexchange.com/questions/27982/export-code-blocks-in-org-mode-with-minted-environment
    (setq org-agenda-files'("~/Documents/Org/agenda.org"))
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
      (add-hook 'org-mode-hook 'org-indent-mode)
      (setq org-startup-folded t)
      ;; (setq org-latex-listings 'minted
      ;;       org-latex-packages-alist '(("" "minted"))
      ;;       org-latex-pdf-process
      ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
      ;; ;; (setq org-latex-listings 'listings)
      ;; (setq org-src-preserve-indentation 1)
      (setq org-return-follows-link 1)
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
        (add-hook 'org-mode-hook #'org-make-toc-mode) ;automtically update a file'sTOC with the save
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
