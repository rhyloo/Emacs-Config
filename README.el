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
  '(("org"     .       "https://orgmode.org/elpa/")
  ("gnu"     .       "https://elpa.gnu.org/packages/")
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
    :defer t
    :mode "\\.m\\'"
    ;; :interpreter ("matlab -nodesktop -nosplash -r" . matlab-mode)
    )

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

(use-package swiper
:bind ("C-s" . swiper-isearch))

;; ;;Auctex highlight syntax
(use-package auctex
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

(use-package simple-httpd
  :defer t
  :config
  (setq httpd-root "~/Documents/Github/Blog/public_html")
  ;; (setq httpd-port "8080")
  )

;; (use-package impatient-mode
;;   :defer t)

;; (use-package hledger-mode
;;   :defer t
;;   :mode ("\\.journal\\'" "\\.hledger\\'")
;;   :commands hledger-enable-reporting
;;   :preface
;;   (defun hledger/next-entry ()
;;     "Move to next entry and pulse."
;;     (interactive)
;;     (hledger-next-or-new-entry)
;;     (hledger-pulse-momentary-current-entry))

;;   (defface hledger-warning-face
;;     '((((background dark))
;;        :background "Red" :foreground "White")
;;       (((background light))
;;        :background "Red" :foreground "White")
;;       (t :inverse-video t))
;;     "Face for warning"
;;     :group 'hledger)

;;   (defun hledger/prev-entry ()
;;     "Move to last entry and pulse."
;;     (interactive)
;;     (hledger-backward-entry)
;;     (hledger-pulse-momentary-current-entry))

;;   :bind (("C-c j" . hledger-run-command)
;;          :map hledger-mode-map
;;          ("C-c e" . hledger-jentry)
;;          ("M-p" . hledger/prev-entry)
;;          ("M-n" . hledger/next-entry))
;;   :init
;;   (setq hledger-jfile "~/finance/2021.journal")
;;   :config
;;   (add-hook 'hledger-view-mode-hook #'hl-line-mode)
;;   (add-hook 'hledger-view-mode-hook #'center-text-for-reading)

;;   (add-hook 'hledger-view-mode-hook
;;             (lambda ()
;;               (run-with-timer 1
;;                               nil
;;                               (lambda ()
;;                                 (when (equal hledger-last-run-command
;;                                              "balancesheet")
;;                                   ;; highlight frequently changing accounts
;;                                   (highlight-regexp "^.*\\(savings\\|cash\\).*€")
;;                                   (highlight-regexp "^.*credit-card.*€"
;;                                                     'hledger-warning-face))))))

;;   (add-hook 'hledger-mode-hook
;;             (lambda ()
;;               (make-local-variable 'company-backends)
;;               (add-to-list 'company-backends 'hledger-company))))

;; ;; (use-package guess-language         ; Automatically detect language for Flyspell
;; ;;   :ensure t
;; ;;   :defer t
;; ;;   :init (add-hook 'text-mode-hook #'guess-language-mode)
;; ;;   :config
;; ;;   (setq guess-language-langcodes '((en . ("en_GB" "English"))
;; ;;                                    (es . ("es" "Spanish")))
;; ;;         guess-language-languages '(en es)
;; ;;         guess-language-min-paragraph-length 45)
;; ;;   :diminish guess-language-mode)

;; (use-package yasnippet                  ; Snippets
;;   :ensure t
;;   :config
;;   ;; (validate-setq
;;   ;;  yas-verbosity 1                      ; No need to be so verbose
;;   ;;  yas-wrap-around-region t)
;;   ;;  (with-eval-after-load 'yasnippet
;;   ;;    (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
;;   (yas-reload-all)
;;   (yas-global-mode))

;; (use-package yasnippet-snippets         ; Collection of snippets
;;   :ensure t)

;; (use-package dashboard
;; :ensure t
;; :config
;; (dashboard-setup-startup-hook)
;; (setq dashboard-startup-banner 'logo)
;; (setq dashboard-center-content t)
;; (setq dashboard-banner-logo-title "Bienvenido Rhyloo"))

;; (use-package org-superstar
;;   :after org
;;   :hook (org-mode . org-superstar-mode)
;;   :custom
;;   (org-superstar-remove-leading-stars t)
;;   (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))
;;   (require 'org-indent)

;; Replace list hyphen with dot
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; ;; Increase the size of various headings
;; (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
;; (dolist (face '((org-level-1 . 1.2)
;;                 (org-level-2 . 1.1)
;;                 (org-level-3 . 1.05)
;;                 (org-level-4 . 1.0)
;;                 (org-level-5 . 1.1)
;;                 (org-level-6 . 1.1)
;;                 (org-level-7 . 1.1)
;;                 (org-level-8 . 1.1)))
;;   (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make sure org-indent face is available


;; ;; Ensure that anything that should be fixed-pitch in Org files appears that way
;; (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;; (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
;; (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;; (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;; (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;; (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Get rid of the background on column views
(set-face-attribute 'org-column nil :background nil)
(set-face-attribute 'org-column-title nil :background nil)

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package ledger-mode
:defer t)

(use-package flymake
:defer t
:config
(add-hook 'after-init-hook 'flymake-mode))

;; (use-package flymake-ledger
;; :after flymake
;; )

;; (use-package org
  ;;   :config
  ;;   (progn
  ;;   (use-package ob
  ;;     :config
      ;; (setq org-src-fontify-natively t)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((js . t)
         (org . t)
         (css . t)
         (dot . t)
         (latex . t)
         (shell . t)
         (python . t)
         (matlab . t)
         (emacs-lisp . t)))
    ;; (use-package ox-md
    ;;   :config
    ;;   (setq org-md-headline-style 'atx)
    ;;   (use-package ox-gfm
    ;;     :ensure t))
    ;; (use-package ox-html
    ;;   :config
    ;;   (setq org-html-doctype "html5"
    ;;         org-html-html5-fancy t
    ;;         org-html-metadata-timestamp-format "%Y-%m-%d %H:%M"))
    ;; (use-package org-crypt
    ;;   :config
    ;;   (org-crypt-use-before-save-magic)
    ;;   (setq org-crypt-key "i@l42y.com"
    ;;         org-tags-exclude-from-inheritance (quote ("crypt"))))
    ;; (use-package org-agenda
    ;;   :bind ("C-c a" . org-agenda))
    ;; (use-package ox
    ;;   :defer t
    ;;   :config
    ;;   (progn
    ;;   (use-package ox-publish
    ;;   :config
      (setq org-publish-project-alist
      '(("org-content"
      :base-directory "~/Documents/Github/Blog/blog/"
      :base-extension "org"
      :auto-sitemap t                ; Generate sitemap.org automagically...
      :sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
      :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
      :publishing-directory "~/Documents/Github/Blog/public_html"
      :recursive t
      :publishing-function org-html-publish-to-html
      :headline-levels 4             ; Just the default for this project.
      :auto-preamble t
      )
      ("org-media"
      :base-directory "~/Documents/Github/Blog/blog"
      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg"
      :publishing-directory "~/Documents/Github/Blog/public_html"
      :recursive t
      :publishing-function org-publish-attachment
      )
      ("blog" :components ("org-content" "org-media"))
      ))
;; )
      ;; ))))

;; (use-package ox-publish
;;   :config
;;   (setq org-publish-project-alist
;;   '(("org-notes"
;; 	:base-directory "~/Documents/Github/Blog/blog/"
;; 	:base-extension "org"
;; 	:auto-sitemap t                ; Generate sitemap.org automagically...
;; 	:sitemap-filename "sitemap.org"  ; ... call it sitemap.org (it's the default)...
;; 	:sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
;; 	:publishing-directory "~/Documents/Github/Blog/public_html"
;; 	:recursive t
;; 	:publishing-function org-html-publish-to-html
;; 	:headline-levels 4             ; Just the default for this project.
;; 	:auto-preamble t
;; 	)
;; 	("org-static"
;; 	:base-directory "~/Documents/Github/Blog/blog/"
;; 	:base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
;; 	:publishing-directory "~/Documents/Github/blog/public_html"
;; 	:recursive t
;; 	:publishing-function org-publish-attachment
;; 	)
;; 	("org" :components ("org-notes" "org-static"))
;; 	)))

(use-package json
:defer t)

;; (setq package-check-signature nil)

;; (use-package org-gcal
;; :defer t
;; :config
;; (setq org-gcal-client-id (my/get-gcal-config-value 'org-gcal-client-id)
;;       org-gcal-client-secret (my/get-gcal-config-value 'org-gcal-client-secret)
;;       org-gcal-file-alist '(("jorgebenma@gmail.com" . "~/Documents/Org/agenda.org")))
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;; ;; (add-hook 'org-agenda-mode-hook
;; ;;   (lambda ()
;; ;;   (add-hook 'after-save-hook 'org-gcal-sync)))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) )))

;; (use-package guess-language         ; Automatically detect language for Flyspell
;;   :defer t
;;   :init (add-hook 'text-mode-hook #'guess-language-mode)
;;   :config
;;   (setq guess-language-langcodes '((en . ("en_GB" "English"))
;;                                    (es . ("es" "Spanish")))
;;         guess-language-languages '(en es)
;;         guess-language-min-paragraph-length 45)
;;   :diminish guess-language-mode)

(use-package htmlize
:defer t
:config
(setq org-src-fontify-natively t))

;; (use-package auto-complete
;; :config
;; (ac-config-default)
;; (setq ac-auto-start t)
;; (setq ac-delay 0.1)
;; (setq ac-auto-show-menu nil)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (setq ac-trigger-key nil)
;; (add-hook 'after-init-hook 'global-auto-complete-mode))

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide))

(use-package org-present
  :bind (:map org-present-mode-keymap
         ("C-c C-j" . dw/org-present-next)
         ("C-c C-k" . dw/org-present-prev))
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(use-package epresent
:defer t)

(use-package org-roam
:init
(setq org-roam-v2-ack t)
:custom
(org-roam-directory "~/Documents/org")
(setq org-roam-graph-viewer nil)
:bind (("C-c n l" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n g" . org-roam-graph)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-capture)
       ;; Dailies
       ("C-c n j" . org-roam-dailies-capture-today))
:config
;; (org-roam-db-autosync-mode)
(org-roam-setup))

(use-package ox-reveal
  :config
  (setq org-reveal-root "./reveal.js"))

(use-package vhdl-mode
  :defer t)

(use-package lua-mode
  :defer t)

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

(use-package scihub
  :defer t)

(use-package which-key
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

;; (defun my/theme-configuration ()
;; (set-face-attribute hl-line-face nil :underline nil :background "black")
;; (set-face-attribute 'mode-line-inactive nil :background nil :box nil :foreground "gray" :overline "white")
;; (set-face-attribute 'vertical-border nil :background nil :foreground "white")
;; )
(set-face-attribute 'mode-line nil :height 100)

;; (defun my/setup-color-theme-dark ()
;;   (interactive)
;;   (when (display-graphic-p)
;;     (color-theme-sanityinc-solarized-dark))
;;   (set-frame-parameter (selected-frame) 'alpha '(85 85))
;;   (add-to-list 'default-frame-alist '(alpha 85 85))
;;   ;; (set-foreground-color "white")
;;   (set-face-background 'secondary-selection "black")
;;   (set-face-background 'font-lock-doc-face "black")
;;   (set-face-foreground 'font-lock-comment-face "blue")
;;   ;; (set-face-background 'org-indent "black")
;;   ;; (set-face-foreground 'org-indent "black")
;;   (set-face-background 'org-hide "black")
;;   (set-face-foreground 'org-hide "black")
;;   (set-face-background 'font-lock-string-face "black")
;;   (set-background-color "black")
;;   (set-face-background 'hl-line "black"))

;; (defun my/setup-color-theme-light ()
;;   (interactive)
;;   (when (display-graphic-p)
;;     (color-theme-sanityinc-solarized-light))
;;   ;; set transparency
;;   (set-frame-parameter (selected-frame) 'alpha '(95 95))
;;   (add-to-list 'default-frame-alist '(alpha 95 95))
;;   (set-foreground-color "black")
;;   (set-face-background 'secondary-selection "#fdf6e3")
;;   (set-face-background 'font-lock-doc-face "black")
;;   (set-face-background 'hl-line "lightblue")
;;   ;; (set-face-background 'company-tooltip "white")
;;   ;; (set-face-background 'org-indent "#fdf6e3")
;;   ;; (set-face-foreground 'org-indent "#fdf6e3")
;;   ;; (set-face-foreground 'company-preview-common "#b58900")
;;   (set-face-attribute 'region nil :background "lightgrey")
;;   (set-face-foreground 'font-lock-comment-face "dark red"))

;; (setq current-theme '(my/setup-color-theme-dark))
;; (defun synchronize-theme ()
;;   (interactive)
;;   (setq hour (string-to-number (substring (current-time-string)11 13)))
;;     (if (member hour (number-sequence 7 18))
;;         (setq now '(my/setup-color-theme-light))
;;         (setq now '(my/setup-color-theme-dark)))
;;     (if (equal now current-theme)
;;         nil
;;       (setq current-theme now))
      ;; (eval now))

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

;; Thanks, but no thanks
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
(show-paren-mode 1)
(global-hl-line-mode 0) ;; Highlight lines
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

(setq large-file-warning-threshold nil)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
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

;; (use-package spacegray-theme :defer t)
;; (use-package doom-themes
;; :defer t
;; :hook
;; (after-init . (lambda () (load-theme 'doom-palenight t)))
;; )
;; (doom-themes-visual-bell-config)

;; (use-package emojify
;;   :hook (erc-mode . emojify-mode)
;;   :commands emojify-mode)

(setq display-time-format "%H:%M %p %b %y"
          display-time-default-load-average nil)
  (setq display-time-day-and-date t
        display-time-24hr-format t)
  (display-time)
  (unless (equal "Battery status not available" (battery)) ;;;Show battery
(display-battery-mode 1))    ; On laptops it's nice to know how much power you have

;; (use-package diminish)

;; (use-package smart-mode-line
;; :config
;; (smart-mode-line-enable 1)
;;   ;; (sml/apply-theme 'respectful)  ; Respect the theme colors
;;   ;; (sml/setup)
;; ;; :config
;; ;; (setq sml/mode-width 'right
;; ;;         sml/name-width 60)
;; ;; 	(setq-default mode-line-format
;; ;; 	`("%e"
;; ;;         mode-line-front-space
;; ;;         evil-mode-line-tag
;; ;;         mode-line-mule-info
;; ;;         mode-line-client
;; ;;         mode-line-modified
;; ;;         mode-line-remote
;; ;;         mode-line-frame-identification
;; ;;         mode-line-buffer-identification
;; ;;         sml/pos-id-separator
;; ;;         (vc-mode vc-mode)
;; ;;         " "
;; ;;         ;mode-line-position
;; ;;         sml/pre-modes-separator
;; ;;         mode-line-modes
;; ;;         " "
;; ;;         mode-line-misc-info))

;; ;;     (setq rm-excluded-modes
;; ;;       (mapconcat
;; ;;         'identity
;; ;;         ; These names must start with a space!
;; ;;         '(" GitGutter" " MRev" " company"
;; ;;         " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
;; ;;         " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
;; ;;         "\\|"))
;;     )

;; ;; You must run (all-the-icons-install-fonts) one time after
;; ;; installing this package!
;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))
;; (use-package doom-modeline
;; :defer t
;; :hook
;; (after-init . (lambda () (doom-modeline-mode 1) (defvar doom-modeline-icon (display-graphic-p))))
;; :custom-face
;; (mode-line ((t (:height 125))))
;; (mode-line-inactive ((t (:height 110))))
;; :custom
;; ;; (doom-modeline-enable-word-count 1)
;; (doom-modeline-height 20)
;; (doom-modeline-bar-width 6)
;; (doom-modeline-lsp t)
;; (doom-modeline-github nil)
;; (doom-modeline-mu4e nil)
;; (doom-modeline-irc t)
;; (doom-modeline-minor-modes t)
;; (doom-modeline-persp-name nil)
;; (doom-modeline-buffer-file-name-style 'truncate-except-project)
;; (doom-modeline-major-mode-icon t))

(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(setq org-confirm-babel-evaluate nil)

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
      ;; (org-babel-do-load-languages ;; list of babel languages
      ;;  'org-babel-load-languages
      ;;  '((matlab . t)
      ;;    (ditaa . t)
      ;;    ;; (spice . t)
      ;;    (gnuplot . t)
      ;;    (org . t)
      ;;    (shell . t)
      ;;    (latex . t)
      ;;    (python . t)
      ;;    (asymptote . t)
      ;;    ))
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
      ;; (add-hook 'python-mode-hook
      ;;           (lambda ()
      ;;             (set (make-local-variable 'compile-command)
      ;;                  (format "d:/Software/WPy64-3940/python-3.9.4.amd64/python.exe %s" (shell-quote-argument (buffer-name))))))

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
