(org-babel-load-file(expand-file-name "~/.emacs.d/README.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(custom-enabled-themes '(doom-monokai-pro))
 '(custom-safe-themes
   '("b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" default))
 '(enable-local-variables :all)
 '(exwm-floating-border-color "#232635")
 '(fci-rule-color "#676E95")
 '(highlight-tail-colors ((("#383f45") . 0) (("#323e51") . 20)))
 '(httpd-host "localhost")
 '(httpd-port 8000)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f2b" "#c792ea"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f2b" "#c3e88d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f2b" "#676E95"))
 '(objed-cursor-color "#ff5370")
 '(org-export-with-drawers '(not "LOGBOOK" "DETAILS"))
 '(org-html-htmlize-output-type 'inline-css)
 '(org-html-postamble 'auto)
 '(org-html-postamble-format
   '(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>
<p class=\"validation\">%v</p> ")))
 '(package-selected-packages
   '(lua-mode editorconfig xkcd google-translate org-ref scihub telega bongo emms EMMS yasnippet-snippets a ox-reveal org-tree-slide bufshow smartparens elpy blacken emacsql-sqlite org-roam epresent auto-complete htmlize obs-websocket websocket guess-language org-gcal simple-httpd ox ox-publish org-agenda ob flymake-ledger ledger-mode company pdf-tools auctex org-indent ws-butler use-package swiper super-save spacegray-theme smart-mode-line org-superstar org-make-toc minions matlab-mode magit emojify doom-themes doom-modeline diminish dashboard benchmark-init alert))
 '(pdf-view-midnight-colors (cons "#EEFFFF" "#292D3E"))
 '(rustic-ansi-faces
   ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook 'write-file-copy)
     (eval add-hook 'after-save-hook 'write-file-copy "README.org")
     (eval add-hook 'after-save-hook 'my/write-file)
     (eval add-hook 'after-save-hook 'org-babel-tangle)))
 '(scihub-homepage "https://sci-hub.se")
 '(vc-annotate-background "#292D3E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#c3e88d")
    (cons 40 "#d7de81")
    (cons 60 "#ebd476")
    (cons 80 "#ffcb6b")
    (cons 100 "#fcb66b")
    (cons 120 "#f9a16b")
    (cons 140 "#f78c6c")
    (cons 160 "#e78e96")
    (cons 180 "#d690c0")
    (cons 200 "#c792ea")
    (cons 220 "#d97dc1")
    (cons 240 "#ec6898")
    (cons 260 "#ff5370")
    (cons 280 "#d95979")
    (cons 300 "#b36082")
    (cons 320 "#8d678b")
    (cons 340 "#676E95")
    (cons 360 "#676E95")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#232635" :foreground "#A6Accd" :box nil :height 95))))
 '(mode-line-inactive ((t (:height 110)))))

;; ;; Minimize garbage collection during startup
;; (setq gc-cons-threshold most-positive-fixnum)
;; ;; Lower threshold back to 8 MiB (default is 800kB)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold (expt 2 23))))
;; ;;Personal information
;; (setq user-full-name "Rhyloo"
;;       user-mail-address "rhyloot@gmail.com")

;; ;;My defuns
;; (require 'ol)
;; (org-link-set-parameters "hide-link"
;;                          :follow #'org-hide-link-open
;;                          :export #'org-hide-link-export
;;                          :store #'org-hide-link-store-link
;; 			 :complete #'org-hide-link-complete-file)

;; (defcustom org-hide-link-complete-file 'hide-link
;;   "The Emacs command to be used to display a man page."
;;   :group 'org-link
;;   :type 'string)

;; (defun org-hide-link-open (path _)
;;   (find-file path))

;; (defun org-hide-link-complete-file (&optional arg)
;;   "Create a file link using completion."
;;   (let ((file (read-file-name "File: "))
;; 	(pwd (file-name-as-directory (expand-file-name ".")))
;; 	(pwd1 (file-name-as-directory (abbreviate-file-name
;; 				       (expand-file-name ".")))))
;;     (cond ((equal arg '(16))
;; 	   (concat "hide-link:"
;; 		   (abbreviate-file-name (expand-file-name file))))
;; 	  ((string-match
;; 	    (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
;; 	   (concat "hide-link:" (match-string 1 file)))
;; 	  ((string-match
;; 	    (concat "^" (regexp-quote pwd) "\\(.+\\)")
;; 	    (expand-file-name file))
;; 	   (concat "hide-link:"
;; 		   (match-string 1 (expand-file-name file))))
;; 	  (t (concat "hide-link:" file)))))

;; (defun org-hide-link-export (link description format _)
;;   "Export a man page link from Org files."
;;   (let ((path (format "¿Buscas algo?"))
;;         (desc (or description link)))
;;     (pcase format
;;       (`html (format "<span class = nolinks><a target=\"_blank\" href=\"%s\">%s</a></span>" path desc))
;;       (`latex (format "\\href{%s}{%s}" path desc))
;;       (`texinfo (format "@uref{%s,%s}" path desc))
;;       (`ascii (format "%s (%s)" desc path))
;;       (t path))))

;; (defun my/svg-to-pdf ()
;;   (interactive)
;;   (shell-command (concat "inkscape " (read-file-name "File name: ")  " --export-area-drawing --batch-process --export-type=pdf --export-filename=" (read-from-minibuffer (concat "Name output file:")) ".pdf&")))

;; (defun my/eps-to-pdf ()
;;   (interactive)
;;   (setq filename (read-file-name "File name: "))
;;   (setq outputname (read-from-minibuffer (concat "Name output file:")))
;;   (shell-command (concat "gswin32 -sDEVICE=pdfwrite -dEPSFitPage -o " outputname ".pdf " filename) ".pdf&"))

;; (defun my/pdf-to-svg ()
;;   (interactive)
;;   (shell-command (concat "pdftocairo -svg " (read-file-name "File name: ") " " (read-from-minibuffer (concat "Name output file:")) ".svg&")))

;; (defun my/blue-color-link (text)
;;   (org-insert-link nil "color:blue" text))

;; (defun my/color-link-region ()
;;   (interactive)
;;   (if (region-active-p)
;;       (my/blue-color-link (buffer-substring-no-properties (region-beginning) (region-end)))
;;     (message "There is no active region.")))

;; (defun my/reload-emacs-configuration ()
;;   (interactive)
;;   (load-file "~/.emacs.d/init.el"))

;; (defun my/load-blog-configuration ()
;;   (interactive)
;;   (load-file "~/.emacs.d/blog.el"))

;; (defun my/find-emacs-configuration ()
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el"))

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
;;   (set-face-background 'company-tooltip "white")
;;   ;; (set-face-background 'org-indent "#fdf6e3")
;;   ;; (set-face-foreground 'org-indent "#fdf6e3")
;;   (set-face-foreground 'company-preview-common "#b58900")
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
;;       (eval now))


;; (defun my/find-file (filename)
;;   (interactive "FFind file: ")
;;   (set-buffer (find-file-noselect filename)))

;; (defun my/pwd ()
;;   "Put the current file name on the clipboard"
;;   (interactive)
;;   (let ((filename (if (equal major-mode 'dired-mode)
;;                       default-directory
;;                     (buffer-file-name))))
;;     (when filename
;;       (with-temp-buffer
;;         (insert filename)
;;         (clipboard-kill-region (point-min) (point-max)))
;;       (message filename))))

;; ;;add-to-list
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (add-to-list 'exec-path "d:/Software/latex/texmfs/install/miktex/bin/x64")
;; (add-to-list 'exec-path "d:/Software/WPy64-3940/python-3.9.4.amd64")
;; (add-to-list 'exec-path "d:/Software/Spice64/bin")
;; (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
;; ;; (add-to-list 'load-path "d:/Software/Spice64/bin")
;; ;; (add-to-list 'load-path "d:/Software/WPy64-3940/python-3.9.4.amd64/")
;; ;; (add-to-list 'load-path "d:/Software/latex/texmfs/install/miktex/bin/x64")
;; ;; (add-to-list 'load-path "~/.emacs.d/ob-spice/")
;; (add-to-list 'load-path "~/.emacs.d/ispell-multi")
;; ;; (require 'ob-spice)
;; (add-to-list 'auto-mode-alist '("\\.cir$"          . spice-mode))
;; ;; (setenv "PATH" (concat (getenv "PATH") "d:/Software/Spice64/bin/"))
;; ;; (setq exec-path (append exec-path '("d:/Software/Spice64/bin/")))
;; ;; (setenv "PATH" (concat (getenv "PATH") "d:/Software/latex/texmfs/install/miktex/bin/x64"))
;; ;; (setq exec-path (append exec-path '("d:/Software/latex/texmfs/install/miktex/bin/x64")))
;; ;; (setenv "PATH" (concat (getenv "PATH") "d:/Software/WPy64-3940/python-3.9.4.amd64/"))
;; ;; (setq exec-path (append exec-path '("d:/Software/WPy64-3940/python-3.9.4.amd64/")))
;; ;;;GUI configs
;; ;; set transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 85))
;; (add-to-list 'default-frame-alist '(alpha 85 85))
;; (run-with-timer 0 3600 'synchronize-theme)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (show-paren-mode 1) ;; Highlight parens
;; (scroll-bar-mode -1)
;; (tooltip-mode -1)
;; (setq display-time-day-and-date t
;;       display-time-24hr-format t)
;; (display-time)
;; (delete-selection-mode 1) ;;Let you select and replace with yank or write
;; (fset 'yes-or-no-p 'y-or-n-p) ;; Replace yes or no for y or n
;; (setq inhibit-startup-message t) ;;Inhibit startup message
;; (setq-default frame-title-format '("%f [%m]")) ;;title bar name
;; (unless (equal "Battery status not available" (battery)) ;;;Show battery
;; (display-battery-mode 1))    ; On laptops it's nice to know how much power you have
;; ;;Writting
;; (setq sentence-end-double-space nil)
;; (setq initial-buffer-choice "~/jorge")

;; ;;;Global configs
;; ;; Interactive defun
;; (global-auto-revert-mode 1) ;;Not ask for refresh a buffer
;; (global-display-line-numbers-mode 1) ;; Display line numbers left
;; (global-hl-line-mode 1) ;; Highlight lines
;; (global-visual-line-mode 1) ;;Better than fix the lines with set-fill-column
;; (setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory
;; ;;Keys
;; (eval-after-load 'pdf-tools
;;   '(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp))
;; (global-set-key (kbd "C-c C-c") 'org-capture)
;; (global-set-key (kbd "C-c q") 'org-passwords)
;; (global-set-key (kbd "C-c <left>")  'windmove-left)
;; (global-set-key (kbd "C-c <right>") 'windmove-right)
;; (global-set-key (kbd "C-c <up>")    'windmove-up)
;; (global-set-key (kbd "C-c <down>")  'windmove-down)
;; (global-set-key (kbd "<f1>") 'my/find-emacs-configuration)
;; (global-set-key (kbd "<f6>") 'org-publish-current-file)
;; (global-set-key (kbd "<f4>") 'org-publish-all)
;; (global-set-key (kbd "<f9>") 'my/pwd)
;; (global-set-key (kbd "C-c l") 'my/svg-to-pdf)
;; (global-set-key (kbd "<f5>") 'my/reload-emacs-configuration)
;; (global-set-key (kbd "C-x q") 'compile)
;; ;;Dictionary
;; (setq-default ispell-program-name "aspell")
;; (setq ispell-dictionary "castellano")
;; (eval-after-load "flyspell"
;;     '(progn
;;        (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;        (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; ;;System coding
;; (prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")

;; ;;Compilation
;; (setq compilation-ask-about-save nil);; Shut up compile saves
;; (setq compilation-save-buffers-predicate '(lambda () nil));; Don't save *anything* before compilation

;; ;; Org mode config
;; (setq org-agenda-files'("~/projects/agenda.org"))
;; (setq org-agenda-start-with-log-mode t)
;; (setq org-log-done 'time)
;; (setq org-log-into-drawer t)
;; (setq org-image-actual-width 400)
;; ;; (require 'ox-extra)
;; ;; (ox-extras-activate '(ignore-headlines))
;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)
;; (add-hook 'org-mode-hook 'org-indent-mode)
;; (setq org-startup-folded t)
;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted"))
;;       org-latex-pdf-process
;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; ;; (setq org-latex-listings 'listings)
;; (setq org-src-preserve-indentation 1)
;; (setq org-return-follows-link 1)
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
;; ;;Loading customizations
;; ;; (setq custom-file "~/.emacs.d/custom.el")
;; ;; (load custom-file)
;; ;; (load-file "~/.emacs.d/export.el")
;; ;; (load-file "~/.emacs.d/export_blog.el")
;; ;; (load-file "~/.emacs.d/develop/publisher.el")

;; ;;Hooks
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

;; ;;;SSH
;; (setq tramp-default-method "plink")
;; (defun connect-remote ()
;;   (interactive)
;;   (dired "/plink:terminal:/"))

;; (defun my-shell ()
;;     (interactive)
;;     (let ((default-directory "/plink:terminal:/"))
;;       (shell)))

;; ;;PASSWORDS
;; (eval-after-load "org-passwords"
;;   '(progn
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c u")
;;        'org-passwords-copy-username)
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c p")
;;        'org-passwords-copy-password)
;;      (define-key org-passwords-mode-map
;;        (kbd "C-c o")
;;        '(lambda ()
;;           (interactive)
;;           (org-passwords-copy-password)
;;           (org-passwords-open-url)))))
;; ;;;Initialize package system
;; (require 'package)
;; (setq package-archives
;;       '(("org"     .       "https://orgmode.org/elpa/")
;;         ("gnu"     .       "https://elpa.gnu.org/packages/")
;;        ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
;; 	("melpa" . "http://melpa.org/packages/")))
;; (package-initialize)
;; ;; Use-package for civilized configuration
;; ;; Bootstrap 'use-package'
;; (eval-after-load 'gnutls
;;   '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'bind-key)
;; (setq use-package-always-ensure t)

;; ;; Matlab
;; (use-package matlab-mode
;;   :defer t)

;; ;; Magit
;; (use-package magit
;;   :defer t
;;   :config
;;   (global-set-key (kbd "C-x g") 'magit-status))

;; ;;Mode-line format
;; (use-package smart-mode-line
;;   :config
;;   (smart-mode-line-enable 1))

;; ;;Color-theme-sanityinc
;; (use-package color-theme-sanityinc-solarized
;;   :config
;;   (synchronize-theme))

;; ;;Ivy-mode/Swiper
;; (use-package swiper
;;   :config
;;   (global-set-key (kbd "C-s") 'swiper))

;; ;;Auctex highlight syntax
;; (use-package auctex
;;   :defer t)

;; ;;Company-mode
;; (use-package company
;;   :config
;;   (company-mode))

;; (use-package pdf-tools
;;   :config
;;   (pdf-loader-install)
;;   (setq-default pdf-view-display-size 'fit-page))

;; (use-package simple-httpd
;;   :defer t
;;   :config
;;   (setq httpd-root "~/projects/blog/public_html"))


;; (use-package impatient-mode
;;   :defer t)

;; (use-package linkode
;;   :defer t)

;; (use-package org-mime
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

;; (use-package all-the-icons)

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-directory-name-transformer    #'identity
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-extension-regex          treemacs-last-period-regex-value
;;           treemacs-file-follow-delay             0.2
;;           treemacs-file-name-transformer         #'identity
;;           treemacs-follow-after-init             t
;;           treemacs-expand-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-move-forward-on-expand        nil
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-read-string-input             'from-child-frame
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-asc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-user-mode-line-format         nil
;;           treemacs-user-header-line-format       nil
;;           treemacs-width                         35
;;           treemacs-workspace-switch-cleanup      nil)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null treemacs-python-executable)))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :defer t
;;   :after (treemacs evil)
;;   :ensure t)

;; (use-package treemacs-projectile
;;   :defer t
;;   :after (treemacs projectile)
;;   :ensure t)

;; (use-package treemacs-icons-dired
;;   :defer t
;;   :after (treemacs dired)
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :defer t
;;   :after (treemacs magit)
;;   :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :defer t
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-enable-word-count t))

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

;; (use-package org-pomodoro
;;   :defer t)

;; ;; (use-package websocket)

;; ;; (defun my/twitch-message (text)
;; ;;   (interactive "MText: ")
;; ;;   (with-current-buffer
;; ;;       (get-buffer-create "Twitch message")
;; ;;     (erase-buffer)
;; ;;     (insert text)
;; ;;     (goto-char (point-min))))

;; ;; (load-file "./develop/obs-websocket.el")
;; ;; (setq obs-websocket-password "123456789")

;; ;; (load-file "./develop/org-passwords.el")
;; ;; (setq org-passwords-file "~/projects/internet/passwords.org")

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-banner-logo-title "Bienvenido Rhyloo"))

;; (use-package benchmark-init
;;   :defer t
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate)
;; )

;; (use-package org
;;   :defer t
;;   :ensure org-plus-contrib)

;; (use-package ispell-multi
;;   :defer t
;;   :ensure nil
;;   :load-path "~/.emacs.d/ispell-multi/ispell-multi.el")

;; (use-package org-bullets)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-enabled-themes '(sanityinc-solarized-dark))
;;  '(custom-safe-themes
;;    '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
;;  '(package-selected-packages
;;    '(org-bullets org-plus-contrib org-pomodoro yasnippet-snippets treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs yasnippet ws-butler use-package swiper super-save spacegray-theme smart-mode-line pfuture persp-mode pdf-tools org-superstar org-mime org-make-toc minions matlab-mode magit linkode impatient-mode hydra hledger-mode emojify doom-themes doom-modeline diminish dashboard company color-theme-sanityinc-solarized cfrs benchmark-init auctex alert ace-window)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
