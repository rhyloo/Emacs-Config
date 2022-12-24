(setq user-full-name "Jorge Benavides"
      user-mail-address "jorge2@uma.es")

(scroll-bar-mode -1)        ;; Disable visible scrollbar
(tool-bar-mode -1)          ;; Disable the toolbar
(tooltip-mode -1)           ;; Disable tooltips
(menu-bar-mode -1)          ;; Disable the menu bar
(set-fringe-mode 15)        ;; Give some breathing room
(show-paren-mode 1)         ;; Show parens
(global-hl-line-mode 1)     ;; Highlight lines
(global-visual-line-mode 1)                    ;;Better than fix the lines with set-fill-column
(set-frame-parameter (selected-frame) 'alpha '(100 . 100)) ;;  Set frame transparency
(add-to-list 'default-frame-alist '(alpha . (100 . 100))) ;;  Set frame transparency
(setq-default tab-width 2) ;; Default to an indentation size of 2 spaces
(setq-default evil-shift-width tab-width) ;; Default to an indentation size of 2 spaces
(setq-default indent-tabs-mode nil) ;;Use spaces instead of tabs for indentation
(display-time) ;; Show the time in the bar
(setq display-time-format "%H:%M %d %b %y" ;; Show hour minute day month and year
display-time-default-load-average nil)
(setq display-time-day-and-date t
display-time-24hr-format t) ;; Change format
(unless (equal "Battery status not available" (battery)) ;;Show battery
  (display-battery-mode 1))    ; On laptops it's nice to know how much power you have
(setq-default frame-title-format '("%b [%m]")) ;;Title bar name
(column-number-mode) ;; Enable column mode
(setq inhibit-startup-message t) ;; Avoid startup message
(dolist (mode '(text-mode-hook
		prog-mode-hook
    matlab-mode-hook
		conf-mode-hook
		lisp-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))    ;; Enable line numbers for some modes
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))   ;; Override modes which derive from the above

(setq read-file-name-completion-ignore-case t) ;;Insensitive letter case
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX
(setq large-file-warning-threshold nil) ;;Dont warn for large files
(setq org-confirm-babel-evaluate nil) ;;Stop the confirmation to evaluate source code
(fset 'yes-or-no-p 'y-or-n-p)                  ;; Replace yes or no for y or n
(set-frame-parameter (selected-frame) 'fullscreen 'maximized) ;; maximize windows by default.
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize windows by default.
(delete-selection-mode 1) ;;Let you select and replace with yank or write
(setq backup-directory-alist `(("." . "~/.backups"))) ;;;Backup directory
(global-auto-revert-mode 1)  ;; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t)    ;; Revert Dired and other buffers
(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)) ;; Open pdfs by default with emacs

;;(setq completion-ignore-case  t)             ;;Tab completion in minibuffer: case insensitive
;; (setq read-buffer-completion-ignore-case t)
;; (setq visible-bell t) ;; Set up the visible bell

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
  ;; FUNCION PARA CREAR ARCHIVOS TEMPORALES, PARA PROBAR COSAS O ESCRIBIR x COSAS
  (lambda ()
    (with-temp-buffer
      (setq temp-file-name (read-string "Temporary file name: "))
      (message temp-file-name)
      (find-file (concat "/tmp/" temp-file-name))))

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

  ;; (defun my/kill-this-buffer ()
  ;;     "Kill the current buffer."
  ;;     (interactive)
  ;;     (setq name (buffer-name))
  ;;       (delete-window name)
  ;;       (kill-buffer name))

  ;;--------------------------
  ;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
  ;;--------------------------

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

;; Initialize package sources
(require 'package)
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

(use-package vscode-dark-plus-theme
:ensure t
:config
(load-theme 'vscode-dark-plus t))

(use-package minions
  :config
  (minions-mode 1))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
(setq doom-modeline-support-imenu t)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)

;; Whether to use hud instead of default bar. It's only respected in GUI.
(setq doom-modeline-hud nil)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)

;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   auto => emacs/l/comint.el (in a project) or comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the time icon. It respects variable `doom-modeline-icon'.
(setq doom-modeline-time-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)

;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)
;; also enable the start of mu4e-alert
(mu4e-alert-enable-mode-line-display)

;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)

;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
(setq doom-modeline-gnus-timer 2)

;; Wheter groups should be excludede when gnus automatically being updated.
(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the time. It respects `display-time-mode'.
(setq doom-modeline-time t)

;; Whether display the misc segment on all mode lines.
;; If nil, display only if the mode line is active.
(setq doom-modeline-display-misc-in-all-mode-lines t)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(use-package all-the-icons
  :ensure t)

(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0"))  ; make sure you have set this, see below

(use-package arduino-mode
  :defer t)
(use-package company-arduino
       :defer t)

(use-package matlab-mode
  :defer t
  :mode "\\.m\\'"
  :interpreter ("matlab -nodesktop -nosplash -r" . matlab-mode)
  )
 (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
 (setq matlab-indent-function t)
 (setq matlab-shell-command "matlab")

;; setup matlab in babel
(setq org-babel-default-header-args:matlab
  '((:results . "output") (:session . "*MATLAB*")))

;; list of babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((matlab . t)))

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

(provide 'ob-octave-fix)

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
  (lsp-enable-which-key-integration t)
  (setq lsp-vhdl-server-path "~/bin/vhdl-tool")
  (use-package lsp-mode
       :config
       (add-hook 'vhdl-mode-hook 'lsp)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

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
  (python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))
;; (setq custom-theme-directory "~/.emacs.d/private/themes")
;; (load-theme 'minimal t)

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-auto-revert-mode t)
  (setq magit-auto-revert-immediately t)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

;; ;;Auctex highlight syntax
(use-package auctex
  :defer t)

;; ;;Company-mode
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package swiper
  :bind ("C-s" . swiper-isearch))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        TeX-source-correlate-method 'synctex))

(use-package org
  :pin gnu
  :hook
  ((before-save . zp/org-set-last-modified))
  :config
  (ivy-mode 1)
  (setq org-src-tab-acts-natively t))

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
(setq org-startup-folded t)
(setq org-return-follows-link 1)
(setq org-src-preserve-indentation nil
	org-edit-src-content-indentation 0) ;; Respect parent buffer indentation
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-hide-leading-stars t)
(global-set-key (kbd "C-c C-c") 'org-capture)
(setq org-startup-with-inline-images nil)
(setq org-image-actual-width nil)
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-todo-keyword-faces
      '(
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))

;;https://yiufung.net/post/org-mode-hidden-gems-pt2
(setq org-catch-invisible-edits 'show-and-error)
(setq org-cycle-separator-lines 0)
(setq org-latex-caption-above nil)
(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("university-works"
               "\\documentclass{article}
                   [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(defun my/org-latex-export-to-pdf-minted
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist
      #'my/org-latex-compile)))

(defcustom org-latex-pdf-minted-process
  (if (executable-find "latexmk")
      '("latexmk -f -pdf -%latex -bibtex -interaction=nonstopmode  -shell-escape -output-directory=%o %f")
    '("%latexmk -interaction nonstopmode -shell-escape -output-directory %o %f"
      "%bib -interaction nonstopmode -shell-escape -output-directory %o %f"
      "%latexmk -interaction nonstopmode -shell-escape -output-directory %o %f"
      "%latexxmk -interaction nonstopmode -shell-escape -output-directory %o %f"))
  "Commands to process a LaTeX file to a PDF file.

  This is a list of strings, each of them will be given to the
  shell as a command.  %f in the command will be replaced by the
  relative file name, %F by the absolute file name, %b by the file
  base name (i.e. without directory and extension parts), %o by the
  base directory of the file, %O by the absolute file name of the
  output file, %latex is the LaTeX compiler (see
  `org-latex-compiler'), and %bib is the BibTeX-like compiler (see
  `org-latex-bib-compiler').

  The reason why this is a list is that it usually takes several
  runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
  does not have a clever mechanism to detect which of these
  commands have to be run to get to a stable result, and it also
  does not do any error checking.

  Consider a smart LaTeX compiler such as `texi2dvi' or `latexmk',
  which calls the \"correct\" combinations of auxiliary programs.

  Alternatively, this may be a Lisp function that does the
  processing, so you could use this to apply the machinery of
  AUCTeX or the Emacs LaTeX mode.  This function should accept the
  file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of latex"
                 ("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))
          (const :tag "3 runs of latex"
                 ("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))
          (const :tag "latex,bibtex,latex,latex"
                 ("%latex -interaction nonstopmode -shell-escape -%bib -output-directory %o %f"
                  "%bib %b"
                  "%latex -interaction nonstopmode -shell-escape -%bib -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -%bib -output-directory %o %f"))
          (const :tag "texi2dvi"
                 ("cd %o; LATEX=\"%latex\" texi2dvi -p -b -V %b.tex"))
          (const :tag "latexmk"
                 ("latexmk -f -pdf -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f"))
          (function)))

(defun my/org-latex-compile (texfile &optional snippet)
  (unless snippet (message "Processing LaTeX file %s..." texfile))
  (let* ((compiler
          (or (with-temp-buffer
                (save-excursion (insert-file-contents texfile))
                (and (search-forward-regexp (regexp-opt org-latex-compilers)
                                            (line-end-position 2)
                                            t)
                     (progn (beginning-of-line) (looking-at-p "%"))
                     (match-string 0)))
              "pdflatex"))
         (process (if (functionp org-latex-pdf-minted-process) org-latex-pdf-minted-process
                    ;; Replace "%latex" with "%L" and "%bib" and
                    ;; "%bibtex" with "%B" to adhere to `format-spec'
                    ;; specifications.
                    (mapcar (lambda (command)
                              (replace-regexp-in-string
                               "%\\(?:\\(?:bib\\|la\\)tex\\|bib\\)\\>"
                               (lambda (m) (upcase (substring m 0 2)))
                               command))
                            org-latex-pdf-minted-process)))
         (spec `((?B . ,(shell-quote-argument org-latex-bib-compiler))
                 (?L . ,(shell-quote-argument compiler))))
         (log-buf-name "*Org PDF LaTeX Output*")
         (log-buf (and (not snippet) (get-buffer-create log-buf-name)))
         (outfile (org-compile-file texfile process "pdf"
                                    (format "See %S for details" log-buf-name)
                                    log-buf spec)))
    (unless snippet
      (when org-latex-remove-logfiles
        (mapc #'delete-file
              (directory-files
               (file-name-directory outfile)
               t
               (concat (regexp-quote (file-name-base outfile))
                       "\\(?:\\.[0-9]+\\)?\\."
                       (regexp-opt org-latex-logfiles-extensions))
               t)))
      (let ((warnings (org-latex--collect-warnings log-buf)))
        (message (concat "PDF file produced"
                         (cond
                          ((eq warnings 'error) " with errors.")
                          (warnings (concat " with warnings: " warnings))
                          (t "."))))))
    ;; Return output file name.
    outfile))

(org-export-define-derived-backend 'my-latex 'latex
  :menu-entry
  '(?l "My export to LaTeX"
       ((?m "As PDF with minted" my/org-latex-export-to-pdf-minted)))
  ;; :translate-alist
  ;; '((quote-block . org-latex-testing-block))
  )

(require 'midnight)
                                (setq ido-use-virtual-buffers t) ;; Save buffers in the memory even if you close them

                                  (use-package treemacs
                                  :ensure t)
                                (defun org-latex-math-block (_math-block contents _info)
                                  "Transcode a MATH-BLOCK object from Org to LaTeX.
                                                  CONTENTS is a string.  INFO is a plist used as a communication
                                                  channel."
                                  (when (org-string-nw-p contents)
                                    (format "$%s$" (org-trim contents))))
                                (defun create-temp-directory ()
                                  "This function let you create directories or files
                                                    in the tmp directory for testing"
                                  (interactive)
                                  (let (
                                        (choices '("directory" "files"))
                                        (name (read-string "Enter name temporary file: ")))

                                    (find-file (concat "/tmp/" name))
                                    (message name)

                                    ))
                                (global-set-key (kbd "\C-c M-+") 'create-temp-directory)

                                (windmove-default-keybindings 'M) ;; Me muevo por las ventanas

                                (defun window-toggle-split-direction ()
                                  "Switch window split from horizontally to vertically, or vice versa.
                                i.e. change right window to bottom, or change bottom window to right."
                                  (interactive)
                                  (require 'windmove)
                                  (let ((done))
                                    (dolist (dirs '((right . down) (down . right)))
                                      (unless done
                                        (let* ((win (selected-window))
                                               (nextdir (car dirs))
                                               (neighbour-dir (cdr dirs))
                                               (next-win (windmove-find-other-window nextdir win))
                                               (neighbour1 (windmove-find-other-window neighbour-dir win))
                                               (neighbour2 (if next-win (with-selected-window next-win
                                                                          (windmove-find-other-window neighbour-dir next-win)))))
                                          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
                                          (setq done (and (eq neighbour1 neighbour2)
                                                          (not (eq (minibuffer-window) next-win))))
                                          (if done
                                              (let* ((other-buf (window-buffer next-win)))
                                                (delete-window next-win)
                                                (if (eq nextdir 'right)
                                                    (split-window-vertically)
                                                  (split-window-horizontally))
                                                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))


                                (global-set-key (kbd "C-x 4") 'window-toggle-split-direction)

                                (use-package treemacs
                                  :ensure t
                                  :defer t
                                  :init
                                  (with-eval-after-load 'winum
                                    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
                                  :config
                                  (progn
                                    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
                                          treemacs-deferred-git-apply-delay        0.5
                                          treemacs-directory-name-transformer      #'identity
                                          treemacs-display-in-side-window          t
                                          treemacs-eldoc-display                   'simple
                                          treemacs-file-event-delay                5000
                                          treemacs-file-extension-regex            treemacs-last-period-regex-value
                                          treemacs-file-follow-delay               0.2
                                          treemacs-file-name-transformer           #'identity
                                          treemacs-follow-after-init               t
                                          treemacs-expand-after-init               t
                                          treemacs-find-workspace-method           'find-for-file-or-pick-first
                                          treemacs-git-command-pipe                ""
                                          treemacs-goto-tag-strategy               'refetch-index
                                          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
                                          treemacs-hide-dot-git-directory          t
                                          treemacs-indentation                     2
                                          treemacs-indentation-string              " "
                                          treemacs-is-never-other-window           nil
                                          treemacs-max-git-entries                 5000
                                          treemacs-missing-project-action          'ask
                                          treemacs-move-forward-on-expand          nil
                                          treemacs-no-png-images                   nil
                                          treemacs-no-delete-other-windows         t
                                          treemacs-project-follow-cleanup          nil
                                          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
                                          treemacs-position                        'left
                                          treemacs-read-string-input               'from-child-frame
                                          treemacs-recenter-distance               0.1
                                          treemacs-recenter-after-file-follow      nil
                                          treemacs-recenter-after-tag-follow       nil
                                          treemacs-recenter-after-project-jump     'always
                                          treemacs-recenter-after-project-expand   'on-distance
                                          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
                                          treemacs-show-cursor                     nil
                                          treemacs-show-hidden-files               t
                                          treemacs-silent-filewatch                nil
                                          treemacs-silent-refresh                  nil
                                          treemacs-sorting                         'alphabetic-asc
                                          treemacs-select-when-already-in-treemacs 'move-back
                                          treemacs-space-between-root-nodes        t
                                          treemacs-tag-follow-cleanup              t
                                          treemacs-tag-follow-delay                1.5
                                          treemacs-text-scale                      nil
                                          treemacs-user-mode-line-format           nil
                                          treemacs-user-header-line-format         nil
                                          treemacs-wide-toggle-width               70
                                          treemacs-width                           35
                                          treemacs-width-increment                 1
                                          treemacs-width-is-initially-locked       t
                                          treemacs-workspace-switch-cleanup        nil)

                                    ;; The default width and height of the icons is 22 pixels. If you are
                                    ;; using a Hi-DPI display, uncomment this to double the icon size.
                                    ;;(treemacs-resize-icons 44)

                                    (treemacs-follow-mode t)
                                    (treemacs-filewatch-mode t)
                                    (treemacs-fringe-indicator-mode 'always)
                                    (when treemacs-python-executable
                                      (treemacs-git-commit-diff-mode t))

                                    (pcase (cons (not (null (executable-find "git")))
                                                 (not (null treemacs-python-executable)))
                                      (`(t . t)
                                       (treemacs-git-mode 'deferred))
                                      (`(t . _)
                                       (treemacs-git-mode 'simple)))

                                    (treemacs-hide-gitignored-files-mode nil))
                                  :bind
                                  (:map global-map
                                        ("M-0"       . treemacs-select-window)
                                        ("C-x t 1"   . treemacs-delete-other-windows)
                                        ("C-x t t"   . treemacs)
                                        ("C-x t d"   . treemacs-select-directory)
                                        ("C-x t B"   . treemacs-bookmark)
                                        ("C-x t C-t" . treemacs-find-file)
                                        ("C-x t M-t" . treemacs-find-tag)))
                                ;; (ido-mode 1)
                                (global-set-key (kbd "M-o") 'ace-window)


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
                          (use-package forge)
                          (setq auth-sources '("~/.authinfo"))
                          (use-package magit-pretty-graph
                            :ensure nil
                            :load-path "~/.emacs.d/private/packages/magit-pretty-graph")
                          ;(magit-pg-repo "/some/path")

                             (defun my-clear ()
                                (interactive)
                                ;; (erase-buffer)
                                (comint-clear-buffer))

                              (defun my-shell-hook ()
                                (local-set-key "\C-l" 'my-clear))

                              (add-hook 'shell-mode-hook 'my-shell-hook)

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

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; Full width comment box                                                 ;;
                        ;; from http://irreal.org/blog/?p=374                                     ;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (defun bjm-comment-box (b e)
                "Draw a box comment around the region but arrange for the region to extend to at least the fill column. Place the point after the comment box."

                (interactive "r")

                (let ((e (copy-marker e t)))
                  (goto-char b)
                  (end-of-line)
                  (insert-char 49  (+ 0 0))
                  ;; (insert-char ?  (- (/ fill-column ) (current-column)))
                  (comment-box b e 1)
                  (goto-char e)
                  (set-marker e nil)))

                (global-set-key (kbd "C-c b b") 'bjm-comment-box)
                (add-hook 'c-mode-hook 'display-fill-column-indicator-mode)
                (add-hook 'arduino-mode-hook 'display-fill-column-indicator-mode)
                (add-hook 'c-mode-hook 'turn-on-auto-fill)
                (add-hook 'arduino-mode-hook 'turn-on-auto-fill)
                (defun my-arduino-hook ()
                  ;;(auto-fill-mode 1)
                  (setq fill-column 80))
                (add-hook 'arduino-mode-hook 'my-arduino-hook)
                 (add-hook 'c-mode-common-hook
                            (lambda ()
                              (auto-fill-mode 1)
                              (set (make-local-variable 'fill-nobreak-predicate)
                                   (lambda ()
                                     (not (eq (get-text-property (point) 'face)
                                              'font-lock-comment-face))))))
                 (add-hook 'arduino-mode-common-hook
                            (lambda ()
                              (auto-fill-mode 1)
                              (set (make-local-variable 'fill-nobreak-predicate)
                                   (lambda ()
                                     (not (eq (get-text-property (point) 'face)
                                              'font-lock-comment-face))))))
        (add-hook 'c-mode-hook (lambda () (c-toggle-comment-style 1)))
        (add-hook 'c-mode-hook (lambda () (setq comment-start "/*"
                                                comment-end   "*/")))

        (add-hook 'c-mode-common-hook (lambda () (setq comment-start "/*"
                                                comment-end   "*/")))

        (add-hook 'c++-mode-hook (lambda () (setq comment-start "/*"
                                                comment-end   "*/")))

        (add-hook 'arduino-mode-hook (lambda () (setq comment-start "/*"
                                                comment-end   "*/")))
              (use-package json-mode)
              (setq auth-sources '("~/.authinfo"))
              (use-package markdown-mode
                :ensure t
                :commands (markdown-mode gfm-mode)
                :mode (("README\\.md\\'" . gfm-mode))
                :init (setq markdown-command "/usr/local/bin/multimarkdown"))
              (custom-set-variables
               '(markdown-command "/usr/bin/markdown")
               ;; '(markdown-open-command "/usr/bin/grip")
               )
            (use-package markdown-preview-eww)
            (use-package taskwarrior
              :load-path "~/.emacs.d/private/packages/taskwarrior"
              :bind
              (("C-x t" . taskwarrior)
               ("C-x t" . taskwarrior)))
          (add-to-list 'lsp-language-id-configuration '(forge-post-mode . "markdown"))
      (auto-fill-mode 1)
      (setq comment-auto-fill-only-comments t)
      ;; (add-hook 'text-mode-hook
      ;;           (lambda () (auto-fill-mode -1)))
  (add-hook 'c-mode-common-hook
      (lambda ()
        (when (featurep 'filladapt)
          (c-setup-filladapt))))
  (add-hook 'cc-mode-common-hook
      (lambda ()
        (when (featurep 'filladapt)
          (c-setup-filladapt))))
  (add-hook 'arduino-mode-hook
      (lambda ()
        (when (featurep 'filladapt)
          (c-setup-filladapt))))

  ;; (use-package helm
  ;; :ensure t)
  (delete-selection-mode 1) ;;Let you select and replace with yank or write
(setq dired-listing-switches "-ls")

;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; ;; The default is 800 kilobytes.  Measured in bytes.
;; (setq gc-cons-threshold (* 511 1024 1024))
;; (setq gc-cons-percentage 0.5)
;; (run-with-idle-timer 5 t #'garbage-collect)
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
      '(;; ("org"     .       "https://orgmode.org/elpa/")
        ("gnu"     .       "https://elpa.gnu.org/packages/")
        ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

;; (package-initialize)

;; Use-package for civilized configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package org-special-block-extras
;;   :defer t
;;   :hook (org-mode . org-special-block-extras-mode))

(use-package minions
  :config
  (minions-mode 1))

;; (use-package mu4e-alert
;;   :ensure t
;;   :after mu4e
;;   :init
;;   (setq mu4e-alert-interesting-mail-query
;;         (concat
;;          "flag:unread maildir:/INBOX"))
;;   (mu4e-alert-enable-mode-line-display)
;;   (defun my/mu4e-alert ()
;;     (interactive)
;;     (mu4e~proc-kill)
;;     (mu4e-alert-enable-mode-line-display)
;;     )
;;   (run-with-timer 0 2700 'my/mu4e-alert)
;;   ;; (setq mu4e-alert-enable-notifications t)
;;   ;; :config
;;   ;; (mu4e-alert-set-default-style 'libnotify)
;;   ;; (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;;   )

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; (use-package mu4e
;;   :ensure nil
;;   :config
;;   (setq
;;    send-mail-function 'smtpmail-send-it
;;    smtpmail-smtp-server "correo.uma.es"
;;    smtpmail-smtp-service 587)

;;   (setq mu4e-update-interval (* 45 60))
;;   (setq mu4e-get-mail-command "offlineimap")
;;   (setq mu4e-change-filenames-when-moving t)
;;   (setq mu4e-attachment-dir "~/Downloads")
;;   (setq mu4e-maildir "~/Maildir"
;;         mu4e-sent-folder "/Sent"
;;         mu4e-drafts-folder "/Drafts"
;;         mu4e-trash-folder "/Trash")
;;   ;; (setq mu4e-refile-folder
;;   ;;       (lambda (msg)
;;   ;;         (cond
;;   ;;          ((mu4e-message-contact-field-matches msg :from
;;   ;;                                               "jorge2@uma.es")
;;   ;;           "/Sent"))))

;;   (setq message-kill-buffer-on-exit t)
;;   (setq mu4e-sent-messages-behavior 'sent)

;;   (setq mu4e-contexts
;;         `(,(make-mu4e-context
;;             :name "University"
;;             :enter-func (lambda () (mu4e-message "University mode"))
;;             :leave-func (lambda () (mu4e-message "Leaving University mode"))
;;             :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches msg
;;                                                                                     :to "jorge2@uma.es")))
;;             :vars '((user-mail-address . "jorge2@uma.es")
;;                     (user-full-name . "Jorge Benavides M.")
;;                     (mu4e-compose-signature . (concat
;;                                                "Jorge Benavides M.\n"
;;                                                "Estudiante de Ingeniería en electrónica, robótica y mecatrónica\n"
;;                                                "\n"))))))
;;   (setq mu4e-context-policy 'pick-first)
;;   (setq mail-user-agent 'mu4e-user-agent)
;;   ;; (add-hook 'mu4e-compose-mode-hook
;;   ;;           (defun my-add-bcc ()
;;   ;;             "Add a Bcc: header."
;;   ;;             (save-excursion (message-add-header "Bcc: jorge2@uma.es\n"))))
;;   (mu4e t)
;;   )

(use-package arduino-mode
  :defer t)
(use-package company-arduino
  :defer t)

(use-package deft
    :config
    (setq deft-directory "~/Documents/org"
          deft-recursive t
          ;; deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          ;; deft-strip-title-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t
          )
    :bind
    ("C-c n s" . deft))

(use-package org
  :pin gnu
  :hook
  ((before-save . zp/org-set-last-modified))
  :config
  (ivy-mode 1)
  (setq org-src-tab-acts-natively t))

(use-package org-ref
  :defer t)

  ;; (use-package citar)

  ;; (use-package helm-bibtex)

(use-package org-noter
  :defer t)

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

(use-package guess-language         ; Automatically detect language for Flyspell
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (es . ("es" "Spanish")))
        guess-language-languages '(en es)
        guess-language-min-paragraph-length 45)
  :diminish guess-language-mode)

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
  :defer t
  :hook ((org-mode . auto-revert-mode)
         ;; (org-mode . super-save-mode)
         ;; (org-mode . highlight-changes-mode)
         )
  :diminish super-save-mode
  :config
  (super-save-mode 1)
  ;; (setq super-save-auto-save-when-idle t)
  )

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
   (octave . t)
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
         ("C-c n r" . org-roam-node-random)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n a" . org-roam-alias-add)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; (org-roam-db-autosync-mode)
  (org-roam-setup)
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.2)
                 (window-parameters . (
                                       ;; (no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
           :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
           :immediate-finish t)
          ("p" "programming" plain "%?"
           :target (file+head "programming/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n") :unnarrowed t)
          ("i" "ideas" plain "%?"
           :target (file+head "ideas/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n") :unnarrowed t)
          ("r" "referencias" plain "%?"
           :target (file+head "referencias/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n") :unnarrowed t)
          ("t" "trabajos" plain "%?"
           :target (file+head "trabajos/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n#+language: es\n#+options: ^:nil tex:t\n#+options: toc:nil author:nil title:nil\n#+latex_class: university-works\n#+latex_class_options: [11pt,a4paper]\n#+latex_header: \\input{config_files/packages}\n#+latex_header: \\datosportada{Grado en ingeniería en electrónica, robótica y mecatrónica}{Ingeniería hidráulica}{Prácticas de laboratorio}{Prácticas con EPANET}{Práctica \# 3}{Diseño y análisis de instalaciones hidráulicas con EPANET}{images/hidrauilica_practica3_instacion_propuesta1.pdf}{2021-2022}{Jorge Benavides Macías \\\\ 05306948-C}\n #+begin_src latex :eval yes\n \\portada \n \\tableofcontents\n \\newpage\n#+end_src\n") :unnarrowed t)
          ("o" "posts" plain "%?"
           :target (file+head "posts/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n") :unnarrowed t)
          ("P" "personal" plain "%?"
           :target (file+head "personal/%<%Y-%m-%d>-${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n") :unnarrowed t)
          )
        time-stamp-start "#\\+lastmod: [\t]*")
  )

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
  (python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab))
;; (setq custom-theme-directory "~/.emacs.d/private/themes")
;; (load-theme 'minimal t)

(use-package scihub
  :defer t)

(use-package which-key
  :defer t)

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  ;; (org-display-inline-images)
  (dw/org-present-prepare-slide))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images))

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
         (org-present-mode . org-present-read-only)
         (org-present-mode-quit . dw/org-present-quit-hook)))

(setq user-full-name "Jorge Benavides"
      user-mail-address "jorge2@uma.es")

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

;; (defun my/kill-this-buffer ()
;;     "Kill the current buffer."
;;     (interactive)
;;     (setq name (buffer-name))
;;       (delete-window name)
;;       (kill-buffer name))

;;--------------------------
;; Handling file properties for ‘CREATED’ & ‘LAST_MODIFIED’
;;--------------------------

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
;; (set-face-attribute 'mode-line nil :height 100)

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
(setq files-to-ignore '("2022-02-06-agenda.org"))
(defun my/org-generate-custom-ids ()
  "Generate CUSTOM_ID for any headings that are missing one"
  (unless (member (buffer-name) files-to-ignore)
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
  )

(defun my/title-to-filename (title)
  "Convert TITLE to a reasonable filename."
  ;; Based on the slug logic in org-roam, but org-roam also uses a
  ;; timestamp, and I use only the slug. BTW "slug" comes from
  ;; <https://en.wikipedia.org/wiki/Clean_URL#Slug>
  (setq title (s-downcase title))
  (setq title (s-replace-regexp "[^a-zA-Z0-9À-ú]+" "-" title))
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

(setq ido-use-virtual-buffers t)
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
;; (add-hook 'split-window-right-hook 'my/theme-configuration)
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

(setq display-time-format "%H:%M %d %b %y"
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

;;I am not sure about this, check it
  (add-to-list 'safe-local-variable-values
               '(compile-command . '(concat "pdflatex -shell-escape ")))

(eval-after-load 'pdf-tools
  '(define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward-regexp))

(global-set-key (kbd "C-c C-c") 'org-capture)
(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-todo-keyword-faces
      '(
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ))

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "DONE")))

;;https://yiufung.net/post/org-mode-hidden-gems-pt2
(setq org-catch-invisible-edits 'show-and-error)
(setq org-cycle-separator-lines 0)
(setq org-latex-caption-above nil)
(add-to-list 'org-latex-classes
             '("university-works"
               "\\documentclass{article}
                   [NO-DEFAULT-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(defun my/org-latex-export-to-pdf-minted
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex outfile
      async subtreep visible-only body-only ext-plist
      #'my/org-latex-compile)))

(defcustom org-latex-pdf-minted-process
  (if (executable-find "latexmk")
      '("latexmk -f -pdf -%latex -interaction=nonstopmode  -shell-escape -output-directory=%o %f")
    '("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
      "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
      "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))
  "Commands to process a LaTeX file to a PDF file.

  This is a list of strings, each of them will be given to the
  shell as a command.  %f in the command will be replaced by the
  relative file name, %F by the absolute file name, %b by the file
  base name (i.e. without directory and extension parts), %o by the
  base directory of the file, %O by the absolute file name of the
  output file, %latex is the LaTeX compiler (see
  `org-latex-compiler'), and %bib is the BibTeX-like compiler (see
  `org-latex-bib-compiler').

  The reason why this is a list is that it usually takes several
  runs of `pdflatex', maybe mixed with a call to `bibtex'.  Org
  does not have a clever mechanism to detect which of these
  commands have to be run to get to a stable result, and it also
  does not do any error checking.

  Consider a smart LaTeX compiler such as `texi2dvi' or `latexmk',
  which calls the \"correct\" combinations of auxiliary programs.

  Alternatively, this may be a Lisp function that does the
  processing, so you could use this to apply the machinery of
  AUCTeX or the Emacs LaTeX mode.  This function should accept the
  file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
          (repeat :tag "Shell command sequence"
                  (string :tag "Shell command"))
          (const :tag "2 runs of latex"
                 ("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))
          (const :tag "3 runs of latex"
                 ("%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -output-directory %o %f"))
          (const :tag "latex,bibtex,latex,latex"
                 ("%latex -interaction nonstopmode -shell-escape -bibtex -output-directory %o %f"
                  "%bib %b"
                  "%latex -interaction nonstopmode -shell-escape -bibtex -output-directory %o %f"
                  "%latex -interaction nonstopmode -shell-escape -bibtex -output-directory %o %f"))
          (const :tag "texi2dvi"
                 ("cd %o; LATEX=\"%latex\" texi2dvi -p -b -V %b.tex"))
          (const :tag "latexmk"
                 ("latexmk -f -pdf -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f"))
          (function)))

(defun my/org-latex-compile (texfile &optional snippet)
  (unless snippet (message "Processing LaTeX file %s..." texfile))
  (let* ((compiler
          (or (with-temp-buffer
                (save-excursion (insert-file-contents texfile))
                (and (search-forward-regexp (regexp-opt org-latex-compilers)
                                            (line-end-position 2)
                                            t)
                     (progn (beginning-of-line) (looking-at-p "%"))
                     (match-string 0)))
              "pdflatex"))
         (process (if (functionp org-latex-pdf-minted-process) org-latex-pdf-minted-process
                    ;; Replace "%latex" with "%L" and "%bib" and
                    ;; "%bibtex" with "%B" to adhere to `format-spec'
                    ;; specifications.
                    (mapcar (lambda (command)
                              (replace-regexp-in-string
                               "%\\(?:\\(?:bib\\|la\\)tex\\|bib\\)\\>"
                               (lambda (m) (upcase (substring m 0 2)))
                               command))
                            org-latex-pdf-minted-process)))
         (spec `((?B . ,(shell-quote-argument org-latex-bib-compiler))
                 (?L . ,(shell-quote-argument compiler))))
         (log-buf-name "*Org PDF LaTeX Output*")
         (log-buf (and (not snippet) (get-buffer-create log-buf-name)))
         (outfile (org-compile-file texfile process "pdf"
                                    (format "See %S for details" log-buf-name)
                                    log-buf spec)))
    (unless snippet
      (when org-latex-remove-logfiles
        (mapc #'delete-file
              (directory-files
               (file-name-directory outfile)
               t
               (concat (regexp-quote (file-name-base outfile))
                       "\\(?:\\.[0-9]+\\)?\\."
                       (regexp-opt org-latex-logfiles-extensions))
               t)))
      (let ((warnings (org-latex--collect-warnings log-buf)))
        (message (concat "PDF file produced"
                         (cond
                          ((eq warnings 'error) " with errors.")
                          (warnings (concat " with warnings: " warnings))
                          (t "."))))))
    ;; Return output file name.
    outfile))

(org-export-define-derived-backend 'my-latex 'latex
  :menu-entry
  '(?l "My export to LaTeX"
       ((?m "As PDF with minted" my/org-latex-export-to-pdf-minted)))
  ;; :translate-alist
  ;; '((quote-block . org-latex-testing-block))
  )

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

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; https://emacs.stackexchange.com/questions/27982/export-code-blocks-in-org-mode-with-minted-environment
;; (setq org-agenda-files'("~/Documents/org/personal/2022-02-06-agenda.org"))
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)
(setq org-latex-prefer-user-labels 1)
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
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'my/org-generate-custom-ids)))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;; (eval-after-load "flyspell"
;;   '(progn
;;      (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
;;      (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; (setq-default ispell-program-name "aspell")
;; (setq ispell-dictionary "castellano")
;; (setq flyspell-default-dictionary "castellano")
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
	(local-set-key (kbd "C-c b") 'my/color-link-region)))
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

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))
(use-package treemacs
  :ensure t)
(defun org-latex-math-block (_math-block contents _info)
  "Transcode a MATH-BLOCK object from Org to LaTeX.
                  CONTENTS is a string.  INFO is a plist used as a communication
                  channel."
  (when (org-string-nw-p contents)
    (format "$%s$" (org-trim contents))))
(defun create-temp-directory ()
  "This function let you create directories or files
                    in the tmp directory for testing"
  (interactive)
  (let (
        (choices '("directory" "files"))
        (name (read-string "Enter name temporary file: ")))

    (find-file (concat "/tmp/" name))
    (message name)

    ))
(global-set-key (kbd "\C-c M-+") 'create-temp-directory)

(windmove-default-keybindings 'M) ;; Me muevo por las ventanas

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))


(global-set-key (kbd "C-x 4") 'window-toggle-split-direction)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
;; (ido-mode 1)
(global-set-key (kbd "M-o") 'ace-window)

;; (defcustom TeX-buf-close-at-warnings-only t
;;   "Close TeX buffer if there are only warnings."
;;   :group 'TeX-output
;;   :type 'boolean)

;; (defun my-tex-close-TeX-buffer (_output)
;;   "Close compilation buffer if there are no errors.
;; Hook this function into `TeX-after-compilation-finished-functions'."
;;   (let ((buf (TeX-active-buffer)))
;;     (when (buffer-live-p buf)
;;       (with-current-buffer buf
;;         (when (progn (TeX-parse-all-errors)
;;                      (or
;;                       (and TeX-buf-close-at-warnings-only
;;                            (null (cl-assoc 'error TeX-error-list)))
;;                       (null TeX-error-list)))
;;           (cl-loop for win in (window-list)
;;                    if (eq (window-buffer win) (current-buffer))
;;                    do (delete-window win)))))))

;; (add-hook 'TeX-after-compilation-finished-functions #'my-tex-close-TeX-buffer)

;; (defun bury-compile-buffer-if-successful (buffer string)
;;   "Bury a compilation buffer if succeeded without warnings "
;;   (if (and
;;        (string-match "compilation" (buffer-name buffer))
;;        (string-match "finished" string)
;;        (not
;;         (with-current-buffer buffer
;;           **(goto-char 1)**
;;           (search-forward "warning" nil t))))
;;       (run-with-timer 1 nil
;;                       (lambda (buf)
;;                         (bury-buffer buf)
;;                         (switch-to-prev-buffer (get-buffer-window buf) 'kill))
;;                       buffer)))
;; (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)
;; (setq org-image-actual-width nil)
;; (use-package exec-path-from-shell)
;;    (setq org-latex-create-formula-image-program 'dvipng)
;;    (setq org-latex-listings 'minted)
;;  (require 'ox-latex)
;;  (add-to-list 'org-latex-packages-alist '("" "minted"))
;;  (add-to-list 'org-latex-packages-alist '("" "minted" nil))
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
;; (use-package org-fragtog)
;; (add-hook 'org-mode-hook 'org-fragtog-mode)
;; (global-visual-line-mode 1)

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
        (cons msg code)
        ))
(use-package lsp-ltex
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-version "15.2.0"))  ; make sure you have set this, see below

(use-package all-the-icons
  :if (display-graphic-p))

; from enberg on #emacs
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
