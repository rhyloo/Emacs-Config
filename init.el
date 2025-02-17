
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq my-user-init-file "README.org")
(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/org/inbox.org"))
 '(org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
 '(org-safe-remote-resources
   '("\\`/ssh:root@www\\.rhyloo\\.com:/root/content/org/blog/index\\.org\\'"))
 '(package-selected-packages
   '(languagetool writegood-mode org-caldav org-auto-tangle org-special-block-extras lsp-ui lsp-mode yasnippet htmlize minions org-download undo-tree multiple-cursors ox-odt vscode-dark-plus-theme use-package treemacs pyvenv python-mode pdf-tools matlab-mode lua-mode json-mode flycheck counsel company))
 '(safe-local-variable-values
   '((eval setq org-babel-tangle-default-file-mode o755)
     (eval setq org-babel-tangle-default-file-mode 755)
     (eval add-hook 'after-save-hook
           (lambda nil
             (run-with-idle-timer 0.1 nil
                                  (lambda nil
                                    (org-babel-tangle))))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (let
                 ((default-directory "/ssh:root@www.rhyloo.com:/root/content/test.el"))
               (org-babel-tangle)))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (if
                 (y-or-n-p "Tangle?")
                 (org-babel-tangle)))
           nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((((class color) (min-colors 88) (background light)) (:inherit (font-lock-function-name-face))) (((class grayscale mono) (background dark)) (:inherit (font-lock-function-name-face))) (((class color) (background light)) (:inherit (font-lock-function-name-face))) (((class color) (min-colors 88) (background dark)) (:foreground "white")) (((background dark)) (:foreground "white")) (t (:foreground "white")))))
