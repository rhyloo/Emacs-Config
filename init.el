(package-initialize)
;; Functions
(defun load-if-exists (f)
  (if (file-exists-p (expand-file-name f))
      (load-file (expand-file-name f))))

(load-if-exists "~/.emacs.d/secrets.el")

(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(gnus-desktop-notify anki-editor anki-connect yasnippet writegood-mode vscode-dark-plus-theme undo-tree treemacs pyvenv python-mode pdf-tools ox-odt org-special-block-extras org-download org-caldav multiple-cursors minions matlab-mode magit lua-mode lsp-ui languagetool json-mode htmlize flycheck counsel consult-org-roam company))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (run-with-idle-timer 0.1 nil
				  (lambda nil
				    (org-babel-tangle))))
	   nil t)
     (eval add-hook 'before-save-hook
	   (lambda nil
	     (org-babel-ref-resolve "photo_generation"))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(which-func ((((class color) (min-colors 88) (background light)) (:inherit (font-lock-function-name-face))) (((class grayscale mono) (background dark)) (:inherit (font-lock-function-name-face))) (((class color) (background light)) (:inherit (font-lock-function-name-face))) (((class color) (min-colors 88) (background dark)) (:foreground "green")) (((background dark)) (:foreground "red")) (t (:foreground "red")))))
