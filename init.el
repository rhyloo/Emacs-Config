(package-initialize)

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
 '(org-agenda-files
   '("~/Documents/org-mode-files/Agenda_Fastmail.org" "/home/rhyloo/Documents/org-mode-files/Agenda.org"))
 '(package-selected-packages
   '(auto-complete-c-headers sr-speedbar projectile company-irony dashboard multiple-cursors pyvenv python-mode hledger-mode org-caldav appt org-super-agenda writegood-mode vscode-dark-plus-theme undo-tree minions magit counsel company))
 '(safe-local-variable-values
   '((eval setq-local my/c-eldoc-includes
	   (list
	    (concat
	     (projectile-project-root)
	     "include")))
     (company-clang-arguments list
			      ("-I/home/rhyloo/Documents/nymbolator/include/"))
     (company-clang-arguments "-I/home/rhyloo/Documents/nymbolator/include/")
     (company-clang-arguments "-I/home/rhyloo/nymbolator/include/")
     (eval add-to-list 'irony-additional-clang-options
	   (concat "-I"
		   (projectile-project-root)
		   "include"))
     (eval setq-local company-c-headers-path-system
	   (list
	    (concat
	     (projectile-project-root)
	     "include")))
     (eval setq-local company-clang-arguments
	   (list
	    (concat
	     (projectile-project-root)
	     "include")
	    "-std=c11"))
     (eval setq-local company-clang-arguments
	   (list
	    (concat "-I"
		    (projectile-project-root)
		    "include")
	    "-std=c11"))
     (eval setq-local lsp-clients-clangd-args
	   '("-Iinclude"))
     (eval setq-local flycheck-clang-include-path
	   '("include"))
     (eval setq-local company-clang-arguments
	   '("-Iinclude"))
     (eval add-hook 'before-save-hook
	   (lambda nil
	     (org-babel-ref-resolve "photo_generation"))
	   nil t)
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (run-with-idle-timer 0.1 nil
				  (lambda nil
				    (org-babel-tangle))))
	   nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-keyword-face :weight bold))))
 '(org-agenda-date-today ((t (:background "yellow" :weight bold))))
 '(which-func ((((class color) (min-colors 88) (background light)) (:inherit (font-lock-function-name-face))) (((class grayscale mono) (background dark)) (:inherit (font-lock-function-name-face))) (((class color) (background light)) (:inherit (font-lock-function-name-face))) (((class color) (min-colors 88) (background dark)) (:foreground "green")) (((background dark)) (:foreground "red")) (t (:foreground "red")))))
