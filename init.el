(setq my-user-init-file "README.org")

(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))
