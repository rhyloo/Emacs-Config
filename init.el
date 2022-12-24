(setq my-user-init-file "README.org")

(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(arduino-cli-mode platformio-mode undo-tree cmake-mode ws-butler which-key websocket weblorg web-server vscode-dark-plus-theme vs-dark-theme use-package treemacs swiper super-save sr-speedbar solaire-mode slime simple-httpd scihub pyvenv python-mode popup pdf-tools ox-reveal org-special-block-extras org-roam org-ref org-present org-noter org-make-toc org-gcal org-fragtog neotree multiple-cursors mu4e-alert minions matlab-mode markdown-preview-eww lua-mode lsp-ui lsp-ltex ledger-mode json-mode guess-language fountain-mode forge flycheck exec-path-from-shell epresent edit-indirect doom-modeline deft company-arduino benchmark-init auctex-latexmk all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
