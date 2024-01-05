(setq my-user-init-file "README.org")
(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("993aac313027a1d6e70d45b98e121492c1b00a0daa5a8629788ed7d523fe62c1" default))
 '(markdown-command "/usr/bin/markdown" t)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(languagetool org-tidy ox-hugo org-static-blog org2blog magit-pretty-graph elmacro org-gtasks mu4e counsel arduino-cli-mode platformio-mode undo-tree cmake-mode ws-butler which-key websocket weblorg web-server vscode-dark-plus-theme vs-dark-theme use-package treemacs swiper super-save sr-speedbar solaire-mode slime simple-httpd scihub pyvenv python-mode popup pdf-tools ox-reveal org-special-block-extras org-roam org-ref org-present org-noter org-make-toc org-gcal org-fragtog neotree multiple-cursors mu4e-alert minions matlab-mode markdown-preview-eww lua-mode lsp-ui lsp-ltex ledger-mode json-mode guess-language fountain-mode forge flycheck exec-path-from-shell epresent edit-indirect doom-modeline deft company-arduino benchmark-init auctex-latexmk all-the-icons))
 '(safe-local-variable-values '((eval add-hook 'after-save-hook 'org-babel-tangle)))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
