(setq my-user-init-file "README.org")

(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(electric-indent-mode nil)
;;  '(enable-local-eval 'maybe)
;;  '(enable-local-variables :all)
;;  '(global-company-mode t)
;;  '(org-agenda-files
;;    '("/home/rhyloo/Documents/org/personal/2022-02-06-agenda.org"))
;;  '(org-roam-graph-viewer nil)
;;  '(package-selected-packages
;;    '(all-the-icons org-special-block-extras mu4e-alert mu4 mu mu4e- company-arduino arduino-mode deft helm-bibtex citar which-key sci-hub org-roam-ui websocket yasnippet-snippets xkcd ws-butler use-package telega swiper super-save spacegray-theme smartparens simple-httpd scihub request-deferred python-mode persist pdf-tools org-tree-slide minions matlab-mode magit lua-mode lsp-ui ledger-mode google-translate emojify emms elpy editorconfig doom-themes doom-modeline bufshow bongo blacken benchmark-init auctex alert a))
;;  '(send-mail-function 'smtpmail-send-it)
;;  '(smtpmail-smtp-server "correo.uma.es")
;;  '(smtpmail-smtp-service 587))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(mode-line ((t (:background "dark grey" :foreground "grey10" :box (:line-width 1 :color "grey10" :style unspecified) :overline "grey10" :underline nil :height 100))))
;;  '(mode-line-inactive ((t (:height 110)))))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(arduino-executable "/home/rhyloo/.local/Software/arduino-cli")
 '(comment-auto-fill-only-comments t)
 '(comment-style 'indent)
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("6c4c97a17fc7b6c8127df77252b2d694b74e917bab167e7d3b53c769a6abb6d6" default))
 '(global-display-line-numbers-mode t)
 '(ispell-dictionary nil)
 '(markdown-command "/usr/bin/markdown")
 '(markdown-open-command "/usr/bin/grip")
 '(org-agenda-files '("~/Documents/Agenda/Universidad.org"))
 '(package-selected-packages
   '(markdown-preview-eww edit-indirect json-mode multiple-cursors guess-language forge doom-modeline flycheck cmake-mode auto-complete ws-butler which-key vscode-dark-plus-theme use-package treemacs swiper super-save solaire-mode simple-httpd scihub pyvenv python-mode pdf-tools ox-reveal org-special-block-extras org-roam org-ref org-present org-noter org-make-toc mu4e-alert minions matlab-mode magit lua-mode lsp-ui lsp-ltex ledger-mode epresent deft company-arduino benchmark-init auctex all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
