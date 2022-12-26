(setq my-user-init-file "README.org")

(org-babel-load-file
 (expand-file-name my-user-init-file
                   user-emacs-directory))

;; Experimental WARNING!!!!!!!!
(defconst dot-emacs (concat (getenv "HOME") "/" "~/.emacs.d/README.el")
  "My dot EMACS file.")

(require 'bytecomp)
(setq compiled-dot-emacs (byte-compile-dest-file dot-emacs))

(if (or (not (file-exists-p compiled-dot-emacs))
    	  (file-newer-than-file-p dot-emacs compiled-dot-emacs)
        (equal (nth 4 (file-attributes dot-emacs)) (list 0 0)))
    (load dot-emacs)
  (load compiled-dot-emacs))

(add-hook 'kill-emacs-hook
          '(lambda () (and (file-newer-than-file-p dot-emacs compiled-dot-emacs)
                           (byte-compile-file dot-emacs))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-command "/usr/bin/markdown" t)
 '(package-selected-packages
   '(mu4e counsel arduino-cli-mode platformio-mode undo-tree cmake-mode ws-butler which-key websocket weblorg web-server vscode-dark-plus-theme vs-dark-theme use-package treemacs swiper super-save sr-speedbar solaire-mode slime simple-httpd scihub pyvenv python-mode popup pdf-tools ox-reveal org-special-block-extras org-roam org-ref org-present org-noter org-make-toc org-gcal org-fragtog neotree multiple-cursors mu4e-alert minions matlab-mode markdown-preview-eww lua-mode lsp-ui lsp-ltex ledger-mode json-mode guess-language fountain-mode forge flycheck exec-path-from-shell epresent edit-indirect doom-modeline deft company-arduino benchmark-init auctex-latexmk all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
