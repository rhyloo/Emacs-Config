;;; init.el --- Emacs init file for Emacs 30.2.5  -*- lexical-binding: t -*-
;; Company:       RhylooSolutions
;; Engineer:      Rhyloo
;; 
;; Create Date:   2025-12-25
;; Project Name:  Personal Emacs configuration
;; Version:       0.0.2v
;;
;;; Description:
;;
;; This is the main configuration file for Emacs.
;; In this file:
;; - Set no-littering etc and var locations
;; - Set elpaca as default install method for use-package
;; - Set eln-cache location for no-littering
;; - Set the bootstratp for elpaca.el
;; - Install the latest release of org
;; - Load no-littering
;; - Set elpaca for no-littering
;; - Add elpaca-use-package-mode
;; - Load the file ~/.config/emacs/slv-config.org
;;
;;; Revision:
;;
;; Revision 0.0.1 - File Created
;;
;;; Additional Comments:
;;
;; -----------------------------------------------------------------------------

;;; Code:
;; Set alternative locations for no-littering
(setq no-littering-etc-directory (expand-file-name "~/.cache/emacs/etc")
      no-littering-var-directory (expand-file-name "~/.cache/emacs/var"))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory)))
;; -----------------------------------------------------------------------------
;; Uncomment next line for use-package statistics
(defvar use-package-compute-statistics t)
;; -----------------------------------------------------------------------------
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory)))
;; -----------------------------------------------------------------------------
;; Needed to avoid elpaca warning
(setq elpaca-core-date '(20251225)) ;; Build date of Emacs in my system
;; -----------------------------------------------------------------------------
;; Define elpaca bootstrap (Copied from: https://github.com/progfolio/elpaca)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" no-littering-var-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; -----------------------------------------------------------------------------
;; Install elpaca use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
;; -----------------------------------------------------------------------------
;; Update transient
(use-package transient
  :ensure (:host github :repo "magit/transient" :branch "main")
  :defer t)
;; -----------------------------------------------------------------------------
;; Keep emacs clean!
(use-package no-littering
  :ensure t
  :demand t  
  :config
  ;; Enable backups logic
  (no-littering-theme-backups)
  ;; Set custom files (Cleaned up: You had 'custom-file' defined twice)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")
        url-history-file (no-littering-expand-etc-file-name "url/history")))
;; -----------------------------------------------------------------------------
;; Wait to every queued elpaca order to finish
(elpaca-wait)
;; -----------------------------------------------------------------------------
;; Load org config file
(let* ((base-name "README")
       (org-file (expand-file-name (concat base-name ".org") user-emacs-directory))
       (el-file  (expand-file-name (concat base-name ".el")  user-emacs-directory)))

  ;; Si el .org es más nuevo que el .el (o el .el no existe), generamos el .el
  ;; Usamos un proceso externo para no cargar Org en esta sesión todavía.
  (when (and (file-exists-p org-file)
             (or (not (file-exists-p el-file))
                 (time-less-p (file-attribute-modification-time (file-attributes el-file))
                              (file-attribute-modification-time (file-attributes org-file)))))
    (message "Tangling configuration file...")
    (call-process (concat invocation-directory invocation-name) nil nil nil
                  "-Q" "--batch" "--eval" "(require 'org)"
                  "--eval" (format "(org-babel-tangle-file \"%s\" \"%s\" \"emacs-lisp\")" org-file el-file)))

  ;; Cargamos el archivo .el resultante
  (if (file-exists-p el-file)
      (load-file el-file)
    (error "No se pudo cargar la configuración: %s no existe" el-file)))
;; -----------------------------------------------------------------------------
(provide 'init)
;; -----------------------------------------------------------------------------
;;; init.el ends here

