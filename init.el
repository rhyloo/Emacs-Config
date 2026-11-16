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
;; Set alternative locations for no-littering.
;; ~/.emacs.d keeps using ~/.cache/emacs; any other init directory (for example a
;; test copy started with `emacs --init-directory ~/.emacs-nuevo') gets its own
;; cache, so both configurations never share packages or state.
(defvar my/cache-directory
  (let ((name (file-name-nondirectory (directory-file-name user-emacs-directory))))
    (expand-file-name (if (string= name ".emacs.d") "emacs" (concat "emacs" name))
                      "~/.cache/")))
(setq no-littering-etc-directory (expand-file-name "etc" my/cache-directory)
      no-littering-var-directory (expand-file-name "var" my/cache-directory))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache" no-littering-var-directory)))
;; -----------------------------------------------------------------------------
;; Set to t and run `use-package-report' to see load times per package.
(defvar use-package-compute-statistics nil)
;; -----------------------------------------------------------------------------
;; Needed to avoid elpaca warning
(setq elpaca-core-date '(20251225)) ;; Build date of Emacs in my system
;; -----------------------------------------------------------------------------
;; Define elpaca bootstrap (Copied from: https://github.com/progfolio/elpaca)
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; -----------------------------------------------------------------------------
;; Install elpaca use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
;; -----------------------------------------------------------------------------
;; Ensure there is a package compatible with the Emacs Version
(use-package compat :ensure t)
;; Update transient
(use-package transient
  :ensure t
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
(setq my-user-init-file "blog")
;; Load org config file
(let* ((base-name my-user-init-file)
       (org-file (expand-file-name (concat base-name ".org") user-emacs-directory))
       (el-file  (expand-file-name (concat base-name ".el")  user-emacs-directory)))
  ;; Re-tangle only when the .org is newer than the .el (or the .el is missing),
  ;; so edits made outside Emacs, or a `git pull', are never silently ignored.
  (when (and (file-exists-p org-file)
             (file-newer-than-file-p org-file el-file))
    ;; Tangle in a separate Emacs: loading the built-in Org here would shadow
    ;; the newer Org that Elpaca installs ("org loaded before Elpaca activation").
    (call-process (expand-file-name invocation-name invocation-directory)
                  nil nil nil "--batch" "-Q" "--eval"
                  (format "(progn (require 'ob-tangle) (org-babel-tangle-file %S %S))"
                          org-file el-file)))
  (load el-file nil t))
;; -----------------------------------------------------------------------------
(provide 'init)
;; -----------------------------------------------------------------------------
;;; init.el ends here
