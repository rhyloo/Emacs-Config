;;; early-init.el --- Emacs early init file for Emacs 30.2.5  -*- lexical-binding: t -*-
;; Company:       RhylooSolutions
;; Engineer:      Rhyloo
;; 
;; Create Date:   2025-12-25
;; Project Name:  Personal Emacs configuration
;; Version:       0.0.1v
;;
;;; Description:
;;
;; This is the main configuration file for Emacs.
;; In this file:
;; - Disable package.el manager
;;
;;; Revision:
;;
;; Revision 0.0.1 - File Created
;;
;;; Additional Comments:
;;
;; -----------------------------------------------------------------------------

;;; Code:
(setq package-enable-at-startup nil) ;; Disable package.el in favor of elpaca.el

;; Startup speed: no garbage collection and no file handlers while loading the
;; configuration. Both are restored once, here, when startup finishes.
(defvar my/default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024) ; 32 MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/default-file-name-handler-alist)))
;;; early-init.el ends here
