;;; check-config.el --- smoke test for this Emacs configuration  -*- lexical-binding: t -*-
;;
;; Loaded by test/check-config.sh after init. Waits for Elpaca to finish, then
;; checks startup time, Org (the Elpaca build, not the built-in one) and the
;; Markdown live preview with Mermaid, writes a report to $PROBE_OUT and exits.
;;
;;; Code:
(require 'cl-lib)
(defvar func-dir (getenv "FUNC_DIR"))
(defun func-run ()
  (let (out)
    (push (format "init-time: %s  gcs-done: %s" (emacs-init-time) gcs-done) out)
    (find-file (expand-file-name "test.org" func-dir))
    (push (format "org: mode=%s version=%s lib=%s" major-mode (org-version) (locate-library "org")) out)
    (find-file (expand-file-name "test.md" func-dir))
    (push (format "md: mode=%s preview-bound=%s" major-mode (key-binding (kbd "C-c C-c p"))) out)
    (setq markdown-preview-auto-open nil)
    (markdown-preview-mode 1)
    (push (format "preview-mode=%s ws=%s" markdown-preview-mode (and markdown-preview--websocket-server t)) out)
    (let ((html (expand-file-name ".markdown-preview.html" func-dir)))
      (push (format "preview html exists=%s mermaid-js=%s onupdate=%s"
                    (file-exists-p html)
                    (and (file-exists-p html) (with-temp-buffer (insert-file-contents html) (search-forward "mermaid.min.js" nil t) t))
                    (and (file-exists-p html) (with-temp-buffer (insert-file-contents html) (search-forward "language-mermaid" nil t) t)))
            out))
    (push (format "markdown html: %s" (string-trim (shell-command-to-string (concat markdown-command " < " (expand-file-name "test.md" func-dir) " | grep -c language-mermaid")))) out)
    (markdown-preview-mode -1)
    (with-temp-file (getenv "PROBE_OUT") (insert (mapconcat #'identity (nreverse out) "\n")
                                                 "\n--- *Warnings* ---\n"
                                                 (if (get-buffer "*Warnings*") (with-current-buffer "*Warnings*" (buffer-string)) "none"))))
  (kill-emacs 0))
(run-with-timer 1 0.5
                (lambda ()
                  (when (cl-every (lambda (c) (memq (elpaca--status (cdr c)) '(finished failed))) (elpaca--queued))
                    (condition-case err (func-run)
                      (error (with-temp-file (getenv "PROBE_OUT") (insert (format "ERROR: %S" err))) (kill-emacs 1))))))
;;; check-config.el ends here
