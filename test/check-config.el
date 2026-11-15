;;; check-config.el --- smoke test for this Emacs configuration  -*- lexical-binding: t -*-
;;
;; Loaded by test/check-config.sh after init. Waits for Elpaca to finish, then
;; checks startup time, Org (the Elpaca build, not the built-in one) and the
;; Markdown live preview with Mermaid, reading aids, dirvish, diff-hl, the
;; ghostel terminal and the Claude Code bindings; writes a report to
;; $PROBE_OUT and exits.
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
    (push (format "reading: olivetti=%s imenu-key=%s"
                  (bound-and-true-p olivetti-mode) (key-binding (kbd "M-i")))
          out)
    ;; Own ports, so the test also runs while a real preview is open.
    (setq markdown-preview-auto-open nil
          markdown-preview-ws-port 17379
          markdown-preview-http-port 19000)
    (markdown-preview-mode 1)
    (push (format "preview-mode=%s ws=%s" markdown-preview-mode (and markdown-preview--websocket-server t)) out)
    (let ((html markdown-preview-file-name))
      (push (format "preview html exists=%s mermaid-js=%s onupdate=%s"
                    (file-exists-p html)
                    (and (file-exists-p html) (with-temp-buffer (insert-file-contents html) (search-forward "mermaid.min.js" nil t) t))
                    (and (file-exists-p html) (with-temp-buffer (insert-file-contents html) (search-forward "language-mermaid" nil t) t)))
            out))
    (push (format "markdown html: %s" (string-trim (shell-command-to-string (concat markdown-command " < " (expand-file-name "test.md" func-dir) " | grep -c language-mermaid")))) out)
    ;; A preview page that was closed must not break later updates.
    (let* ((dead (make-process :name "dead" :command '("true")))
           (_ (while (process-live-p dead) (accept-process-output dead 0.05)))
           (stale (websocket-inner-create :conn dead :url "ws://closed" :ready-state 'open)))
      (puthash markdown-preview--uuid stale markdown-preview--remote-clients)
      (markdown-preview--send-preview markdown-preview--uuid)
      (sit-for 0.5)
      (push (format "preview dead-page: dropped=%s errors=%s"
                    (null (gethash markdown-preview--uuid markdown-preview--remote-clients))
                    (with-current-buffer "*Messages*"
                      (save-excursion (goto-char (point-min))
                                      (and (search-forward "====> Error" nil t) t))))
            out))
    (markdown-preview-mode -1)
    ;; Files: dired must open through dirvish.
    (dired func-dir)
    (push (format "files: dirvish=%s side-key=%s"
                  (and (fboundp 'dirvish-curr) (dirvish-curr) t)
                  (key-binding (kbd "<f8>")))
          out)
    (push (format "git: diff-hl=%s" (bound-and-true-p global-diff-hl-mode)) out)
    ;; Slow-path guard: git state per file in dirvish spawns a process per refresh.
    (push (format "dirvish: vc-state=%s" (and (or (memq 'vc-state dirvish-attributes)
                                                  (memq 'vc-state dirvish-side-attributes))
                                              t))
          out)
    ;; Terminal: ghostel must start a live shell with its native module.
    (let ((term (ghostel)))
      (push (format "terminal: ghostel-live=%s"
                    (and (bufferp term) (process-live-p (get-buffer-process term)) t))
            out)
      (let ((kill-buffer-query-functions nil)) (kill-buffer term)))
    ;; Loading it also runs its :config (the Emacs MCP tools setup).
    (require 'claude-code-ide)
    (require 'claude-code-ide-mcp-http-server)
    (push (format "claude-mcp: setLevel=%s unknown-method=%s"
                  (hash-table-p (claude-code-ide-mcp-http-server--dispatch "logging/setLevel" nil))
                  (condition-case nil
                      (progn (claude-code-ide-mcp-http-server--dispatch "no/such-method" nil) 'answered)
                    (error 'caught-as-error)))
          out)
    (push (format "claude: key=%s cli=%s backend=%s"
                  (key-binding (kbd "C-c C-'"))
                  (and (executable-find "claude") t)
                  claude-code-ide-terminal-backend)
          out)
    ;; Static page with the same scripts as the live preview, for
    ;; check-config.sh to render in headless Chrome and look for diagrams.
    (with-temp-file (expand-file-name "mermaid.html" func-dir)
      (insert "<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
              (mapconcat (lambda (src) (format "<script src=\"%s\"></script>" src))
                         markdown-preview-javascript "")
              "<script>" markdown-preview-script-oninit "</script></head><body>"
              (shell-command-to-string
               (concat markdown-command " < " (expand-file-name "test.md" func-dir)))
              "<script>" markdown-preview-script-onupdate "</script></body></html>"))
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
