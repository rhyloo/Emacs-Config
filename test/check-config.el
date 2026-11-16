;;; check-config.el --- smoke test for this Emacs configuration  -*- lexical-binding: t -*-
;;
;; Loaded by test/check-config.sh after init. Waits for Elpaca to finish, then
;; checks startup time, Org (the Elpaca build, not the built-in one) and the
;; Markdown live preview with Mermaid, reading aids, the dired side panel, diff-hl, the
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
    ;; In-buffer diagrams: headless Chrome must produce a sized SVG that keeps
    ;; spaces, and markup hiding must be on in every Markdown buffer.
    (let* ((source (nth 2 (car (my/mermaid--blocks))))
           (file (my/mermaid--svg-file source)))
      (when (file-exists-p file) (delete-file file))
      (my/mermaid--draw (list source) #'ignore)
      (let ((deadline (+ (float-time) 60)))
        (while (and (not (file-exists-p file)) (< (float-time) deadline))
          (accept-process-output nil 0.2)))
      (push (format "inline-diagram: svg=%s sized=%s spaces=%s hide-markup=%s"
                    (file-exists-p file)
                    (and (file-exists-p file)
                         (with-temp-buffer (insert-file-contents file)
                                           (and (re-search-forward "<svg[^>]* width=\"[0-9.]+\"" nil t) t)))
                    (and (file-exists-p file)
                         (with-temp-buffer (insert-file-contents file)
                                           (and (search-forward "xml:space=\"preserve\"" nil t) t)))
                    markdown-hide-markup)
            out))
    ;; Live view: a table is drawn with box lines that fit the text column,
    ;; and its Markdown comes back while the cursor is inside it.
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n| Pantalla | Para qué |\n|---|---|\n| Hoy | "
              (string-join (make-list 40 "texto") " ") " |\n\nfin\n"))
    ;; The test runs in a terminal, where markdown-mode-hook skips the live
    ;; view; turn it on by hand so its hooks are installed too.
    (my/md-live-mode 1)
    ;; One overlay per Markdown line, so the cursor goes row by row.
    (let* ((block (sort (seq-filter (lambda (o) (eq (overlay-get o 'my/md-live) 'table))
                                    (overlays-in (point-min) (point-max)))
                        (lambda (a b) (< (overlay-start a) (overlay-start b)))))
           (row (car (last block)))
           (drawn (mapconcat (lambda (o) (overlay-get o 'display)) block "\n"))
           (fits (cl-every (lambda (l) (<= (string-width l) (/ (my/mermaid--max-width) (my/md-live--char-width))))
                           (split-string drawn "\n")))
           (all-drawn (lambda () (cl-every (lambda (o) (overlay-get o 'display)) block))))
      ;; Reading: opened read-only, no line numbers, and the table stays
      ;; drawn with the cursor inside it.
      (push (format "reading-mode: read-only=%s line-numbers=%s"
                    buffer-read-only (bound-and-true-p display-line-numbers-mode))
            out)
      (goto-char (overlay-start row))
      (my/md-live--update)
      (let ((drawn-while-reading (funcall all-drawn)))
        ;; Editing: leaving read-only reveals the whole table's Markdown.
        (read-only-mode -1)
        (let ((revealed (cl-notany (lambda (o) (overlay-get o 'display)) block)))
          (goto-char (point-max))
          (my/md-live--update)
          (push (format "table: rows=%s boxed=%s fits=%s drawn-while-reading=%s revealed-inside=%s drawn-outside=%s"
                        (length block) (and (string-match-p "┌" drawn) t) fits
                        drawn-while-reading revealed (funcall all-drawn))
                out))))
    (set-buffer-modified-p nil)
    ;; Files: plain dired, keeping the normal mode line (time, battery...).
    (make-directory (expand-file-name "carpeta/sub" func-dir) t)
    ;; Random bytes: a compressing filesystem (ZFS, btrfs) would make du
    ;; report a repeated character as a few K.
    (call-process "sh" nil nil nil "-c" "head -c 300000 /dev/urandom > \"$1\"" "sh"
                  (expand-file-name "carpeta/sub/datos" func-dir))
    ;; ZFS only shows the new blocks to du once its transaction group is
    ;; written (a few seconds); wait for that so the size is the real one.
    (let ((deadline (+ (float-time) 20)))
      (while (and (< (float-time) deadline)
                  (< (string-to-number
                      (shell-command-to-string
                       (format "sync; du -sk -- %s | cut -f1"
                               (shell-quote-argument (expand-file-name "carpeta" func-dir)))))
                     250))
        (sleep-for 0.5)))
    (dired func-dir)
    ;; Folder sizes: du replaces the folder's own 4K with what it holds.
    (let ((deadline (+ (float-time) 20)))
      (while (and my/dired--du (< (float-time) deadline)
                  ;; Until its sentinel has written the sizes (it kills the
                  ;; output buffer); the process ends a moment before that.
                  (or (process-live-p my/dired--du) (buffer-live-p (process-buffer my/dired--du))))
        (accept-process-output nil 0.1)))
    ;; The du sizes are wider than ls's, but must not push any name aside.
    (let (columns)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (dired-move-to-filename) (push (current-column) columns))
          (forward-line 1)))
      (push (format "dired-names: lines=%s aligned=%s"
                    (length columns) (and columns (null (cdr (delete-dups columns))) t))
            out))
    (goto-char (point-min))
    (search-forward "carpeta")
    (let ((size-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (dired-subtree-insert)
      (forward-line 0)
      ;; A coloured │ hangs the subfolder's files under its icon.
      (let* ((name (dired-move-to-filename))
             (guided (seq-some (lambda (o)
                                 (and (overlay-get o 'my/dired-guide)
                                      (string-match-p "│" (overlay-get o 'after-string))))
                               (overlays-in (1- name) name))))
        ;; A revert (dired does it by itself when the folder changes) must
        ;; keep the guides and the sizes of the main folder.
        (revert-buffer)
        (let ((deadline (+ (float-time) 20)))
          (while (and my/dired--du (< (float-time) deadline)
                  ;; Until its sentinel has written the sizes (it kills the
                  ;; output buffer); the process ends a moment before that.
                  (or (process-live-p my/dired--du) (buffer-live-p (process-buffer my/dired--du))))
            (accept-process-output nil 0.1)))
        (goto-char (point-min))
        (search-forward "carpeta")
        (let ((size-after (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (guides-after (seq-some (lambda (o) (overlay-get o 'my/dired-guide))
                                      (overlays-in (point-min) (point-max)))))
          (push (format "dired-tree: folder-size=%s guide=%s mode-name=%s after-revert: size=%s guide=%s"
                        (and (string-match-p "\\b\\(29[0-9]\\|3[0-3][0-9]\\)K\\b" size-line) t)
                        (and guided t) mode-name
                        (and (string-match-p "\\b\\(29[0-9]\\|3[0-3][0-9]\\)K\\b" size-after) t)
                        (and guides-after t))
                out))))
    (push (format "files: mode=%s own-mode-line=%s icons=%s side-key=%s"
                  major-mode (local-variable-p 'mode-line-format)
                  (bound-and-true-p nerd-icons-dired-mode) (key-binding (kbd "<f8>")))
          out)
    ;; F8 opens the side panel and closes it again.
    (find-file (expand-file-name "test.md" func-dir))
    (delete-other-windows)
    (my/files-side)
    (let ((opened (and (window-parameter (selected-window) 'my/files-side)
                       (derived-mode-p 'dired-mode) t)))
      (my/files-side)
      (push (format "side-panel: opened=%s closed=%s"
                    opened (null (seq-find (lambda (w) (window-parameter w 'my/files-side))
                                           (window-list))))
            out))
    (push (format "git: diff-hl=%s" (bound-and-true-p global-diff-hl-mode)) out)
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
    ;; Terminator colours (enabled only in GUI; enabled here to check them).
    (enable-theme 'terminator)
    (push (format "terminator: bg=%s fg=%s active=%s inactive=%s font-ok=%s"
                  (face-attribute 'default :background)
                  (face-attribute 'default :foreground)
                  (face-attribute 'mode-line :background)
                  (face-attribute 'mode-line-inactive :background)
                  (and (member "JetBrains Mono" (split-string (shell-command-to-string "fc-list : family") "[\n,]")) t))
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
                  ;; `elpaca--status' was replaced by the `elpaca<-status'
                  ;; accessor in newer Elpaca; support both.
                  (when (cl-every (lambda (c)
                                    (let ((e (if (consp c) (cdr c) c)))
                                      (memq (if (fboundp 'elpaca--status) (elpaca--status e) (elpaca<-status e))
                                            '(finished failed))))
                                  (elpaca--queued))
                    (condition-case err (func-run)
                      (error (with-temp-file (getenv "PROBE_OUT") (insert (format "ERROR: %S" err))) (kill-emacs 1))))))
;;; check-config.el ends here
