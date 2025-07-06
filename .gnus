(setq gnus-inhibit-images nil)
(setq gnus-always-read-dribble-file nil)
(setq gnus-use-full-window nil)
(setq gnus-message-archive-group nil)

;; Método principal (vacío)
(setq gnus-select-method '(nnnil ""))

;; Método IMAP para Gmail
(setq gnus-secondary-select-methods
      '((nnimap "fastmail"
                (nnimap-address "imap.fastmail.com")  ; Servidor IMAP de Gmail
                (nnimap-server-port 993)            ; Puerto seguro
                (nnimap-stream ssl)                 ; Conexión SSL
                (nnimap-authenticator login)        ; Autenticación
                (nnimap-authinfo-file "~/.authinfo") ; Archivo de credenciales
                (nnmail-expiry-target "[Fastmail]/Trash") ; Carpeta de eliminados
                ;; (nnmail-expiry-wait 'immediate)     ; Expiración inmediata al borrar
                )))

;; Configuración adicional para SMTP (envío de correos)
(setq smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type  'ssl
      smtpmail-auth-credentials 'auth-source-search
      message-send-mail-function 'smtpmail-send-it)
