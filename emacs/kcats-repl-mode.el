(require 'comint)

(defun kcats-send (proc code)
  "Send the CODE to the kcats interpreter and return the result."
  (message "Sending: %s" code)
  (let* ((code-len (+ (string-bytes code) 1))
         (code-str (format "%d\n%s" code-len code)))
    (with-temp-buffer
      (insert code-str)
      (process-send-region proc (point-min) (point-max)))
    (process-send-string proc "\n")
    ;;(accept-process-output proc)
    ))

(defun kcats-repl ()
  "Start a REPL for process kcats."
  (interactive)
  (let ((buffer (get-buffer-create "*kcats* REPL")))
    (switch-to-buffer buffer)
    (unless (comint-check-proc buffer)
      (let ((buffer (comint-exec buffer "kcats" kcats-babel-executable nil '("-i")))
            (process (get-buffer-process buffer)))
        (set-process-buffer process buffer)
        (set-process-query-on-exit-flag process nil)
        (set-process-sentinel
         process
         (lambda (process event)
           (when (string= event "finished\n")
             (message "kcats process terminated.")))))
      (kcats-repl-mode))))

(defun string-drop-first-line (s)
  (let ((lines (split-string s "\n" t)))
    (mapconcat 'identity (cdr lines) "\n")))

(defun kcats-repl-insert-prompt (s)
  (concat s "kcats> "))

(define-derived-mode kcats-repl-mode comint-mode "kcats REPL"
  "Major mode for interacting with the foo process."
  (smartparens-strict-mode t)
  (add-hook 'comint-preoutput-filter-functions 'string-drop-first-line)
  (add-hook 'comint-preoutput-filter-functions 'kcats-repl-insert-prompt)
  (setq comint-prompt-regexp "^kcats>")
  (setq comint-highlight-input nil)
  (setq comint-use-prompt-regexp t)
  (setq comint-input-sender 'kcats-send)
  (set-syntax-table kcats-mode-syntax-table)
  (setq font-lock-defaults '(kcats-font-lock-keywords)))

(define-abbrev-table 'kcats-repl-mode-abbrev-table kcats-abbrevs)
(add-hook 'kcats-repl-mode-hook 'abbrev-mode)
