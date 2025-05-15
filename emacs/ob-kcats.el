(defcustom kcats-babel-executable "kcats"
  "Location of the kcats binary"
  :type 'string
  :group 'kcats-babel)

(defcustom kcats-kill-buffer-on-success t
  "Whether to automatically kill the output buffer when process completes successfully."
  :type 'boolean
  :group 'kcats-babel)

(defun org-babel-execute:kcats (body params)
  "Execute a block of kcats code with org-babel.
Supports incremental output and preserves output on interruption."
  (let ((output-buffer-name "*kcats-output*")
        (captured-output nil)
        (process nil)
        (kill-buffer (if (assq :keep-buffer params)
                         (not (equal (cdr (assq :keep-buffer params)) "yes"))
                         kcats-kill-buffer-on-success))
        (executable (executable-find kcats-babel-executable)))
    
    ;; Create output buffer
    (when (get-buffer output-buffer-name)
      (kill-buffer output-buffer-name))
    (let ((output-buffer (generate-new-buffer output-buffer-name)))
      
      ;; Display output buffer
      (display-buffer output-buffer)
      
      (with-current-buffer output-buffer
        (erase-buffer)
        (insert "Running kcats...\n\n")
        ;; Add local variable to store output in case buffer is killed
        (setq-local kcats-captured-output "")
        ;; Add key binding to kill process when buffer is killed
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (and (boundp 'process) process (process-live-p process))
                      (setq captured-output (buffer-local-value 'kcats-captured-output (current-buffer)))
                      (interrupt-process process)
                      (delete-process process)))
                  nil t))
      
      ;; Start the process
      (setq process (make-process
                     :name "kcats-process"
                     :buffer output-buffer
                     :command (list executable)
                     :connection-type 'pipe
                     :sentinel (lambda (proc event)
                                 (when (buffer-live-p (process-buffer proc))
                                   (with-current-buffer (process-buffer proc)
                                     (let ((inhibit-read-only t))
                                       (goto-char (point-max))
                                       (insert (format "\n\n--- Process %s ---\n" (string-trim event)))))))
                     :filter (lambda (proc output-str)
                               (when (buffer-live-p (process-buffer proc))
                                 (with-current-buffer (process-buffer proc)
                                   (let ((inhibit-read-only t))
                                     (goto-char (point-max))
                                     (insert output-str)
                                     ;; Update our captured output
                                     (setq-local kcats-captured-output 
                                                 (concat kcats-captured-output output-str))))))))
      
      ;; Send the code with a newline at the end
      (process-send-string process (concat body "\n"))
      (process-send-eof process)
      
      ;; Wait for process with the working timeout value
      (while (and (process-live-p process)
                  (accept-process-output process 0.1)))
      
      ;; Collect the output - strip the header and footer
      (let ((final-output
             (if (not (buffer-live-p output-buffer))
                 ;; If buffer was killed, use our saved output
                 (if captured-output
                     captured-output
                   "Process was interrupted and output buffer was killed.")
               ;; If buffer is still alive, extract content normally
               (with-current-buffer output-buffer
                 (goto-char (point-min))
                 (forward-line 2) ;; Skip past "Running kcats...\n\n"
                 (let ((output (buffer-substring (point)
                                                 (progn
                                                   (goto-char (point-max))
                                                   (if (re-search-backward "^--- Process " nil t)
                                                       (match-beginning 0)
                                                     (point-max))))))
                   ;; Check if process exited normally and kill buffer if needed
                   (when (and kill-buffer
                              (memq (process-status process) '(finished exited)))
                     (run-with-timer 0.5 nil 'kill-buffer output-buffer))
                   ;; Return the cleaned output
                   (string-trim output))))))
        (kill-buffer output-buffer)
        final-output))))
