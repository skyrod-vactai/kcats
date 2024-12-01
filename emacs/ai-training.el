
(defun ndk/get-keyword-key-value (kwd)
     (let ((data (cadr kwd)))
       (list (plist-get data :key)
             (plist-get data :value))))

(defun ndk/org-current-buffer-get-title ()
  (nth 1
       (assoc "TITLE"
              (org-element-map (org-element-parse-buffer 'greater-element)
                  '(keyword)
                #'ndk/get-keyword-key-value))))

(defun get-full-heading-path (point)
  "Get the full path of headings leading to POINT."
  (save-excursion
    (goto-char point)
    (let ((path '()))
      
      ;; Get current heading if we're at a heading
      (when (org-at-heading-p)
        (push (org-get-heading t t t t) path))
      (while (org-up-heading-safe)
        (push (org-get-heading t t t t) path))
      (push (ndk/org-current-buffer-get-title) path)
      (string-join path " > "))))

(defun clean-org-text (text)
  "Clean org-mode specific markup from TEXT."
  (when text
    (let ((cleaned text))
      ;; Remove org-mode specific lines
      ;(setq cleaned (replace-regexp-in-string "^#\\+.*$" "" cleaned))
      ;; Remove mode line
      (setq cleaned (replace-regexp-in-string "-\\*- .*-\\*-" "" cleaned))
      ;; Remove headline lines
      (setq cleaned (replace-regexp-in-string "^\\*+ .*$" "" cleaned))
      ;; Clean up multiple newlines
      (setq cleaned (replace-regexp-in-string "\n\n\n+" "\n\n" cleaned))
      ;; Trim
      (string-trim cleaned))))

(defun get-element-content (element)
  "Get content if element has direct text (not sub-headlines)."
  (let* ((begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (first-child (car (org-element-contents element))))
    (message "Element type: %s, First child type: %s" 
             (org-element-type element)
             (when first-child (org-element-type first-child)))
    ;; Only get content if first child is not a headline
    (when (and first-child 
               (not (eq (org-element-type first-child) 'headline)))
      (let ((content (buffer-substring-no-properties begin end)))
        (clean-org-text content)))))

(defun convert-org-to-jsonl ()
  "Convert org file to JSONL format with context."
  (interactive)
  (let* ((output-buffer (generate-new-buffer "*org-to-jsonl*"))
         (doc-title (ndk/org-current-buffer-get-title))
         (processed-content (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (let ((ast (org-element-parse-buffer)))
        (message "Processing AST...")
        ;; Process only headlines
        (org-element-map ast 'headline
          (lambda (element)
            ;; Get the section following this headline
            (let* (
                   (content (get-element-content element))
                   (heading-path (save-excursion
                                 (goto-char (org-element-property :begin element))
                                 (get-full-heading-path (point))))
                   (content-key (concat heading-path (or content ""))))
              (message "Processing headline: '%s'" heading-path)
              (when (and content 
                         (> (length (string-trim content)) 0)
                         (not (gethash content-key processed-content)))
                (message "Writing content for: %s" heading-path)
                (puthash content-key t processed-content)
                (with-current-buffer output-buffer
                  (insert 
                   (format "{\"text\": %s}\n"
                           (json-encode-string 
                            (format "%s\n\n%s"
                                    heading-path
                                    (string-trim content))))))))))))
    (with-current-buffer output-buffer
      (write-file "output.jsonl"))
    (kill-buffer output-buffer)))
