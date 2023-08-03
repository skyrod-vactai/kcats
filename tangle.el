(progn
  (defun my/org-babel-tangle-single-block (oldfun block-counter &optional only-this-block)
    (let* ((info (nth 4 (org-babel-get-src-block-info)))
           (tangle-file (cdr (assq :tangle info))))
      (when tangle-file
        (let ((orig-output-dir default-directory)
              (output-dir (if (file-name-absolute-p tangle-file)
                              (file-name-directory tangle-file)
                            default-directory)))
          (unless (file-exists-p output-dir)
            (make-directory output-dir t)))))
    (funcall oldfun block-counter only-this-block))
  (advice-add 'org-babel-tangle-single-block :around #'my/org-babel-tangle-single-block))
(org-babel-tangle-file "production.org")
