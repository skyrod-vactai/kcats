(setq org-export-use-babel nil)
(setq org-html-htmlize-output-type 'css)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-install 'htmlize)
(package-install 'rust-mode)
(require 'rust-mode)

(defface kcats-brackets 
  '((((class color)) (:foreground "DimGrey" :weight bold)))
  "kcats brackets" :group 'faces)
(defface kcats-stackop 
  '((((class color)) (:foreground "LightGreen" :inherit 'font-lock-keyword-face)))
  "kcats stack manipulation operation" :group 'faces)

(defconst kcats-font-lock-keywords
      `(("\\[\\|\\]" 0 'kcats-brackets)
        (";;.*" 0 'font-lock-comment-face)
        (,(regexp-opt '("swap" "swapdown" "drop" "dropdown" "sink" "float" "clone" "snapshot" "evert") 'words) . (0 font-lock-builtin-face))
        (,(regexp-opt '("true" "false" "nothing" "[]") 'words) . (0 font-lock-keyword-face))
        (,(regexp-opt '("first" "second" "last" "put" "take" "pop" "step" "filter"
                        "map" "count" "join" "rest" "wrap" "unwrap" "reverse") 'words) . (0 font-lock-function-name-face))
        (,(regexp-opt '("execute" "dip" "dive" "divedown" "dipdown" "shield" "shielddown" "shielddeep" "inject"
                        "loop" "while" "until" "if" "branch" "recur" "times") 'words) . '(0 font-lock-preprocessor-face))
        ("#?\"" 0 'double-quote prepend)))

(add-hook 'kcats-mode-hook (lambda () (font-lock-add-keywords nil kcats-font-lock-keywords)))

(setq kcats-mode-syntax-table (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    ;; Setting commas as whitespace makes functions like `delete-trailing-whitespace' behave unexpectedly (#561)
    (modify-syntax-entry ?, "." table)

    ;; Delimiters
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; Others
    (modify-syntax-entry ?\; "<" table) ; comment start
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table))

(require 'smie) ;; indentation engine

(define-derived-mode kcats-mode fundamental-mode "kcats"
  "major mode for editing kcats."
  (set-syntax-table kcats-mode-syntax-table)
  (setq-local comment-start ";") ;; try ";;"
  (setq-local comment-end "")

  (smie-setup nil (lambda (method arg)
                    (when (eq method :list-intro)
                      t)))
  (setq font-lock-defaults '(kcats-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.kcats\\'" . kcats-mode))

(defun org-babel-execute:kcats (body params)
  "Execute a block of kcats code with org-babel."
  (org-babel-eval
   kcats-babel-executable
   body))

(defun org-is-result-block-p (src-block)
  "Check if the given SRC-BLOCK is a results block."
  (save-excursion
    (goto-char (org-element-property :begin src-block))
    ;;(forward-line -1)
    (beginning-of-line)
    (looking-at-p "^#\\+RESULTS:")))

(defun org-html-src-block-modified (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((lang (org-element-property :language src-block))
	   (code (org-html-format-code src-block info))
	   (label (let ((lbl (org-html--reference src-block info t)))
		    (if lbl (format " id=\"%s\"" lbl) "")))
	   (klipsify  (and  (plist-get info :html-klipsify-src)
                            (member lang '("javascript" "js"
					   "ruby" "scheme" "clojure" "php" "html"))))
           (is-results (org-is-result-block-p src-block))
           (css-class (if is-results " results" "")))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format "<div class=\"org-src-container%s\">\n%s%s\n</div>"
                css-class
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (org-trim (org-export-data caption info))))))
		;; Contents.
		(if klipsify
		    (format "<pre><code class=\"src src-%s\"%s%s>%s</code></pre>"
			    lang
			    label
			    (if (string= lang "html")
				" data-editor-type=\"html\""
			      "")
			    code)
		  (format "<pre class=\"src src-%s\"%s>%s</pre>"
                          lang label code)))))))

(defun get-tangle-from-src-block (src-block)
  "Get the :tangle value from the header arguments of a SRC-BLOCK."
  (let* ((header-args (nth 2 (org-babel-get-src-block-info nil src-block)))
        (tangle (cdr (assoc :tangle header-args))))
    (message "tangle: %s %s" tangle (type-of tangle))
    (if (equal tangle "no") nil tangle)))

(defun org-html-src-block-modified-tangle (orig-fn src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML with tangle information.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  ;;(message "block at %d" (org-element-property :begin src-block))
  (let ((tangle (get-tangle-from-src-block src-block)))
    (let* ((original-result (funcall orig-fn src-block contents info)))
      (if tangle
          (concat "<div class='tangle-wrapper' data-tangle='" tangle "'>" original-result "</div>")
        original-result))))

(advice-add 'org-html-src-block :override #'org-html-src-block-modified)

;;(advice-remove 'org-html-src-block #'org-html-src-block-modified-tangle)
(advice-add 'org-html-src-block :around #'org-html-src-block-modified-tangle)

(save-excursion
  (find-file "book-of-kcats.org")
  (org-html-export-to-html))

(save-excursion
  (find-file "lexicon.org")
  (org-html-export-to-html))

(save-excursion
  (find-file "production.org")
  (org-html-export-to-html))
