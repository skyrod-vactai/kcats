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

(advice-add 'org-html-src-block :override #'org-html-src-block-modified)

(save-excursion
  (find-file "book-of-kcats.org")
  (org-html-export-to-html))

(save-excursion
  (find-file "lexicon.org")
  (org-html-export-to-html))

(save-excursion
  (find-file "production.org")
  (org-html-export-to-html))
