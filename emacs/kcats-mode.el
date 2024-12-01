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

(require 'smie) ;; indentation engine

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

(defun kcats-format-buffer ()
  "Format the current buffer according to the kcats language style."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (pcase (char-after)
        (?\[ (kcats-indent))
        (?\] (kcats-dedent))
        (_ (forward-char)))))
  (goto-char (point-min))
  (while (search-forward "[[" nil t)
    (replace-match "[  ["))
  (goto-char (point-min))
  (while (search-forward-regexp "\\(\\[\\|\\]\\)[[:space:]]+\\(\\[\\|\\]\\)" nil t)
    (replace-match "\\1\\2")))

(defun kcats-indent ()
  "Increase the indentation level by 2 spaces."
  (beginning-of-line)
  (indent-line-to (+ (current-indentation) 2)))

(defun kcats-dedent ()
  "Decrease the indentation level by 2 spaces."
  (beginning-of-line)
  (indent-line-to (max (- (current-indentation) 2) 0)))

(define-derived-mode kcats-mode fundamental-mode "kcats"
  "major mode for editing kcats."
  (set-syntax-table kcats-mode-syntax-table)
  (setq-local comment-start ";") ;; try ";;"
  (setq-local comment-end "")

  (smie-setup nil (lambda (method arg)
                    (when (eq method :list-intro)
                      t)))
  (setq font-lock-defaults '(kcats-font-lock-keywords)))

(setq kcats-abbrevs
      '(("**1" "✂️1️⃣")
        ("**2" "✂️2️⃣")
        ("**3" "✂️3️⃣")
        ("**4" "✂️4️⃣")
        ("**5" "✂️5️⃣")
        ("*1" "1️⃣")
        ("*2" "2️⃣")
        ("*3" "3️⃣")
        ("*4" "4️⃣")
        ("*5" "5️⃣")
        ("any?" "📣")
        ("branch" "↔️")
        ("clone" "👥")
        ("clonedeep" "••👥")
        ("clonedown" "•👥")
        ("decorate" "🎀")
        ("dip" "🪄")
        ("dipdeep" "••🪄")
        ("dipdown" "•🪄")
        ("dive" "🐋")
        ("divedeep" "••🐋")
        ("divedown" "•🐋")
        ("drop" "🗑️")
        ("dropdeep" "••🗑️")
        ("dropdown" "•🗑️")
        ("every?" "💯")
        ("execute" "▶️")
        ("filter" "🧲")
        ("float" "🛟")
        ("if" "⚖️")
        ("inject" "💉")
        ("join" "🔗")
        ("loop" "🌀")
        ("map" "🚜")
        ("not" "☯️")
        ("pack" "🎒")
        ("put" "📮")
        ("recovery" "🩹")
        ("recur" "🪆")
        ("shield" "🛡️")
        ("shielddeep" "••🛡️")
        ("shielddown" "•🛡️")
        ("sink" "⚓")
        ("snapshot" "📸")
        ("step" "🪜")
        ("swap" "🔀")
        ("swapdeep" "••🔀")
        ("swapdown" "•🔀")
        ("take" "📤")
        ("unwrap" "🍫")
        ("while" "⏳")
        ("wrap" "🎁")))
(define-abbrev-table 'kcats-mode-abbrev-table kcats-abbrevs)

(add-hook 'kcats-mode-hook 'abbrev-mode)
("add-to-list" 'auto-mode-alist '("\"\\\\.kcats\\\\'\"" . kcats-mode))
