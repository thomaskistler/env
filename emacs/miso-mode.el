;;; miso-mode.el --- Major mode for the Miso programming language

;; Copyright 2015 The Miso Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

(require 'cl)
(require 'ffap)
(require 'url)

;; XEmacs compatibility guidelines
;; - Minimum required version of XEmacs: 21.5.32
;;   - Feature that cannot be backported: POSIX character classes in
;;     regular expressions
;;   - Functions that could be backported but won't because 21.5.32
;;     covers them: plenty.
;;   - Features that are still partly broken:
;;     - godef will not work correctly if multibyte characters are
;;       being used
;;     - Fontification will not handle unicode correctly
;;
;; - Do not use \_< and \_> regexp delimiters directly; use
;;   miso--regexp-enclose-in-symbol
;;
;; - The character `_` must not be a symbol constituent but a
;;   character constituent
;;
;; - Do not use process-lines
;;
;; - Use miso--old-completion-list-style when using a plain list as the
;;   collection for completing-read
;;
;; - Use miso--kill-whole-line instead of kill-whole-line (called
;;   kill-entire-line in XEmacs)
;;
;; - Use miso--position-bytes instead of position-bytes
(defmacro miso--xemacs-p ()
  `(featurep 'xemacs))

(defalias 'miso--kill-whole-line
  (if (fboundp 'kill-whole-line)
      'kill-whole-line
    'kill-entire-line))

;; XEmacs unfortunately does not offer position-bytes. We can fall
;; back to just using (point), but it will be incorrect as soon as
;; multibyte characters are being used.
(if (fboundp 'position-bytes)
    (defalias 'miso--position-bytes 'position-bytes)
  (defun miso--position-bytes (point) point))

(defun miso--old-completion-list-style (list)
  (mapcar (lambda (x) (cons x nil)) list))

;; GNU Emacs 24 has prog-mode, older GNU Emacs and XEmacs do not.
;; Ideally we'd use defalias instead, but that breaks in XEmacs.
;;
;; TODO: If XEmacs decides to add prog-mode, change this to use
;; defalias to alias prog-mode or fundamental-mode to miso--prog-mode
;; and use that in define-derived-mode.
(if (not (fboundp 'prog-mode))
    (define-derived-mode prog-mode fundamental-mode "" ""))

(defun miso--regexp-enclose-in-symbol (s)
  ;; XEmacs does not support \_<, GNU Emacs does. In GNU Emacs we make
  ;; extensive use of \_< to support unicode in identifiers. Until we
  ;; come up with a better solution for XEmacs, this solution will
  ;; break fontification in XEmacs for identifiers such as "typeµ".
  ;; XEmacs will consider "type" a keyword, GNU Emacs won't.

  (if (miso--xemacs-p)
      (concat "\\<" s "\\>")
    (concat "\\_<" s "\\_>")))

(defconst miso-dangling-operators-regexp "[^-]-\\|[^+]\\+\\|[/*&><.=|^]")
(defconst miso-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst miso-label-regexp miso-identifier-regexp)
(defconst miso-type-regexp "[[:word:][:multibyte:]*]+")
(defconst miso-func-regexp (concat (miso--regexp-enclose-in-symbol "func") "\\s *\\(" miso-identifier-regexp "\\)"))
(defconst miso-func-meth-regexp (concat (miso--regexp-enclose-in-symbol "func") "\\s *\\(?:(\\s *" miso-identifier-regexp "\\s +" miso-type-regexp "\\s *)\\s *\\)?\\(" miso-identifier-regexp "\\)("))
(defconst miso-builtins
  '()
  "All built-in functions in the Miso language. Used for font locking.")

(defconst miso-mode-keywords
  '("break"    "func"   "select"  "from"  "where"  "by"  "try"  "with"
    "run"      "else"   "collection"   "if"    "continue"   "for"  "return")
  "All keywords in the Miso language.  Used for font locking.")

(defconst miso-constants '("na"  "true"  "false"))
(defconst miso-type-name-regexp (concat "\\(?:[*(]\\)*\\(?:" miso-identifier-regexp "\\.\\)?\\(" miso-identifier-regexp "\\)"))

(defvar miso-dangling-cache)
(defvar miso-misodoc-history nil)

(defgroup miso nil
  "Major mode for editing Miso code"
  :group 'languages)

(defcustom miso-fontify-function-calls t
  "Fontify function and method calls if this is non-nil."
  :type 'boolean
  :group 'miso)

(defvar miso-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?%  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)
    (modify-syntax-entry ?^  "." st)
    (modify-syntax-entry ?!  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?/ (if (miso--xemacs-p) ". 1456" ". 124b") st)
    (modify-syntax-entry ?*  ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?`  "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; It would be nicer to have _ as a symbol constituent, but that
    ;; would trip up XEmacs, which does not support the \_< anchor
    (modify-syntax-entry ?_  "w" st)

    st)
  "Syntax table for Miso mode.")

(defun miso--build-font-lock-keywords ()
  ;; we cannot use 'symbols in regexp-opt because emacs <24 doesn't
  ;; understand that
  (append
   `((,(miso--regexp-enclose-in-symbol (regexp-opt miso-mode-keywords t)) . font-lock-keyword-face)
     (,(miso--regexp-enclose-in-symbol (regexp-opt miso-builtins t)) . font-lock-builtin-face)
     (,(miso--regexp-enclose-in-symbol (regexp-opt miso-constants t)) . font-lock-constant-face)
     (,miso-func-regexp 1 font-lock-function-name-face)) ;; function (not method) name

   (if miso-fontify-function-calls
       `((,(concat "\\(" miso-identifier-regexp "\\)[[:space:]]*(") 1 font-lock-function-name-face) ;; function call/method name
         (,(concat "(\\(" miso-identifier-regexp "\\))[[:space:]]*(") 1 font-lock-function-name-face)) ;; bracketed function call
     `((,miso-func-meth-regexp 1 font-lock-function-name-face))) ;; method name

   `(
     (,(concat (miso--regexp-enclose-in-symbol "type") "[[:space:]]*\\([^[:space:]]+\\)") 1 font-lock-type-face) ;; types
     (,(concat (miso--regexp-enclose-in-symbol "type") "[[:space:]]*" miso-identifier-regexp "[[:space:]]*" miso-type-name-regexp) 1 font-lock-type-face) ;; types
     (,(concat "[^[:word:][:multibyte:]]\\[\\([[:digit:]]+\\|\\.\\.\\.\\)?\\]" miso-type-name-regexp) 2 font-lock-type-face) ;; Arrays/slices
     (,(concat "\\(" miso-identifier-regexp "\\)" "{") 1 font-lock-type-face)
     (,(concat (miso--regexp-enclose-in-symbol "map") "\\[[^]]+\\]" miso-type-name-regexp) 1 font-lock-type-face) ;; map value type
     (,(concat (miso--regexp-enclose-in-symbol "map") "\\[" miso-type-name-regexp) 1 font-lock-type-face) ;; map key type
     (,(concat (miso--regexp-enclose-in-symbol "chan") "[[:space:]]*\\(?:<-\\)?" miso-type-name-regexp) 1 font-lock-type-face) ;; channel type
     (,(concat (miso--regexp-enclose-in-symbol "\\(?:new\\|make\\)") "\\(?:[[:space:]]\\|)\\)*(" miso-type-name-regexp) 1 font-lock-type-face) ;; new/make type
     ;; TODO do we actually need this one or isn't it just a function call?
     (,(concat "\\.\\s *(" miso-type-name-regexp) 1 font-lock-type-face) ;; Type conversion
     (,(concat (miso--regexp-enclose-in-symbol "func") "[[:space:]]+(" miso-identifier-regexp "[[:space:]]+" miso-type-name-regexp ")") 1 font-lock-type-face) ;; Method receiver
     ;; Like the original miso-mode this also marks compound literal
     ;; fields. There, it was marked as to fix, but I grew quite
     ;; accustomed to it, so it'll stay for now.
     (,(concat "^[[:space:]]*\\(" miso-label-regexp "\\)[[:space:]]*:\\(\\S.\\|$\\)") 1 font-lock-constant-face) ;; Labels and compound literal fields
     (,(concat (miso--regexp-enclose-in-symbol "\\(goto\\|break\\|continue\\)") "[[:space:]]*\\(" miso-label-regexp "\\)") 2 font-lock-constant-face)))) ;; labels in goto/break/continue

(defvar miso-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "}" 'miso-mode-insert-and-indent)
    (define-key m ")" 'miso-mode-insert-and-indent)
    (define-key m "," 'miso-mode-insert-and-indent)
    (define-key m ":" 'miso-mode-insert-and-indent)
    (define-key m "=" 'miso-mode-insert-and-indent)
    (define-key m (kbd "C-c C-a") 'miso-import-add)
    m)
  "Keymap used by Miso mode to implement electric keys.")

(defun miso-mode-insert-and-indent (key)
  "Invoke the global binding of KEY, then reindent the line."

  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))

(defmacro miso-paren-level ()
  `(car (syntax-ppss)))

(defmacro miso-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro miso-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro miso-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defmacro miso-goto-beginning-of-string-or-comment ()
  `(goto-char (nth 8 (syntax-ppss))))

(defun miso--backward-irrelevant (&optional stop-at-string)
  "Skips backwards over any characters that are irrelevant for
indentation and related tasks.

It skips over whitespace, comments, cases and labels and, if
STOP-AT-STRING is not true, over strings."

  (let (pos (start-pos (point)))
    (skip-chars-backward "\n\s\t")
    (if (and (save-excursion (beginning-of-line) (miso-in-string-p)) (looking-back "`") (not stop-at-string))
        (backward-char))
    (if (and (miso-in-string-p) (not stop-at-string))
        (miso-goto-beginning-of-string-or-comment))
    (if (looking-back "\\*/")
        (backward-char))
    (if (miso-in-comment-p)
        (miso-goto-beginning-of-string-or-comment))
    (setq pos (point))
    (beginning-of-line)
    (if (or (looking-at (concat "^" miso-label-regexp ":")) (looking-at "^[[:space:]]*\\(case .+\\|default\\):"))
        (end-of-line 0)
      (goto-char pos))
    (if (/= start-pos (point))
        (miso--backward-irrelevant stop-at-string))
    (/= start-pos (point))))

(defun miso--buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (/= (buffer-size)
      (- (point-max)
         (point-min))))

(defun miso-previous-line-has-dangling-op-p ()
  "Returns non-nil if the current line is a continuation line."
  (let* ((cur-line (line-number-at-pos))
         (val (gethash cur-line miso-dangling-cache 'nope)))
    (if (or (miso--buffer-narrowed-p) (equal val 'nope))
        (save-excursion
          (beginning-of-line)
          (miso--backward-irrelevant t)
          (setq val (looking-back miso-dangling-operators-regexp))
          (if (not (miso--buffer-narrowed-p))
              (puthash cur-line val miso-dangling-cache))))
    val))

(defun miso--at-function-definition ()
  "Return non-nil if point is on the opening curly brace of a
function definition.

We do this by first calling (beginning-of-defun), which will take
us to the start of *some* function. We then look for the opening
curly brace of that function and compare its position against the
curly brace we are checking. If they match, we return non-nil."
  (if (= (char-after) ?\{)
      (save-excursion
        (let ((old-point (point))
              start-nesting)
          (beginning-of-defun)
          (when (looking-at "func ")
            (setq start-nesting (miso-paren-level))
            (skip-chars-forward "^{")
            (while (> (miso-paren-level) start-nesting)
              (forward-char)
              (skip-chars-forward "^{") 0)
            (if (and (= (miso-paren-level) start-nesting) (= old-point (point)))
                t))))))

(defun miso-goto-opening-parenthesis (&optional char)
  (let ((start-nesting (miso-paren-level)))
    (while (and (not (bobp))
                (>= (miso-paren-level) start-nesting))
      (if (zerop (skip-chars-backward
                  (if char
                      (case char (?\] "^[") (?\} "^{") (?\) "^("))
                    "^[{(")))
          (if (miso-in-string-or-comment-p)
              (miso-goto-beginning-of-string-or-comment)
            (backward-char))))))

(defun miso--indentation-for-opening-parenthesis ()
  "Return the semantic indentation for the current opening parenthesis.

If point is on an opening curly brace and said curly brace
belongs to a function declaration, the indentation of the func
keyword will be returned. Otherwise the indentation of the
current line will be returned."
  (save-excursion
    (if (miso--at-function-definition)
        (progn
          (beginning-of-defun)
          (current-indentation))
      (current-indentation))))

(defun miso-indentation-at-point ()
  (save-excursion
    (let (start-nesting (outindent 0))
      (back-to-indentation)
      (setq start-nesting (miso-paren-level))

      (cond
       ((miso-in-string-p)
        (current-indentation))
       ((looking-at "[])}]")
        (miso-goto-opening-parenthesis (char-after))
        (if (miso-previous-line-has-dangling-op-p)
            (- (current-indentation) tab-width)
          (miso--indentation-for-opening-parenthesis)))
       ((progn (miso--backward-irrelevant t) (looking-back miso-dangling-operators-regexp))
        ;; only one nesting for all dangling operators in one operation
        (if (miso-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (current-indentation) tab-width)))
       ((zerop (miso-paren-level))
        0)
       ((progn (miso-goto-opening-parenthesis) (< (miso-paren-level) start-nesting))
        (if (miso-previous-line-has-dangling-op-p)
            (current-indentation)
          (+ (miso--indentation-for-opening-parenthesis) tab-width)))
       (t
        (current-indentation))))))

(defun miso-mode-indent-line ()
  (interactive)
  (let (indent
        shift-amt
        end
        (pos (- (point-max) (point)))
        (point (point))
        (beg (line-beginning-position)))
    (back-to-indentation)
    (if (miso-in-string-or-comment-p)
        (misoto-char point)
      (setq indent (miso-indentation-at-point))
      (if (looking-at (concat miso-label-regexp ":\\([[:space:]]*/.+\\)?$\\|case .+:\\|default:"))
          (decf indent tab-width))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))))

(defun miso-beginning-of-defun (&optional count)
  (unless count (setq count 1))
  (let ((first t) failure)
    (dotimes (i (abs count))
      (while (and (not failure)
                  (or first (miso-in-string-or-comment-p)))
        (if (>= count 0)
            (progn
              (miso--backward-irrelevant)
              (if (not (re-search-backward miso-func-meth-regexp nil t))
                  (setq failure t)))
          (if (looking-at miso-func-meth-regexp)
              (forward-char))
          (if (not (re-search-forward miso-func-meth-regexp nil t))
              (setq failure t)))
        (setq first nil)))
    (if (< count 0)
        (beginning-of-line))
    (not failure)))

(defun miso-end-of-defun ()
  (let (orig-level)
    ;; It can happen that we're not placed before a function by emacs
    (if (not (looking-at "func"))
        (miso-beginning-of-defun -1))
    (skip-chars-forward "^{")
    (forward-char)
    (setq orig-level (miso-paren-level))
    (while (>= (miso-paren-level) orig-level)
      (skip-chars-forward "^}")
      (forward-char))))

;;;###autoload
(define-derived-mode miso-mode prog-mode "Miso"
  "Major mode for editing Miso source text.

This mode provides (not just) basic editing capabilities for
working with Miso code. It offers almost complete syntax
highlighting and indentation."

  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(miso--build-font-lock-keywords))

  ;; Indentation
  (set (make-local-variable 'indent-line-function) 'miso-mode-indent-line)

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")

  (set (make-local-variable 'beginning-of-defun-function) 'miso-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) 'miso-end-of-defun)

  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (if (boundp 'syntax-propertize-function)
      (set (make-local-variable 'syntax-propertize-function) 'miso-propertize-syntax))

  (set (make-local-variable 'miso-dangling-cache) (make-hash-table :test 'eql))
  (add-hook 'before-change-functions (lambda (x y) (setq miso-dangling-cache (make-hash-table :test 'eql))) t t)


  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; Miso style
  (setq indent-tabs-mode nil)
  (setq default-tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; Handle unit test failure output in compilation-mode
  ;;
  ;; Note the final t argument to add-to-list for append, ie put these at the
  ;; *ends* of compilation-error-regexp-alist[-alist]. We want miso-test to be
  ;; handled first, otherwise other elements will match that don't work, and
  ;; those alists are traversed in *reverse* order:
  ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2001-12/msg00674.html
  (when (and (boundp 'compilation-error-regexp-alist)
             (boundp 'compilation-error-regexp-alist-alist))
    (add-to-list 'compilation-error-regexp-alist 'miso-test t)
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(miso-test . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.miso\\'" 'miso-mode))

(defun miso-propertize-syntax (start end)
  (save-excursion
    (goto-char start)
    (while (search-forward "\\" end t)
      (put-text-property (1- (point)) (point) 'syntax-table (if (= (char-after) ?`) '(1) '(9))))))

(provide 'miso-mode)
