;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.6
;; Package-Requires: ((emacs "29.1") (porg "0.1.0") (rainbow-delimiters "2.1.3"))
;; Keywords: lisp, languages
;; URL: https://github.com/laluxx/monad-mode

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Monad is a Lisp-like language with Scheme-like syntax and an advanced
;; type system.  This package provides a major mode for editing Monad source
;; files (*.mon).
;;
;; Features:
;;
;; Syntax
;;   - Syntax highlighting for keywords, strings, characters, and comments
;;   - Docstring recognition for functions, variables, and lambdas
;;     (both plain string and :doc keyword styles)
;;   - Underscore (_) highlighted as a shadow face for pattern matching wildcards
;;   - Optional -> arrow highlighting (see `monad-highlight-arrow')
;;
;; Indentation
;;   - Scheme-compatible indentation via `scheme-indent-function'
;;   - Dedicated indentation for (asm ...) blocks
;;
;; Assembly
;;   - Full syntax highlighting inside (asm ...) forms: instructions,
;;     registers, labels, directives, and numeric literals
;;
;; Eldoc
;;   - Hover a function name to see its full type signature
;;   - Hover a variable to see its type annotation and value
;;   - When filling a call, the current parameter is highlighted and
;;     all others are dimmed, matching elisp-mode's parameter tracking
;;   - All signatures are syntax-colored to match the buffer
;;
;; Navigation and cross-references
;;   - Xref backend supporting jump-to-definition for functions, variables,
;;     typed parameters, and module qualified/unqualified symbols
;;   - Cross-module xref: definitions are found in imported .mon files
;;   - Imenu index with aligned type signatures and docstring previews
;;
;; Completion
;;   - Completion-at-point for keywords, local definitions, and imports
;;   - Import-aware: respects qualified, :as, hiding, and explicit export lists
;;   - Distinct completion kinds for functions, variables, keywords, and
;;     asm instructions (with nerd-icons-corfu support)
;;
;; Miscellaneous
;;   - rainbow-delimiters enabled by default; eldoc signatures use matching
;;     delimiter colors to stay consistent with the buffer appearance
;;   - Electric backtick pairing via `electric-pair-mode'
;;   - `C-c C-d' shows the full docstring of the symbol at point

;;; TODO [0/10]
;; - [ ] if there is [any|thing] on RET indent the guard to be aligned with the |
;; - [ ] an option to indent the -> even before the guards at the top
;; - [ ] Make `mark-sep' work with wisp too
;; - [ ] a function to automatically insert the (module Name [...])
;; - [ ] t to monad-type-infer-region
;; - [ ] i to monad-parinfer-region
;; - [ ] smart [] also for function parameters
;; - [ ] Completion should work with wisp too
;; - [ ] we should highlight `color' with the `font-lock-variable-name-face' `'define color Color 33 33 33 0
;; - [ ] Highlight `tmp' with `font-lock-variable-name-face' (let [tmp (arr i)] ...)
;; - [ ] "include <" should complete with C libraries
;; - [ ] If There is a repl we could also know the type of stuff
;;       in eldoc in monad-mode.


;;; Code:

(require 'lisp-mode)
(require 'cl-lib)
(require 'eldoc)
(require 'xref)
(require 'porg nil t)
(require 'rainbow-delimiters)
(require 'monad-repl nil t)


(defgroup monad nil
  "Major mode for editing Monad code."
  :prefix "monad-"
  :group 'languages)

(defcustom monad-highlight-arrow t
  "If non-nil, highlight Monad result arrows with `monad-arrow-face'."
  :type 'boolean
  :group 'monad)

(defcustom monad-highlight-path-literals t
  "If non-nil, highlight Monad path literals."
  :type 'boolean
  :group 'monad)

(defcustom monad-highlight-escape-literals t
  "If non-nil, highlight Monad escape literals."
  :type 'boolean
  :group 'monad)

(defcustom monad-highlight-layout-field-docstrings t
  "If non-nil, highlight unquoted layout field docstrings."
  :type 'boolean
  :group 'monad)

(defface monad-path-literal-face
  '((t :inherit font-lock-constant-face))
  "Face for Monad path literals."
  :group 'monad)

(defface monad-escape-literal-face
  '((t :inherit font-lock-string-face))
  "Face for Monad escape literals."
  :group 'monad)

(defface monad-underscore-face
  '((t :inherit shadow))
  "Face for underscore wildcard pattern."
  :group 'monad)

(defface monad-doc-code-face
  '((t :inherit font-lock-variable-name-face))
  "Face for infix backtick expressions like `fun`."
  :group 'monad)

(defface monad-guard-rail-face
  '((t :inherit shadow))
  "Face for Monad Unicode guard rail connectors."
  :group 'monad)

(defface monad-arrow-face
  '((t :inherit shadow))
  "Face for Monad result arrows."
  :group 'monad)

(defface monad-type-arrow-face
  '((t :inherit default))
  "Face for Monad type signature arrows."
  :group 'monad)

(defvar monad-mode-syntax-table
  (let ((st (make-syntax-table))
        (i 0))
    ;; Symbol constituents (like scheme-mode)
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)
    ;; Brackets and braces
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?\{ "(}  " st)
    (modify-syntax-entry ?\} "){  " st)
    ;; Parentheses
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; Comments
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\| ". 23" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"   " st)
    ;; Character quote
    (modify-syntax-entry ?' "'   " st)
    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st)
  "Syntax table for `monad-mode'.")

(defvar monad-mode-abbrev-table nil)
(define-abbrev-table 'monad-mode-abbrev-table ())

(defconst monad-keywords
  '("define" "infer" "method" "variable" "def" "lambda" "match" "with" "layout" "type" "data" "deriving"
    "let" "letrec" "let*" "cond" "case" "if" "then" "else"
    "and" "or" "not" "quote" "unquote" "quasiquote"
    "begin" "when" "unless" "error" "instance" "asm"
    "module" "import" "qualified" "hiding" "tests" "test"
    "take" "drop" "include" "for" "in" "while" "mod" "class"
    "where" "show" "set!" "set" "otherwise")
  "Keywords for the Monad programming language.")

(defconst monad--identifier-regexp "\\(?:\\sw\\|\\s_\\)+"
  "Regexp matching a Monad identifier using `monad-mode-syntax-table'.")

(defun monad--define-name-regexp (&optional name)
  "Return a regexp matching a Lisp or Haskell-style define for NAME.
When NAME is nil, the returned regexp captures the definition name in
group 1.  Supported forms include `(define (name ...)',
`(define [name :: Type] ...)', `(define name ...)', and
`define name :: Type'.  `method' is accepted in place of `define'."
  (concat "^\\s-*\\(?:([ \t]*\\)?\\(?:define\\|method\\)\\s-+\\(?:([ \t\n]*\\|\\[?\\)"
          "\\(" (or (and name (regexp-quote name))
                    monad--identifier-regexp)
          "\\)\\_>"))

(defun monad--type-name-regexp (&optional name)
  "Return a regexp matching a refinement type definition for NAME.
When NAME is nil, the returned regexp captures the type name in group 1."
  (concat "^\\(?:([ \t]*\\)?type\\s-+"
          "\\(" (or (and name (regexp-quote name))
                    monad--identifier-regexp)
          "\\)\\_>"))

(defun monad--layout-name-regexp (&optional name)
  "Return a regexp matching a layout definition for NAME.
When NAME is nil, the returned regexp captures the layout name in group 1."
  (concat "^\\(?:([ \t]*\\)?layout\\s-+"
          "\\(" (or (and name (regexp-quote name))
                    monad--identifier-regexp)
          "\\)\\_>"))

(defun monad--capitalize-initial-at (pos)
  "Uppercase the character at POS, preserving the rest of the name."
  (let ((char (char-after pos)))
    (when (and char (not (= char (upcase char))))
      (save-excursion
        (goto-char pos)
        (delete-char 1)
        (insert-char (upcase char))))))

(defun monad--definition-name-regexps (&optional name)
  "Return regexps matching top-level definition forms for NAME."
  (list (monad--define-name-regexp name)
        (monad--type-name-regexp name)
        (monad--layout-name-regexp name)))

(defun monad--haskell-define-signature (name)
  "Return the Haskell-style signature line for define NAME, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((rx (concat "^\\s-*\\(?:define\\|method\\)\\s-+\\("
                      (regexp-quote name)
                      "\\)\\_>\\s-+::\\s-*\\(.+\\)$")))
      (when (re-search-forward rx nil t)
        (concat ":: " (string-trim (match-string-no-properties 2)))))))

(defun monad--haskell-define-at-point-p ()
  "Return non-nil when point is at a Haskell-style `define name ::' form."
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at
     (concat "^\\s-*\\(?:define\\|method\\)\\s-+"
             monad--identifier-regexp "\\_>\\s-+::"))))

;;; Imenu — flat index with cached docstring annotations

(defvar-local monad--docstring-cache nil
  "Cache for definition docstrings.
Hash table mapping name strings to the first line of their docstring,
or nil when there is none.")

(defun monad--invalidate-docstring-cache ()
  "Clear buffer-local docstring, Commentary, and alignment caches."
  (setq monad--docstring-cache nil
        monad--commentary-section-cache nil
        monad--imenu-max-name-len 0
        monad--imenu-max-type-len 0))

(defun monad--docstring-first-line (raw)
  "Return the first non-blank line of RAW, trimmed, max 80 chars.
Returns nil when RAW has no non-blank content."
  (when raw
    (let* ((lines (split-string raw "\n"))
           (first (cl-find-if (lambda (l) (not (string-blank-p l))) lines)))
      (when first
        (let ((s (string-trim first)))
          (if (> (length s) 80) (concat (substring s 0 77) "...") s))))))

(defun monad--read-docstring-at-point ()
  "Read the docstring of the definition whose header starts at point.
Point must be at the opening `(' of a `(define ...)' form.
Returns the first line of the docstring, or nil."
  (condition-case nil
      (progn
        (down-list 1)             ; enter (define ...)
        (forward-sexp 1)          ; skip "define"
        (skip-chars-forward " \t\n")
        (let ((is-function (eq (char-after) ?\()))
          (forward-sexp 1)        ; skip header: (name ...) | [name :: T] | name
          (unless is-function
            (skip-chars-forward " \t\n")
            (condition-case nil (forward-sexp 1) (error nil))))
        (let (doc done)
          (while (not done)
            (skip-chars-forward " \t\n")
            (cond
             ((looking-at ":doc[ \t\n]+\"")
              (goto-char (match-end 0))
              (let ((s (1- (point))))
                (goto-char s)
                (forward-sexp 1)
                (setq doc  (monad--docstring-first-line
                            (buffer-substring-no-properties (1+ s) (1- (point))))
                      done t)))
             ((looking-at ":\\(?:\\sw\\|\\s_\\)+")
              (forward-sexp 1)
              (skip-chars-forward " \t\n")
              (condition-case nil (forward-sexp 1) (error (setq done t))))
             ((looking-at "\"")
              (let ((s (point)))
                (forward-sexp 1)
                (setq doc  (monad--docstring-first-line
                            (buffer-substring-no-properties (1+ s) (1- (point))))
                      done t)))
             (t (setq done t))))
          (and doc (not (string-empty-p doc)) doc)))
    (error nil)))

(defun monad--cache-docstrings ()
  "Scan the buffer and build a fresh docstring cache."
  (let ((cache (make-hash-table :test #'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (monad--define-name-regexp) nil t)
        (let* ((name (match-string-no-properties 1))
               (def-start (match-beginning 0))
               (doc  (save-excursion
                       (goto-char def-start)
                       (monad--read-docstring-at-point))))
          (puthash name doc cache))))
    cache))

(defun monad--get-cached-docstring (name)
  "Return the cached docstring for NAME, rebuilding the cache if needed."
  (unless monad--docstring-cache
    (setq monad--docstring-cache (monad--cache-docstrings)))
  (gethash name monad--docstring-cache))

(defvar-local monad--imenu-max-name-len 0)
(defvar-local monad--imenu-max-type-len 0)

(defun monad--imenu-build-index ()
  "Build a flat imenu index for Monad mode."
  (unless monad--docstring-cache
    (setq monad--docstring-cache (monad--cache-docstrings)))
  (let (index (max-name 0) (max-type 0))
    (dolist (rx (monad--definition-name-regexps))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rx nil t)
          (let* ((name (match-string-no-properties 1))
                 (display-name (if (monad--haskell-define-at-point-p)
                                   (propertize name 'face 'font-lock-function-name-face)
                                 name))
                 (type (monad--imenu-type-annotation name))
                 (tlen (if type (length type) 0)))
            (when (> (length name) max-name) (setq max-name (length name)))
            (when (> tlen max-type)          (setq max-type tlen))
            (push (cons display-name (copy-marker (match-beginning 1))) index)))))
    (setq monad--imenu-max-name-len max-name
          monad--imenu-max-type-len max-type)
    (sort index (lambda (a b)
                  (< (marker-position (cdr a))
                     (marker-position (cdr b)))))))

(defun monad--imenu-type-annotation (name)
  "Return a raw (unpropertized) type/signature string for NAME, or nil."
  (or
   (monad--haskell-define-signature name)
   (save-excursion
     (goto-char (point-min))
     (let ((fn-rx (concat "^(define[ \t\n]+(\\("
                          (regexp-quote name) "\\)\\b")))
       (if (re-search-forward fn-rx nil t)
           (condition-case nil
               (progn
                 (goto-char (match-beginning 0))
                 (down-list 1)
                 (forward-sexp 1)
                 (skip-chars-forward " \t\n")
                 (let ((hdr-start (point)))
                   (forward-sexp 1)
                   (buffer-substring-no-properties hdr-start (point))))
             (error nil))
         (goto-char (point-min))
         (let ((tv-rx (concat "^(define[ \t\n]+\\(\\["
                              (regexp-quote name)
                              "[ \t]*::[^]\n]+\\]\\)")))
           (when (re-search-forward tv-rx nil t)
             (match-string-no-properties 1))))))))

(defun monad-imenu-annotate (cand)
  "Return an imenu annotation string for candidate CAND."
  (let* ((name      (substring-no-properties cand))
         (type-raw  (monad--imenu-type-annotation name))
         (doc       (monad--get-cached-docstring name))
         (col-a     (+ monad--imenu-max-name-len 2))
         (col-b     (+ col-a monad--imenu-max-type-len 2)))
    (when (or type-raw doc)
      (concat
       (propertize " " 'display `(space :align-to ,col-a))
       (when type-raw
         (monad--propertize-signature type-raw))
       (when doc
         (propertize " " 'display `(space :align-to ,col-b)))
       (when doc
         (propertize doc 'face 'font-lock-doc-face))))))

(defun monad-char-literal-matcher (limit)
  "Match character literals like \\='a\\=' or \\='\\\\n\\=' up to LIMIT."
  (catch 'found
    (while (re-search-forward "'\\(\\\\.[^']*\\|.\\)'" limit t)
      (throw 'found t))
    nil))

(defconst monad--commentary-heading-regexp
  "^[ \t]*;;;[ \t]*Commentary:[^\n]*"
  "Regexp matching the top-level Commentary heading.")

(defconst monad--code-heading-regexp
  "^[ \t]*;;;[ \t]*Code:[^\n]*"
  "Regexp matching the top-level Code heading.")

(defvar-local monad--commentary-section-cache nil
  "Cached Commentary body bounds.
The value is (TICK . BOUNDS), where BOUNDS is (START . END) or nil.")

(defun monad--commentary-section-cache-bounds ()
  "Return cached raw Commentary body bounds, or nil."
  (let ((tick (buffer-chars-modified-tick)))
    (if (and monad--commentary-section-cache
             (= (car monad--commentary-section-cache) tick))
        (cdr monad--commentary-section-cache)
      (let (bounds)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward monad--commentary-heading-regexp nil t)
            (forward-line 1)
            (let ((body-start (point))
                  (body-end (if (re-search-forward monad--code-heading-regexp nil t)
                                (match-beginning 0)
                              (point-max))))
              (setq bounds (cons body-start body-end)))))
        (setq monad--commentary-section-cache (cons tick bounds))
        bounds))))

(defun monad--commentary-section-boundaries (&optional pos)
  "Return the raw Commentary body bounds around POS, or nil.
The body starts after `;;; Commentary:' and ends before `;;; Code:'.
The Commentary heading itself is left to normal comment syntax."
  (let* ((here (or pos (point)))
         (bounds (monad--commentary-section-cache-bounds)))
    (when (and bounds
               (>= here (car bounds))
               (< here (cdr bounds)))
      bounds)))

(defun monad-commentary-section-matcher (limit)
  "Match only the raw top-level Commentary body up to LIMIT.
The `;;; Commentary:' heading is not part of this match."
  (let (found)
    (while (and (not found)
                (< (point) limit))
      (let ((bounds (monad--commentary-section-boundaries (point))))
        (cond
         (bounds
          (let ((start (max (point) (car bounds)))
                (end (min limit (cdr bounds))))
            (if (< start end)
                (progn
                  (put-text-property start end 'font-lock-multiline t)
                  (set-match-data (list start end start end))
                  (goto-char end)
                  (setq found t))
              (goto-char limit))))
         ((re-search-forward monad--commentary-heading-regexp limit t)
          (forward-line 1))
         (t
          (goto-char limit)))))
    found))

(defun monad--font-lock-code-position-p (pos)
  "Return non-nil when POS is not inside a string, comment, or Commentary block."
  (let ((state (syntax-ppss pos)))
    (and (not (or (nth 3 state)
                  (nth 4 state)))
         (not (monad--commentary-section-boundaries pos)))))

(defconst monad-path-literal-regexp
  "\\(?:\\`\\|[[:space:]([{]\\)\\(\\(?:~/?\\|\\.\\.?/\\|/\\)[^][(){}\"'\n\t ]+\\)"
  "Regexp matching Monad path literals.
Group 1 is the actual path literal.")

(defconst monad-escape-literal-regexp
  "\\(?:\\`\\|[[:space:]([{]\\)\\(\\\\\\(?:e\\|E\\|x[[:xdigit:]][[:xdigit:]]\\|[0-7][0-7]?[0-7]?\\)[^][(){}\"'\n\t ]*\\)"
  "Regexp matching Monad escape literals.
Group 1 is the actual escape literal.")

(defun monad-path-literal-matcher (limit)
  "Match Monad path literals up to LIMIT."
  (when monad-highlight-path-literals
    (catch 'found
      (while (re-search-forward monad-path-literal-regexp limit t)
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (when (and beg end
                     (monad--font-lock-code-position-p beg))
            (set-match-data (list beg end beg end))
            (throw 'found t))))
      nil)))

(defun monad-escape-literal-matcher (limit)
  "Match Monad escape literals up to LIMIT."
  (when monad-highlight-escape-literals
    (catch 'found
      (while (re-search-forward monad-escape-literal-regexp limit t)
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (when (and beg end
                     (monad--font-lock-code-position-p beg))
            (set-match-data (list beg end beg end))
            (throw 'found t))))
      nil)))

(defconst monad--rx-rail-hline
  (regexp-quote (string ?\u2500)))

(defconst monad--rx-rail-open-left
  (regexp-quote (string ?\u256D)))

(defconst monad--rx-rail-close-left
  (regexp-quote (string ?\u2570)))

(defconst monad--rx-rail-open-right
  (regexp-quote (string ?\u256E)))

(defconst monad--rx-rail-close-right
  (regexp-quote (string ?\u256F)))

(defconst monad--rx-rail-tee
  (regexp-quote (string ?\u252C)))

(defconst monad--rx-rail-branch
  (regexp-quote (string ?\u251C)))

(defconst monad--rx-result-triangle
  (regexp-quote (string ?\u25B6)))

(defconst monad--rx-result-arrow
  (concat "\\(?:->\\|" monad--rx-result-triangle "\\)"))

(defconst monad--rx-rail-line-prefix
  "\\(?:\\`\\|[ \t]\\)")

(defconst monad--rx-guard-rail-open-left
  (concat monad--rx-rail-open-left monad--rx-rail-hline "+"))

(defconst monad--rx-guard-rail-open-top
  (concat monad--rx-rail-tee monad--rx-rail-hline "+"))

(defconst monad--rx-guard-rail-open-right
  (concat monad--rx-rail-hline "+" monad--rx-rail-open-right))

(defconst monad--rx-guard-rail-branch
  (concat monad--rx-rail-branch monad--rx-rail-hline "+"))

(defconst monad--rx-guard-rail-fallback
  (concat monad--rx-rail-close-left monad--rx-rail-hline "*" monad--rx-result-arrow))

(defconst monad--rx-guard-rail
  (concat "\\(?:"
          monad--rx-guard-rail-open-left
          "\\|" monad--rx-guard-rail-open-top
          "\\|" monad--rx-guard-rail-open-right
          "\\|" monad--rx-guard-rail-branch
          "\\|" monad--rx-guard-rail-fallback
          "\\)"))

(defconst monad--rx-box-comment
  (concat "\\(?:"
          "\\(" monad--rx-rail-open-left monad--rx-rail-hline
          "\\|" monad--rx-rail-close-left monad--rx-rail-hline
          "\\)[ \t]*\\(.*\\)$"
          "\\|"
          "^\\(.*?\\)\\([ \t]*\\(?:"
          monad--rx-rail-open-right
          "\\|" monad--rx-rail-close-right
          "\\)\\)\\(?:\\s-\\|$\\)"
          "\\)"))

(defun monad--font-lock-match-regexp (limit regexp predicate &optional group)
  "Match REGEXP up to LIMIT when PREDICATE accepts the match.
GROUP defaults to 0.  PREDICATE receives BEG and END."
  (let ((group (or group 0))
        found)
    (while (and (not found)
                (re-search-forward regexp limit t))
      (let ((beg (match-beginning group))
            (end (match-end group))
            (next (match-end 0)))
        (if (and beg end
                 (or (not predicate)
                     (funcall predicate beg end)))
            (progn
              (set-match-data (list beg end beg end))
              (goto-char end)
              (setq found t))
          (goto-char next))))
    found))

(defun monad--guard-rail-line-p (&optional pos)
  "Return non-nil when POS is on a Unicode guard rail line."
  (save-excursion
    (when pos
      (goto-char pos))
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (or
       (string-match-p
        (concat monad--rx-rail-line-prefix
                "\\(?:"
                monad--rx-guard-rail-open-left
                "\\|"
                monad--rx-guard-rail-open-top
                "\\)[ \t]+.*"
                monad--rx-result-arrow)
        line)
       (string-match-p
        (concat monad--rx-rail-line-prefix
                monad--rx-guard-rail-open-right
                "[ \t]*\\'")
        line)
       (string-match-p
        (concat monad--rx-rail-line-prefix
                monad--rx-guard-rail-open-right
                "[ \t]+.*"
                monad--rx-result-arrow)
        line)
       (string-match-p
        (concat monad--rx-rail-line-prefix
                monad--rx-guard-rail-branch
                "[ \t]+.*"
                monad--rx-result-arrow)
        line)
       (string-match-p
        (concat monad--rx-rail-line-prefix
                monad--rx-guard-rail-fallback
                "[ \t]+")
        line)))))

(defun monad-guard-rail-matcher (limit)
  "Match Unicode guard rail connectors up to LIMIT."
  (monad--font-lock-match-regexp
   limit
   monad--rx-guard-rail
   (lambda (beg _end)
     (and (monad--guard-rail-line-p beg)
          (monad--font-lock-code-position-p beg)))))

(defun monad-box-comment-matcher (limit)
  "Match decorative box comments, but never Monad guard rails."
  (monad--font-lock-match-regexp
   limit
   monad--rx-box-comment
   (lambda (beg _end)
     (and (not (monad--guard-rail-line-p beg))
          (monad--font-lock-code-position-p beg)))))

(defun monad--type-arrow-position-p (pos)
  "Return non-nil when POS is on a type signature arrow."
  (save-excursion
    (goto-char pos)
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (or
       (save-excursion
         (goto-char line-beg)
         (re-search-forward
          (concat "^[ \t]*\\(?:define\\|method\\)\\s-+"
                  monad--identifier-regexp
                  "\\s-+::")
          pos t))
       (save-excursion
         (goto-char line-beg)
         (and (looking-at "^[ \t]*::")
              (>= pos (match-end 0))))
       (let ((open (save-excursion
                     (search-backward "[" line-beg t)))
             (close (save-excursion
                      (search-forward "]" line-end t))))
         (and open close
              (< open pos)
              (< pos close)
              (save-excursion
                (goto-char open)
                (search-forward "::" pos t))))))))

(defun monad-type-arrow-matcher (limit)
  "Match Monad type signature arrows up to LIMIT."
  (when monad-highlight-arrow
    (monad--font-lock-match-regexp
     limit
     "->"
     (lambda (beg _end)
       (and (monad--font-lock-code-position-p beg)
            (monad--type-arrow-position-p beg))))))

(defun monad-arrow-matcher (limit)
  "Match Monad result arrows up to LIMIT."
  (when monad-highlight-arrow
    (monad--font-lock-match-regexp
     limit
     monad--rx-result-arrow
     (lambda (beg _end)
       (and (monad--font-lock-code-position-p beg)
            (not (monad--type-arrow-position-p beg)))))))

(defun monad-error-string-matcher (limit)
  "Match an error payload after `error'.
Quoted and bare payloads split PREFIX: from the message when a colon is
present.  The prefix uses the default face and the message uses the
error face."
  (let (found)
    (while (and (not found)
                (re-search-forward "\\_<error\\_>" limit t))
      (let ((line-end (min (line-end-position) limit))
            payload-start
            payload-end
            string-start
            string-end
            prefix-start
            prefix-end
            message-start
            message-end)
        (save-excursion
          (skip-chars-forward " \t" line-end)
          (setq payload-start (point)
                payload-end line-end)
          (cond
           ((eq (char-after) ?\")
            (setq string-start (point))
            (condition-case nil
                (progn
                  (forward-sexp 1)
                  (when (<= (point) line-end)
                    (setq string-end (point))))
              (error nil))
            (when string-end
              (goto-char (1+ string-start))
              (if (search-forward ":" (1- string-end) t)
                  (setq prefix-start string-start
                        prefix-end (point)
                        message-start (point)
                        message-end string-end)
                (setq prefix-start string-start
                      prefix-end string-start
                      message-start string-start
                      message-end string-end))))
           ((< payload-start payload-end)
            (setq string-start payload-start
                  string-end payload-end)
            (goto-char payload-start)
            (if (search-forward ":" payload-end t)
                (setq prefix-start payload-start
                      prefix-end (point)
                      message-start (point)
                      message-end payload-end)
              (setq prefix-start payload-start
                    prefix-end payload-start
                    message-start payload-start
                    message-end payload-end)))))
        (if (and string-start
                 string-end
                 prefix-start
                 prefix-end
                 message-start
                 message-end
                 (< message-start message-end))
            (progn
              (set-match-data
               (list string-start string-end
                     prefix-start prefix-end
                     message-start message-end))
              (goto-char string-end)
              (setq found t))
          (goto-char line-end))))
    found))

(defun monad-error-delimited-text-matcher (limit)
  "Match bracketed text inside an error payload up to LIMIT.
This only applies to text to the right of `error' on the same line, so
normal Monad code inside (), [], and {} keeps its usual highlighting."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 "\\(?:{[^{}\n]*}\\|\\[[^][\n]*\\]\\|([^()\n]*)\\)"
                 limit t))
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        (when (save-excursion
                (goto-char (line-beginning-position))
                (re-search-forward "\\_<error\\_>" match-start t))
          (set-match-data
           (list match-start match-end
                 match-start match-end))
          (goto-char match-end)
          (setq found t))))
    found))

(defun monad-test-string-matcher (limit)
  "Match the final string label on an indented assert line."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 "^[ \t]+assert-\\(?:\\sw\\|\\s_\\)+\\_>"
                 limit t))
      (let ((line-end (min (line-end-position) limit))
            match-start
            match-end)
        (save-excursion
          (while (re-search-forward
                  "\"\\(?:[^\"\\]\\|\\\\.\\)*\""
                  line-end t)
            (setq match-start (match-beginning 0)
                  match-end (match-end 0))))
        (if (and match-start match-end (< match-start match-end))
            (progn
              (set-match-data
               (list match-start match-end match-start match-end))
              (goto-char match-end)
              (setq found t))
          (goto-char line-end))))
    found))

;;; Infix backtick support

(defconst monad-infix-regexp "`\\([^`\n\t ]+\\)`"
  "Regexp matching infix backtick expressions.
Group 1 matches only non-whitespace characters so that consecutive
backtick pairs like `+` 22 `*` do not bleed into each other.")

(defun monad-infix-matcher (limit)
  "Font-lock matcher for infix backtick expressions up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward monad-infix-regexp limit t))
      (unless (nth 3 (syntax-ppss (match-beginning 0)))
        (when monad-infix-hide-backticks
          (put-text-property (match-beginning 0) (1+ (match-beginning 0)) 'invisible t)
          (put-text-property (1- (match-end 0)) (match-end 0)            'invisible t))
        (setq found t)))
    found))

(defvar-local monad-infix--idle-timer nil)
(defvar-local monad-infix--updating nil)

(defun monad-infix--refresh-visibility ()
  "Refresh infix backtick visibility in a 6-line window around point.
Only removes and re-applies text properties tagged with `monad-infix-invisible',
leaving any `invisible' properties set by other modes (e.g. `porg-mode') intact."
  (when (and (derived-mode-p 'monad-mode)
             (not monad-infix--updating))
    (let ((monad-infix--updating t)
          (scan-start (save-excursion (forward-line -3) (point)))
          (scan-end   (save-excursion (forward-line  3) (point))))
      (with-silent-modifications
        ;; Only remove invisible props that WE set (tagged with monad-infix)

        (let ((pos scan-start))
          (while (< pos scan-end)
            (let ((next (next-single-property-change pos 'monad-infix-invisible nil scan-end)))
              (when (get-text-property pos 'monad-infix-invisible)
                (remove-text-properties pos next '(invisible nil monad-infix-invisible nil)))
              (setq pos next))))

        ;; Re-apply with our tag
        (save-excursion
          (goto-char scan-start)
          (while (re-search-forward monad-infix-regexp scan-end t)
            (let ((mstart (match-beginning 0))
                  (mend   (match-end 0)))
              (when monad-infix-hide-backticks
                (put-text-property mstart (1+ mstart) 'invisible t)
                (put-text-property mstart (1+ mstart) 'monad-infix-invisible t)
                (put-text-property (1- mend) mend 'invisible t)
                (put-text-property (1- mend) mend 'monad-infix-invisible t)))))))))

(defun monad-infix-schedule-refresh ()
  "Schedule a backtick visibility refresh via idle timer."
  (when (derived-mode-p 'monad-mode)
    (when (timerp monad-infix--idle-timer)
      (cancel-timer monad-infix--idle-timer))
    (setq monad-infix--idle-timer
          (run-with-idle-timer 0.05 nil #'monad-infix--refresh-visibility))))

;;; Electric pair for backtick

(defun monad--setup-electric-pair ()
  "Setup electric pairing of backticks in `monad-mode'."
  (when (bound-and-true-p electric-pair-mode)
    (setq-local electric-pair-pairs electric-pair-pairs)
    (cl-pushnew '(?` . ?`) electric-pair-pairs :test #'equal)
    (setq-local electric-pair-text-pairs electric-pair-text-pairs)
    (cl-pushnew '(?` . ?`) electric-pair-text-pairs :test #'equal)))

;; Assembly syntax highlighting support
(defun monad-syntax-propertize (start end)
  "Apply syntax properties from START to END."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; Character literals: mark the surrounding single quotes as string delimiters
    ;; This hides `"` and `(` inside character literals from Emacs's structural parser.
    ("\\('\\)\\(?:\\\\.\\|[^'\\]\\)\\('\\)"
     (1 "\"")
     (2 "\""))
    ("(\\s-*\\(asm\\)\\_>"
     (0 (ignore
         (let* ((asm-start (match-beginning 0))
                (asm-keyword-end (match-end 0))
                (asm-end (save-excursion
                           (goto-char asm-start)
                           (condition-case nil
                               (progn (forward-sexp 1) (1- (point)))
                             (error nil)))))
           (when asm-end
             (put-text-property asm-keyword-end asm-end 'monad-asm-region t))
           nil)))))
   start end))

(defun monad-font-lock-extend-region ()
  "Extend font-lock region to cover complete asm forms and block comments."
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (let ((changed nil))
    (save-excursion
      ;; Extend for asm forms
      (goto-char font-lock-beg)
      (when (monad-in-asm-form-p)
        (let ((start (previous-single-property-change (point) 'monad-asm-region)))
          (when start
            (goto-char start)
            (when (re-search-backward "(\\s-*asm\\_>" (max (point-min) (- start 100)) t)
              (setq font-lock-beg (point)
                    changed t)))))
      (goto-char font-lock-end)
      (when (monad-in-asm-form-p)
        (let ((end (next-single-property-change (point) 'monad-asm-region)))
          (when end
            (setq font-lock-end (1+ end)
                  changed t))))
      ;; Extend for -| ... |- block comments
      ;; If font-lock-beg is inside a block comment, pull beg back to the -|
      (goto-char font-lock-beg)
      (let ((search-start (max (point-min) (- font-lock-beg 10000))))
        (save-excursion
          (when (re-search-backward "-|" search-start t)
            (let ((open (point)))
              (save-excursion
                (goto-char (+ open 2))
                (let ((close (search-forward "|-" nil t)))
                  (when (and close (> close font-lock-beg))
                    (setq font-lock-beg open
                          changed t))))))))
      ;; If font-lock-end is inside a block comment, push end forward to the |-
      (save-excursion
        (goto-char font-lock-beg)
        (while (re-search-forward "-|" font-lock-end t)
          (let ((after-open (point)))
            (let ((close (save-excursion (search-forward "|-" nil t))))
              (when (and close (> close font-lock-end))
                (setq font-lock-end close
                      changed t)))))))
    changed))

(defun monad-in-asm-form-p (&optional pos)
  "Check if POS (or point) is inside an asm form."
  (get-text-property (or pos (point)) 'monad-asm-region))

(defun monad-asm-indent-line ()
  "Indent the current line in an asm block."
  (interactive)
  (let* ((savep (point))
         (target-col
          (save-excursion
            (condition-case nil
                (progn
                  (re-search-backward "(\\s-*asm\\_>" nil t)
                  (goto-char (match-end 0))
                  (skip-chars-forward " \t")
                  (if (not (eolp))
                      (current-column)
                    (+ (progn (goto-char (match-beginning 0))
                              (current-column))
                       2))) ; Changed from 5 to 2
              (error 2))))) ; Changed from 7 to 2
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((indent-col (if (looking-at "\\sw+:") 0 target-col)))
        (unless (= (current-indentation) indent-col)
          (delete-horizontal-space)
          (indent-to indent-col))))
    (when (< savep (save-excursion (back-to-indentation) (point)))
      (back-to-indentation))))

(defun monad-indent-line ()
  "Indent current line."
  (interactive)
  (cond
   ((monad-in-asm-form-p)
    (monad-asm-indent-line))
   ;; Inside a paragraph comment -> match indentation of the -| opening line + 3
   ((save-excursion
      (catch 'in-para
        (while (not (bobp))
          (forward-line -1)
          (cond
           ((looking-at "^$")  (throw 'in-para nil))
           ((looking-at "^-|")
            (throw 'in-para
                   (not (string-match-p "|-" (buffer-substring-no-properties
                                              (line-beginning-position)
                                              (line-end-position))))))
           ((looking-at "^\\s-+") nil)  ; continue searching
           (t (throw 'in-para nil))))
        nil))
    ;; Inside a paragraph comment -> match indentation of the -| opening line + 3
    (let* ((old-col (current-column))
           (old-indent (current-indentation))
           (para-indent (save-excursion
                          (catch 'found
                            (while (not (bobp))
                              (forward-line -1)
                              (when (looking-at "^-|")
                                (throw 'found (+ (current-indentation) 3))))
                            3))))
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to para-indent))
      (move-to-column (max para-indent (+ para-indent (- old-col old-indent))))))
   ;; Previous non-blank line closes a top-level sexp -> go to col 0
   ((save-excursion
      (forward-line -1)
      (while (and (not (bobp)) (looking-at "^\\s-*$"))
        (forward-line -1))
      (end-of-line)
      (and (eq (char-before) ?\))
           (condition-case nil
               (progn
                 (backward-sexp)
                 (and (= (current-column) 0)
                      (= (car (syntax-ppss)) 0)))
             (error nil))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space))
    (beginning-of-line))
   ;; Current line starts with define -> always top-level, no indent
   ((save-excursion
      (beginning-of-line)
      (looking-at "^\\s-*define\\s-+"))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space))
    (beginning-of-line))
   ;; Previous non-blank line is a wisp variable define (no ->) -> no indent
   ((save-excursion
      (forward-line -1)
      (while (and (not (bobp)) (looking-at "^\\s-*$"))
        (forward-line -1))
      (and (looking-at "^define\\s-+")
           (not (string-match-p "->" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space))
    (beginning-of-line))
   ;; Previous non-blank line is a bare block header -> indent one level deeper.
   ((save-excursion
      (forward-line -1)
      (while (and (not (bobp)) (looking-at "^\\s-*$"))
        (forward-line -1))
      (looking-at "^\\s-*\\(?:define\\|method\\|layout\\|type\\)\\s-+\\|^\\s-*where\\_>"))
    (let* ((old-col (current-column))
           (old-indent (current-indentation))
           (target-indent (save-excursion
                            (forward-line -1)
                            (while (and (not (bobp)) (looking-at "^\\s-*$"))
                              (forward-line -1))
                            (+ (current-indentation) 2))))
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to target-indent))
      (move-to-column (max target-indent
                           (+ target-indent (- old-col old-indent))))))
   ;; Previous non-blank line is an indented wisp body line -> match its indent
   ;; but NOT if it's inside a paragraph comment (blank line separates us)
   ((save-excursion
      (and
       ;; There must be no blank line between us and the previous indented line
       (progn (forward-line -1) (not (looking-at "^$")))
       (progn
         (while (and (not (bobp)) (looking-at "^\\s-*$"))
           (forward-line -1))
         (and (looking-at "^\\s-+")
              (not (looking-at "^("))))))
    (let* ((old-col (current-column))
           (old-indent (current-indentation))
           (prev-indent (save-excursion
                          (forward-line -1)
                          (while (and (not (bobp)) (looking-at "^\\s-*$"))
                            (forward-line -1))
                          (current-indentation))))
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to prev-indent))
      (move-to-column (max prev-indent (+ prev-indent (- old-col old-indent))))))
   (t
    (lisp-indent-line))))

(defun monad--paragraph-comment-start (&optional pos)
  "Return the start of the paragraph comment containing POS, or nil.
A paragraph comment starts with `-|' and runs until the first truly
empty line.  If a matching `|-' appears before that line, treat it as a
block comment instead."
  (save-excursion
    (goto-char (or pos (point)))
    (let ((origin (point)))
      (beginning-of-line)
      (catch 'found
        (while t
          (let ((bol (line-beginning-position))
                (eol (line-end-position)))
            (cond
             ((looking-at "^[ \t]*-|")
              (let* ((open-start (match-beginning 0))
                     (after-open (match-end 0))
                     (para-end
                      (save-excursion
                        (goto-char after-open)
                        (catch 'end
                          (while (not (eobp))
                            (forward-line 1)
                            (when (= (line-beginning-position)
                                     (line-end-position))
                              (throw 'end (point))))
                          (point-max))))
                     (close-pos
                      (save-excursion
                        (goto-char after-open)
                        (search-forward "|-" para-end t))))
                (throw 'found
                       (and (<= origin para-end)
                            (not close-pos)
                            open-start))))
             ((= bol eol)
              (throw 'found nil))
             ((looking-at "^[ \t]")
              (if (bobp)
                  (throw 'found nil)
                (forward-line -1)))
             (t
              (throw 'found nil)))))))))

(defun monad--paragraph-comment-backward-delete ()
  "Delete backward by content inside a paragraph comment.
Whitespace between point and the previous non-whitespace character is
removed together with that character, making backspace behave like
paragraph text editing."
  (let ((comment-start (monad--paragraph-comment-start)))
    (when comment-start
      (let* ((origin (point))
             (content-start (save-excursion
                              (goto-char comment-start)
                              (search-forward "-|" nil t)
                              (point)))
             (delete-start
              (save-excursion
                (skip-chars-backward " \t\n" content-start)
                (when (> (point) content-start)
                  (1- (point))))))
        (when delete-start
          (delete-region delete-start origin)
          t)))))

(defun monad--electric-pair-adjacent-p ()
  "Return non-nil when point is between adjacent electric-pair delimiters."
  (let ((open (char-before))
        (close (char-after)))
    (and open close
         (or (eq (cdr (assq open electric-pair-pairs)) close)
             (eq (cdr (assq open electric-pair-text-pairs)) close)
             (eq (matching-paren open) close)))))

(defun monad-backward-delete-char-untabify (arg)
  "Delete backward, with paragraph-comment content deletion.
With point in a `-|' paragraph comment, backspace ignores indentation
and blank continuation space, then deletes the previous real character.
Everywhere else, behave like `backward-delete-char-untabify'."
  (interactive "p")
  (cond
   ((use-region-p)
    (delete-region (region-beginning) (region-end)))
   ((and (= arg 1)
         (monad--paragraph-comment-backward-delete)))
   ((and (bound-and-true-p electric-pair-mode)
         (monad--electric-pair-adjacent-p))
    (electric-pair-delete-pair arg))
   (t
    (backward-delete-char-untabify arg))))

(defun monad--in-layout-block-p ()
  "Return non-nil when point is inside a parenthesized or bare layout block."
  (or (save-excursion
        (catch 'found
          (condition-case nil
              (while t
                (up-list -1)
                (when (and (eq (char-after) ?\()
                           (save-excursion
                             (forward-char 1)
                             (skip-chars-forward " \t\n")
                             (looking-at "layout\\_>")))
                  (throw 'found t)))
            (error nil))))
      (save-excursion
        (let ((indent (current-indentation)))
          (catch 'found
            (while (not (bobp))
              (forward-line -1)
              (unless (looking-at "^[ \t]*$")
                (let ((previous-indent (current-indentation)))
                  (cond
                   ((and (< previous-indent indent)
                         (looking-at "^[ \t]*layout\\s-+"))
                    (throw 'found t))
                   ((< previous-indent indent)
                    (throw 'found nil)))))))))))

(defun monad-insert-type-annotation ()
  "Insert ':: ' when typing a space inside [].
- Inside a layout block: trigger on single space after a word character.
- Otherwise: trigger on double space.
Aligns ':: ' to match the column of ':: ' on the previous line if present,
pushes previous lines right if the current name is longer, and cleans up
excess whitespace to the right."
  (when (save-excursion
          (backward-char 1)
          (condition-case nil
              (save-excursion (up-list -1) (eq (char-after) ?\[))
            (error nil)))
    (let* ((in-layout
            (monad--in-layout-block-p))
           (triggered
            (if in-layout
                (and (eq (char-before) ?\s)
                     (save-excursion
                       (backward-char 1)
                       (memq (char-syntax (char-before)) '(?w ?_))))
              (and (eq (char-before) ?\s)
                   (save-excursion
                     (backward-char 1)
                     (eq (char-before) ?\s))))))
      (when triggered
        (let ((target-col
               (save-excursion
                 (forward-line -1)
                 (when (re-search-forward "::" (line-end-position) t)
                   (- (match-beginning 0) (line-beginning-position))))))

          (delete-char (if in-layout -1 -2))

          (if target-col
              (let* ((base-col (current-column))
                     (required-col (+ base-col 1)))
                (if (< target-col required-col)
                    (let ((shift (- required-col target-col)))
                      (save-excursion
                        (forward-line -1)
                        (while (and (>= (point) (point-min))
                                    (save-excursion
                                      (beginning-of-line)
                                      (let ((match (re-search-forward "::" (line-end-position) t)))
                                        (and match (= (- (match-beginning 0) (line-beginning-position)) target-col)))))
                          (save-excursion
                            (beginning-of-line)
                            (re-search-forward "::" (line-end-position) t)
                            (goto-char (match-beginning 0))
                            (insert (make-string shift ?\s)))
                          (forward-line -1)))
                      (insert " :: "))
                  (let ((padding (- target-col base-col)))
                    (insert (make-string padding ?\s) ":: "))))
            (insert " :: "))

          ;; NEW: Vacuum up whitespace to the right
          (delete-region (point)
                         (save-excursion
                           (skip-chars-forward " \t")
                           (point))))))))

(defun monad--count-sexps (start end)
  "Count complete sexps between START and END."
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (condition-case nil
                 (progn (forward-sexp 1) (<= (point) end))
               (error nil))
        (setq count (1+ count)))
      count)))

(defun monad--delete-space-and-forward-arrow ()
  "Delete horizontal space at point and one following `->' token."
  (delete-region (point)
                 (save-excursion
                   (skip-chars-forward " \t")
                   (point)))
  (when (looking-at "->")
    (delete-region (point) (match-end 0))
    (delete-region (point)
                   (save-excursion
                     (skip-chars-forward " \t")
                     (point)))))

(defun monad--in-refinement-type-body-p ()
  "Return non-nil when point is inside a `type' refinement body."
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1)
          (when (eq (char-after) ?\{)
            (let ((brace-pos (point))
                  (brace-indent (current-indentation)))
              (or (save-excursion
                    (beginning-of-line)
                    (re-search-forward "\\_<type\\_>" brace-pos t))
                  (catch 'found
                    (while (not (bobp))
                      (forward-line -1)
                      (unless (looking-at "^[ \t]*$")
                        (if (< (current-indentation) brace-indent)
                            (throw 'found
                                   (looking-at "^[ \t]*(?type\\_>"))
                          (throw 'found nil))))
                    nil)))))
      (error nil))))


;; TODO We could also show eldoc showing the current param we are binding in pmatch before ->
(defun monad-insert-arrow-annotation ()
  "Insert '-> ' when typing a space after the last expected pattern sexp.
Dynamically counts expected parameters from peer lines or parent signature,
and aligns '-> ' to match previous lines. Handles type signatures and guards automatically."
  (when (and (eq (char-before) ?\s)
             (or (not (monad--in-layout-block-p))
                 (save-excursion
                   (beginning-of-line)
                   (looking-at "^\\s-*\\(?:define\\|method\\)\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+::")))
             (not (monad--in-refinement-type-body-p))
             ;; Prevent triggering if the user manually typed "-> "
             (not (and (>= (point) 3)
                       (string= (buffer-substring-no-properties (- (point) 3) (1- (point))) "->")
                       (not (save-excursion
                              (beginning-of-line)
                              (looking-at "^\\s-*\\(?:define\\|method\\)\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+::")))))
             ;; Ensure we are exactly after a sexp
             (save-excursion
               (backward-char 1)
               (let ((end-of-sexp (point)))
                 (beginning-of-line)
                 (skip-chars-forward " \t")
                 (or (string-suffix-p
                      "->"
                      (string-trim-right
                       (buffer-substring-no-properties (line-beginning-position)
                                                       end-of-sexp)))
                     (condition-case nil
                         (progn
                           ;; Move forward until we pass end-of-sexp to verify it's a boundary
                           (while (< (point) end-of-sexp)
                             (forward-sexp 1))
                           (= (point) end-of-sexp))
                       (error nil))))))

    ;; Determine context
    (let* ((line-beg (line-beginning-position))
           (line-str (buffer-substring-no-properties line-beg (line-end-position)))
           (text-before (string-trim-right (buffer-substring-no-properties line-beg (1- (point)))))
           (is-type-sig (or (string-match-p "^\\s-*\\(?:define\\|method\\)\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+::" line-str)
                            (string-match-p "^\\s-*::" line-str)))
           (is-guard-line (string-match-p "^\\s-*|\\s-*" line-str)))

      (unless (string-suffix-p "|" text-before)
        (if (and is-type-sig (not (string-suffix-p "::" text-before)))
          ;; FAST PATH: Just insert the arrow, no counting or alignment needed.
          ;; (Bypassed if the token right before the space was "::")
          (progn
            (let ((repeat-type
                   (when (string-match "->\\s-*\\'" text-before)
                     (let ((type-prefix
                            (string-trim-right
                             (substring text-before 0 (match-beginning 0)))))
                       (when (string-match ".*\\(?:->\\|::\\)\\s-*\\(.+\\)\\'" type-prefix)
                         (string-trim (match-string 1 type-prefix)))))))
              (if repeat-type
                  (progn
                    (delete-horizontal-space)
                    (insert " " repeat-type " -> "))
                (delete-char -1)
                (insert " -> ")))
            (monad--delete-space-and-forward-arrow))

        ;; REGULAR PATH: Pattern matching parameter counting, guards, and alignment
        ;; Prevent insertion if an arrow already exists on this line!
        (unless (or is-type-sig (string-match-p "\\s-->\\(?:\\s-\\|\\'\\)" text-before))
          (let* ((current-sexp-count (monad--count-sexps line-beg (1- (point))))
                 (current-indent (save-excursion (beginning-of-line) (current-indentation)))
                 (expected-sexps nil)
                 (target-col nil)
                 (trigger-insert nil)
                 (valid-arrow-context nil))

            (save-excursion
              (catch 'found
                (while (not (bobp))
                  (forward-line -1)
                  (cond
                   ((looking-at "^\\s-*$") nil)
                   ((looking-at "^\\s-*\"") nil)
                   ((looking-at "^\\s-*;") nil)
                   ((= (current-indentation) current-indent)
                    ;; Peer line: count sexps before the arrow
                    (when (re-search-forward "\\s-->" (line-end-position) t)
                      (let ((arrow-start (- (match-end 0) 2)))
                        (unless is-guard-line
                          (setq expected-sexps (monad--count-sexps (line-beginning-position) arrow-start)))
                        (setq target-col (save-excursion (goto-char arrow-start) (current-column))))))
                   ((< (current-indentation) current-indent)
                    ;; Parent line: parse header to find expected parameters
                    (beginning-of-line)
                    (if is-guard-line
                        (throw 'found t) ; Guards don't inherit expected-sexps from parent
                      (cond
                       ((looking-at "^\\s-*(?match\\_>.*\\_<with\\_>")
                        (setq valid-arrow-context t)
                        (unless expected-sexps
                          (setq expected-sexps 1))
                        (throw 'found t))
                       ((looking-at "^\\s-*\\(?:define\\|method\\)\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+::")
                        ;; Haskell style: define name :: A -> B -> C
                        (setq valid-arrow-context t)
                        (re-search-forward "::")
                        (let ((count 0) (limit (line-end-position)))
                          (while (re-search-forward "->" limit t)
                            (when (= (car (syntax-ppss)) 0) (setq count (1+ count))))
                          (unless expected-sexps
                            (setq expected-sexps (max 1 count))))
                        (throw 'found t))
                       ((looking-at "^\\s-*(define\\s-+(")
                        ;; Lisp style: (define (name p1 p2) or (define (name . Int -> Int)
                        (setq valid-arrow-context t)
                        (let ((count 0))
                          (condition-case nil
                              (progn
                                (down-list 1) (forward-sexp 1)
                                (down-list 1) (forward-sexp 1)
                                (skip-chars-forward " \t")
                                (while (not (or (looking-at "\\.") (looking-at "->") (looking-at ")")))
                                  (forward-sexp 1)
                                  (skip-chars-forward " \t")
                                  (setq count (1+ count))))
                            (error nil))
                          (if (= count 0)
                              (progn
                                (beginning-of-line)
                                (let ((arrow-count 0))
                                  (while (re-search-forward "->" (line-end-position) t)
                                    (setq arrow-count (1+ arrow-count)))
                                  (unless expected-sexps
                                    (setq expected-sexps (max 1 arrow-count)))))
                            (unless expected-sexps
                              (setq expected-sexps count))))
                        (throw 'found t))
                       ((looking-at "^\\s-*define\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+.*->")
                        (setq valid-arrow-context t)
                        (unless expected-sexps
                          (setq expected-sexps 1))
                        (throw 'found t)))))))))

            ;; Evaluate if we should trigger the arrow insertion
            (if is-guard-line
                (let* ((guard-expr-start (save-excursion
                                           (beginning-of-line)
                                           (re-search-forward "^\\s-*|\\s-*" (line-end-position) t)
                                           (point)))
                       (guard-str (string-trim (buffer-substring-no-properties guard-expr-start (1- (point))))))
                  ;; Smarter Guard Heuristic: 'otherwise', '(...)', or contains a comparison operator WITH a right-hand operand
                  (when (or (string= guard-str "otherwise")
                            (string-prefix-p "(" guard-str)
                            (string-match-p "\\(?:\\`\\|\\s-\\)\\(?:=\\|<=\\|!=\\|>=\\|<\\|>\\)\\s-+\\S-" guard-str))
                    (setq trigger-insert t)))
              (when (and valid-arrow-context
                         expected-sexps
                         (= current-sexp-count expected-sexps))
                (setq trigger-insert t)))

            ;; Execute insertion and alignment
            (when trigger-insert
              (delete-char -1) ; remove trigger space
              (if (numberp target-col)
                  (let* ((base-col (current-column))
                         (required-col (+ base-col 1)))
                    (if (< target-col required-col)
                        ;; 1. Current pattern is longer. Push previous line(s) right.
                        (let ((shift (- required-col target-col)))
                          (save-excursion
                            (forward-line -1)
                            (while (and (>= (point) (point-min))
                                        (save-excursion
                                          (beginning-of-line)
                                          (let ((match (re-search-forward "\\s-->" (line-end-position) t)))
                                            (and match
                                                 (save-excursion
                                                   (goto-char (- (match-end 0) 2))
                                                   (= (current-column) target-col))))))
                              (save-excursion
                                (beginning-of-line)
                                (re-search-forward "\\s-->" (line-end-position) t)
                                (goto-char (- (match-end 0) 2))
                                (insert (make-string shift ?\s)))
                              (forward-line -1)))
                          (insert " -> "))
                      ;; 2. Previous pattern is longer or equal. Pad the current line.
                      (let ((padding (- target-col base-col)))
                        (insert (make-string padding ?\s) "-> "))))
                ;; 3. No previous line to align to (first pattern arm)
                (insert " -> "))
              ;; Vacuum up whitespace and a pre-existing arrow to the right.
              (monad--delete-space-and-forward-arrow)))))))))

(defun monad--in-module-form-p ()
  "Return non-nil when point is inside a module form."
  (save-excursion
    (catch 'found
      (condition-case nil
          (while t
            (up-list -1)
            (when (and (eq (char-after) ?\()
                       (save-excursion
                         (forward-char 1)
                         (skip-chars-forward " \t\n")
                         (looking-at "module\\b")))
              (throw 'found t)))
        (error nil)))))

(defun monad-post-self-insert ()
  "Handle post-insertion actions."
  (let ((char (char-before)))
    (cond
     ((eq char ?|)
      (monad--expand-guard-pipe-after-self-insert))
     ((and (eq char ?:)
           (monad-in-asm-form-p))
      (save-excursion
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (font-lock-flush line-start line-end)
          (font-lock-fontify-region line-start line-end))))
     ((eq char ?\s)
      (unless (monad--in-module-form-p)
        (monad-insert-type-annotation)
        (monad-insert-arrow-annotation))))))

(defun monad-insert-lambda ()
  "Insert λ smartly, building λx.λy.λz... chains."
  (interactive)
  (cond
   ;; After λX (letter right after λ, no dot yet) -> insert .λ
   ((and (> (point) 1)
         (let ((c (char-before)))
           (and (>= c ?a) (<= c ?z)))
         (eq (char-before (1- (point))) ?λ))
    (insert ".λ"))
   ;; After λX.λ -> increment letter and continue chain
   ;; Structure: prev-letter . λ  <- point is here
   ((and (> (point) 3)
         (eq (char-before) ?λ)
         (eq (char-before (1- (point))) ?.)
         (let ((c (char-before (- (point) 2))))
           (and (>= c ?a) (<= c ?z))))
    (let* ((prev-letter (char-before (- (point) 2)))
           (next-letter (if (< prev-letter ?z) (1+ prev-letter) ?a)))
      (insert (char-to-string next-letter) ".λ")))
   ;; After bare λ -> insert first param 'x'
   ((eq (char-before) ?λ)
    (insert "x"))
   ;; Fallback: insert λ
   (t
    (insert "λ"))))

(defconst monad--guard-rail-entry
  (string ?\u252c ?\u2500)
  "Compact guard rail entry token.")

(defconst monad--guard-rail-branch
  (string ?\u251c ?\u2500)
  "Guard rail branch token.")

(defconst monad--guard-rail-fallback
  (string ?\u2570 ?\u2500 ?\u2500 ?\u2500 ?\u25b6)
  "Guard rail fallback token.")

(defconst monad--guard-rail-hanging
  (string ?\u2500 ?\u256e)
  "Hanging guard rail entry token.")

(defun monad--line-blank-p ()
  "Return non-nil when the current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*$")))

(defun monad--line-wisp-signature-p ()
  "Return non-nil when the current line is a Wisp define signature."
  (save-excursion
    (beginning-of-line)
    (looking-at-p
     (concat "^[ \t]*\\(?:define\\|method\\)\\s-+"
             monad--identifier-regexp
             "\\s-+::"))))

(defun monad--inside-wisp-definition-body-p ()
  "Return non-nil when point is in an indented Wisp definition body."
  (let ((indent (current-indentation)))
    (and (> indent 0)
         (save-excursion
           (catch 'found
             (while (not (bobp))
               (forward-line -1)
               (unless (monad--line-blank-p)
                 (let ((prev-indent (current-indentation)))
                   (cond
                    ((and (< prev-indent indent)
                          (monad--line-wisp-signature-p))
                     (throw 'found t))
                    ((< prev-indent indent)
                     (throw 'found nil)))))))
             nil))))

(defun monad--inside-wisp-definition-body-p ()
  "Return non-nil when point is in an indented Wisp definition body."
  (and (> (current-indentation) 0)
       (save-excursion
         (catch 'found
           (while (not (bobp))
             (forward-line -1)
             (unless (monad--line-blank-p)
               (cond
                ((monad--line-wisp-signature-p)
                 (throw 'found t))
                ((and (= (current-indentation) 0)
                      (looking-at-p
                       "^[ \t]*\\(?:define\\|method\\|layout\\|type\\|data\\|module\\|import\\|tests\\)\\_>"))
                 (throw 'found nil)))))
           nil))))

(defvar-local monad--guard-rail-aligning nil
  "Non-nil while Monad guard rail indentation is being updated.")

(defun monad--inside-wisp-definition-body-p ()
  "Return non-nil when point is in an indented Wisp definition body."
  (and (> (current-indentation) 0)
       (save-excursion
         (catch 'found
           (while (not (bobp))
             (forward-line -1)
             (unless (monad--line-blank-p)
               (cond
                ((monad--line-wisp-signature-p)
                 (throw 'found t))
                ((and (= (current-indentation) 0)
                      (looking-at-p
                       "^[ \t]*\\(?:define\\|method\\|layout\\|type\\|data\\|module\\|import\\|tests\\)\\_>"))
                 (throw 'found nil)))))
           nil))))

(defun monad--inside-square-brackets-p ()
  "Return non-nil when point is inside square brackets."
  (let ((open (nth 1 (syntax-ppss))))
    (and open
         (eq (char-after open) ?\[))))

(defun monad--guard-fallback-regexp ()
  "Return a regexp matching a Monad fallback rail."
  (concat (regexp-quote (string ?\u2570))
          (regexp-quote (string ?\u2500))
          "*"
          (regexp-quote (string ?\u25b6))))

(defun monad--line-has-guard-rail-p ()
  "Return non-nil when the current line already has a guard rail."
  (save-excursion
    (beginning-of-line)
    (re-search-forward
     (concat "\\(?:"
             (regexp-quote monad--guard-rail-entry)
             "\\|"
             (regexp-quote monad--guard-rail-hanging)
             "\\|"
             (regexp-quote monad--guard-rail-branch)
             "\\|"
             (monad--guard-fallback-regexp)
             "\\|->\\)")
     (line-end-position)
     t)))

(defun monad--smart-guard-entry-context-p (&optional pos)
  "Return non-nil when a pipe at POS should become a guard rail entry."
  (save-excursion
    (when pos
      (goto-char pos))
    (and (monad--font-lock-code-position-p (point))
         (monad--inside-wisp-definition-body-p)
         (not (monad--inside-square-brackets-p))
         (not (monad--line-has-guard-rail-p))
         (or (save-excursion
               (skip-chars-forward " \t")
               (eolp))
             (and (eq (char-after) ?|)
                  (save-excursion
                    (forward-char 1)
                    (skip-chars-forward " \t")
                    (eolp)))))))

(defun monad--insert-guard-rail-entry (token &optional trailing-space)
  "Insert guard rail entry TOKEN at point."
  (unless (or (bolp)
              (member (char-before) '(?\s ?\t)))
    (insert " "))
  (insert token)
  (when trailing-space
    (insert " ")))

(defun monad--expand-guard-pipe-after-self-insert ()
  "Rewrite a just-inserted pipe into a guard rail entry when appropriate."
  (when (and (eq last-command-event ?|)
             (> (point) (line-beginning-position))
             (eq (char-before) ?|)
             (monad--smart-guard-entry-context-p (1- (point))))
    (delete-char -1)
    (monad--insert-guard-rail-entry monad--guard-rail-entry t)))

(defun monad-pipe ()
  "Insert a pipe, or start a compact Unicode guard rail in Wisp bodies."
  (interactive)
  (if (monad--smart-guard-entry-context-p)
      (monad--insert-guard-rail-entry monad--guard-rail-entry t)
    (insert "|")))

(defun monad-hanging-pipe ()
  "Start a hanging Unicode guard rail in Wisp bodies."
  (interactive)
  (if (monad--smart-guard-entry-context-p)
      (progn
        (monad--insert-guard-rail-entry monad--guard-rail-hanging nil)
        (monad--insert-guard-rail-line
         (monad--guard-rail-column-on-line)
         monad--guard-rail-branch))
    (insert "|")))

(defun monad--guard-rail-column-on-line ()
  "Return the visual column of the guard rail on the current line."
  (save-excursion
    (beginning-of-line)
    (catch 'found
      (dolist (token (list monad--guard-rail-entry
                           monad--guard-rail-branch))
        (goto-char (line-beginning-position))
        (when (search-forward token (line-end-position) t)
          (goto-char (match-beginning 0))
          (throw 'found (current-column))))
      (goto-char (line-beginning-position))
      (when (search-forward monad--guard-rail-hanging (line-end-position) t)
        (goto-char (1+ (match-beginning 0)))
        (throw 'found (current-column)))
      (goto-char (line-beginning-position))
      (when (re-search-forward (monad--guard-fallback-regexp)
                               (line-end-position)
                               t)
        (goto-char (match-beginning 0))
        (throw 'found (current-column)))
      nil)))

(defun monad--guard-rail-entry-line-p ()
  "Return non-nil when the current line starts a guard rail block."
  (save-excursion
    (beginning-of-line)
    (or (search-forward monad--guard-rail-entry (line-end-position) t)
        (search-forward monad--guard-rail-hanging (line-end-position) t))))

(defun monad--guard-rail-branch-line-p ()
  "Return non-nil when the current line is a guard rail branch."
  (save-excursion
    (back-to-indentation)
    (or (looking-at-p (regexp-quote monad--guard-rail-branch))
        (looking-at-p (monad--guard-fallback-regexp)))))

(defun monad--guard-rail-fallback-line-p ()
  "Return non-nil when the current line is a guard rail fallback."
  (save-excursion
    (back-to-indentation)
    (looking-at-p (monad--guard-fallback-regexp))))

(defun monad--guard-rail-entry-line-position ()
  "Return the beginning of the guard rail entry line for point."
  (save-excursion
    (beginning-of-line)
    (cond
     ((monad--guard-rail-entry-line-p)
      (line-beginning-position))
     ((monad--guard-rail-branch-line-p)
      (catch 'found
        (while (not (bobp))
          (forward-line -1)
          (cond
           ((monad--guard-rail-entry-line-p)
            (throw 'found (line-beginning-position)))
           ((monad--guard-rail-branch-line-p)
            nil)
           ((monad--line-blank-p)
            nil)
           (t
            (throw 'found nil))))
        nil)))))

(defun monad--guard-rail-align-block-at-point (&optional pos)
  "Align the guard rail block around POS."
  (unless monad--guard-rail-aligning
    (save-excursion
      (when pos
        (goto-char pos))
      (let ((entry (monad--guard-rail-entry-line-position)))
        (when entry
          (let ((monad--guard-rail-aligning t)
                (inhibit-modification-hooks t))
            (with-silent-modifications
              (goto-char entry)
              (let ((column (monad--guard-rail-column-on-line)))
                (when column
                  (forward-line 1)
                  (while (and (not (eobp))
                              (monad--guard-rail-branch-line-p))
                    (unless (= (current-indentation) column)
                      (beginning-of-line)
                      (delete-horizontal-space)
                      (indent-to column))
                    (forward-line 1)))))))))))

(defun monad--guard-rail-align-block-near (pos)
  "Align any guard rail block near POS."
  (when pos
    (let ((marker (copy-marker pos t)))
      (unwind-protect
          (save-excursion
            (goto-char marker)
            (forward-line -4)
            (dotimes (_ 12)
              (ignore-errors
                (monad--guard-rail-align-block-at-point (point)))
              (unless (eobp)
                (forward-line 1))))
        (set-marker marker nil)))))

(defun monad--guard-rail-after-change (beg _end _len)
  "Keep guard rail blocks aligned after edits."
  (unless monad--guard-rail-aligning
    (monad--guard-rail-align-block-near beg)))

(defun monad--guard-rail-line-context-p ()
  "Return non-nil when the current line is an editable guard rail line."
  (and (monad--inside-wisp-definition-body-p)
       (monad--guard-rail-column-on-line)
       (not (monad--guard-rail-fallback-line-p))))

(defun monad--insert-guard-rail-line (column token)
  "Insert a new guard rail line at COLUMN with TOKEN."
  (delete-horizontal-space)
  (newline)
  (indent-to column)
  (insert token " "))

(defun monad--guard-rail-ret ()
  "Insert another guard branch when point is on a guard rail line."
  (when (monad--guard-rail-line-context-p)
    (let ((column (monad--guard-rail-column-on-line)))
      (end-of-line)
      (monad--insert-guard-rail-line column monad--guard-rail-branch)
      t)))

(defun monad-shift-ret ()
  "Insert an otherwise-style guard rail fallback branch."
  (interactive)
  (if (monad--guard-rail-line-context-p)
      (let ((column (monad--guard-rail-column-on-line)))
        (end-of-line)
        (monad--insert-guard-rail-line column monad--guard-rail-fallback))
    (monad-newline)))

(defun monad-open-line (arg)
  "Open ARG lines and repair nearby guard rail alignment."
  (interactive "p")
  (let ((origin (copy-marker (point) t))
        (count (or arg 1)))
    (unwind-protect
        (progn
          (open-line count)
          (monad--guard-rail-align-block-near (marker-position origin))
          (save-excursion
            (goto-char origin)
            (forward-line count)
            (monad--guard-rail-align-block-near (point))))
      (set-marker origin nil))))

(defun monad-colon ()
  "Insert a colon and auto-indent if in asm block."
  (interactive)
  (insert ":")
  (when (monad-in-asm-form-p)
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^[ \t]+\\(\\sw+:\\)")
        (delete-horizontal-space)
        (indent-to 0)))
    (save-excursion
      (beginning-of-line)
      (let ((line-end (line-end-position)))
        (font-lock-flush (point) line-end)
        (font-lock-fontify-region (point) line-end)))))

(defun monad-syntactic-face-function (state)
  "Determine face for syntax at STATE."
  (unless (monad-in-asm-form-p)
    (lisp-font-lock-syntactic-face-function state)))

(defconst monad-asm-instructions
  '(;; Data movement
    "mov" "movq" "movl" "movb" "movw" "movabs" "movsx" "movzx"
    "push" "pop" "pushq" "popq" "lea" "leaq"
    ;; Arithmetic
    "add" "addq" "addl" "sub" "subq" "subl"
    "mul" "imul" "imulq" "div" "idiv"
    "inc" "incq" "incl" "dec" "decq" "decl"
    "neg" "not"
    ;; Logic
    "and" "andq" "andl" "or" "orq" "orl" "xor" "xorq" "xorl"
    "shl" "shr" "sal" "sar" "rol" "ror"
    ;; Comparison / test
    "cmp" "cmpq" "cmpl" "test" "testq" "testl"
    ;; Jumps
    "jmp" "je" "jz" "jne" "jnz" "jg" "jge" "jl" "jle"
    "ja" "jae" "jb" "jbe" "js" "jns" "jo" "jno"
    ;; Control flow
    "call" "ret" "leave" "enter" "nop"
    ;; System
    "syscall" "int" "int3" "rdtsc"
    ;; String ops
    "rep" "repe" "repz" "repne" "repnz"
    "cpuid"
    ;; ARM
    "swi" "mrs" "lsr" "cset" "csel")
  "Assembly instructions.")

(defconst monad-asm-registers
  '("%rax" "%rbx" "%rcx" "%rdx" "%rsi" "%rdi" "%rbp" "%rsp"
    "%r8"  "%r9"  "%r10" "%r11" "%r12" "%r13" "%r14" "%r15"
    "%eax" "%ebx" "%ecx" "%edx" "%esi" "%edi" "%ebp" "%esp"
    "%ax"  "%bx"  "%cx"  "%dx"
    "%al"  "%bl"  "%cl"  "%dl"  "%ah"  "%bh"  "%ch"  "%dh"
    "%rip" "%eip"
    ;; ARM
    "r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7"
    "r8" "r9" "r10" "r11" "r12" "sp" "lr" "pc")
  "Assembly registers.")

(defvar monad-asm-font-lock-keywords-cache nil)

(defun monad-asm-get-font-lock-keywords ()
  "Get assembly font-lock keywords."
  (or monad-asm-font-lock-keywords-cache
      (setq monad-asm-font-lock-keywords-cache
            (if (and (boundp 'asm-font-lock-keywords)
                     (not (eq asm-font-lock-keywords 'unbound)))
                asm-font-lock-keywords
              (list
               '(";.*$" . font-lock-comment-face)
               '("^\\s-*\\([.a-zA-Z_][a-zA-Z0-9_]*\\):" 1 font-lock-function-name-face)
               (cons (regexp-opt monad-asm-instructions 'symbols)
                     'font-lock-keyword-face)
               (cons (regexp-opt monad-asm-registers)
                     'font-lock-variable-name-face)
               '("\\.[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-builtin-face)
               '("\\$-?[0-9]+"                . font-lock-constant-face)
               '("\\$0x[0-9a-fA-F]+"          . font-lock-constant-face)
               '("-?[0-9]+(%[a-z]+"           . font-lock-constant-face))))))


(defun monad-block-comment-matcher (limit)
  "Match -| ... |- block comments or -| paragraph comments up to LIMIT."
  (when (re-search-forward "-|" limit t)
    (let* ((open-start (match-beginning 0))
           (after-open (match-end 0))
           (close-pos  (save-excursion
                         (goto-char after-open)
                         (search-forward "|-" nil t)))
           ;; The first newline after -| marks end of paragraph comment
           (first-newline (save-excursion
                            (goto-char open-start)
                            (end-of-line)
                            (point))))
      (cond
       ;; Block comment: found |- with no nested -| before it
       ((and close-pos
             (not (save-excursion
                    (goto-char after-open)
                    (search-forward "-|" close-pos t))))
        (put-text-property open-start close-pos 'font-lock-multiline t)
        (set-match-data (list open-start close-pos))
        (goto-char close-pos))
       ;; Paragraph comment: no valid |- — highlight until first empty line
       (t
        (let ((para-end (save-excursion
                          (goto-char after-open)
                          (catch 'done
                            (while (not (eobp))
                              (forward-line 1)
                              (when (looking-at "^$")
                                (throw 'done (point))))
                            (point)))))
          (put-text-property open-start para-end 'font-lock-multiline t)
          (set-match-data (list open-start para-end)))))
      t)))

(defun monad-define-line-comment-matcher (limit)
  "Match `| ...' comments in `define name value | ...' lines up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward "^[ \t]*define\\_>" limit t))
      (let* ((line-beg (line-beginning-position))
             (line-end (line-end-position))
             (comment-start
              (save-excursion
                (save-restriction
                  (narrow-to-region line-beg line-end)
                  (goto-char (match-end 0))
                  (skip-chars-forward " \t")
                  (condition-case nil
                      (progn
                        (forward-sexp 1)
                        (skip-chars-forward " \t")
                        (forward-sexp 1)
                        (skip-chars-forward " \t")
                        (and (eq (char-after) ?|)
                             (point)))
                    (error nil))))))
        (if (and comment-start (< comment-start limit))
            (let ((comment-end (min line-end limit)))
              (set-match-data (list comment-start comment-end))
              (goto-char comment-end)
              (setq found t))
          (goto-char (min line-end limit)))))
    found))

(defun monad--skip-wisp-value-on-line (line-end)
  "Skip one Wisp value before LINE-END.
Return non-nil when the value is complete.

This handles normal Emacs sexps and Monad quoted list literals of the form
`⌜ ... ⌝'.  When a `⌜' has no matching `⌝' before LINE-END, move to
LINE-END and return nil so trailing text is not treated as a docstring yet."
  (skip-chars-forward " \t" line-end)
  (cond
   ((>= (point) line-end)
    nil)
   ((eq (char-after) ?⌜)
    (forward-char 1)
    (if (search-forward "⌝" line-end t)
        t
      (goto-char line-end)
      nil))
   (t
    (condition-case nil
        (progn
          (forward-sexp 1)
          (when (> (point) line-end)
            (goto-char line-end))
          t)
      (error
       (goto-char line-end)
       nil)))))

(defun monad-wisp-typed-value-docstring-matcher (limit)
  "Match docs after Wisp typed value defines up to LIMIT.
Supports same-line docs and an indented doc block on following lines.
This matcher is line-based so font-lock always moves forward."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 "^[ \t]*\\(?:([ \t]*\\)?\\(?:define\\|method\\)\\s-+\\["
                 limit t))
      (let ((header-indent (save-excursion
                             (goto-char (match-beginning 0))
                             (current-indentation)))
            (line-end (min (line-end-position) limit))
            match-start
            match-end)
        (save-excursion
          (goto-char (match-end 0))
          (condition-case nil
              (progn
                (backward-char 1)
                (forward-sexp 1)
                (skip-chars-forward " \t" line-end)
                (when (monad--skip-wisp-value-on-line line-end)
                  (skip-chars-forward " \t" line-end)
                  (cond
                   ((and (< (point) line-end)
                         (not (eq (char-after) ?\;))
                         (not (eq (char-after) ?:)))
                    (setq match-start (point)
                          match-end line-end))
                   ((>= (point) line-end)
                    (forward-line 1)
                    (when (and (< (point) limit)
                               (> (current-indentation) header-indent)
                               (not (looking-at-p "^[ \t]*$"))
                               (not (looking-at-p "^[ \t]*;"))
                               (not (looking-at-p "^[ \t]*:"))
                               (not (looking-at-p "^[ \t]*\\(?:define\\|method\\|layout\\|type\\|data\\|module\\|import\\|tests\\|test\\)\\_>")))
                      (setq match-start (save-excursion
                                          (back-to-indentation)
                                          (point)))
                      (while (and (< (point) limit)
                                  (> (current-indentation) header-indent)
                                  (not (looking-at-p "^[ \t]*$"))
                                  (not (looking-at-p "^[ \t]*;"))
                                  (not (looking-at-p "^[ \t]*:"))
                                  (not (looking-at-p "^[ \t]*\\(?:define\\|method\\|layout\\|type\\|data\\|module\\|import\\|tests\\|test\\)\\_>")))
                        (setq match-end (min (line-end-position) limit))
                        (forward-line 1)))))))
            (error nil)))
        (if (and match-start match-end (< match-start match-end))
            (progn
              (put-text-property match-start match-end
                                 'font-lock-multiline t)
              (set-match-data
               (list match-start match-end match-start match-end))
              (goto-char match-end)
              (setq found t))
          (goto-char line-end))))
    found))

(defun monad-refinement-type-docstring-matcher (limit)
  "Match indented docstrings following refinement type bodies up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward
                 (concat "^[ \t]*(?type[ \t]+"
                         monad--identifier-regexp "\\_>")
                 limit t))
      (unless (nth 8 (syntax-ppss))
        (save-excursion
          (goto-char (match-end 0))
          (skip-chars-forward " \t\n")
          (when (and (< (point) limit)
                     (eq (char-after) ?\{))
            (condition-case nil
                (progn
                  (forward-sexp 1)
                  (skip-chars-forward " \t\n")
                  (when (and (< (point) limit)
                             (eq (char-after) ?\")
                             (> (current-indentation) 0))
                    (let ((s (point)))
                      (forward-sexp 1)
                      (when (<= (point) limit)
                        (set-match-data (list s (point) s (point)))
                        (setq found t)))))
              (error nil))))))
    found))

(defconst monad--layout-field-regexp
  "\\[[^][\n]*::[^][\n]*\\]"
  "Regexp matching one layout field form on a single line.")

(defun monad--layout-header-line-p ()
  "Return non-nil when the current line starts a layout form."
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (looking-at-p "^[ \t]*\\(?:([ \t]*\\)?layout\\_>"))))

(defun monad--layout-line-in-layout-p ()
  "Return non-nil when the current line belongs to a layout.
This is deliberately indentation based and cheap for font-lock."
  (save-match-data
    (or
     (monad--layout-header-line-p)
     (save-excursion
       (let ((origin-indent (current-indentation))
             (done nil)
             (ok nil))
         (when (> origin-indent 0)
           (while (and (not done) (not (bobp)))
             (forward-line -1)
             (cond
              ((monad--layout-header-line-p)
               (setq ok (< (current-indentation) origin-indent)
                     done t))
              ((and (= (current-indentation) 0)
                    (not (looking-at-p "^[ \t]*$")))
               (setq done t))
              ((and (< (current-indentation) origin-indent)
                    (not (looking-at-p "^[ \t]*$")))
               (setq done t)))))
         ok)))))

(defun monad--layout-real-field-start-on-line (line-end)
  "Return the start of the next real layout field before LINE-END."
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (re-search-forward monad--layout-field-regexp line-end t))
        (setq found (match-beginning 0)))
      found)))

(defun monad--layout-field-doc-end (field-end line-end)
  "Return the doc end after FIELD-END, stopping before the next field."
  (save-excursion
    (goto-char field-end)
    (or (monad--layout-real-field-start-on-line line-end)
        line-end)))

(defun monad--layout-match-field-doc-on-line (line-end)
  "Match one layout field docstring on the current line before LINE-END."
  (let ((matched nil))
    (while (and (not matched)
                (re-search-forward monad--layout-field-regexp line-end t))
      (let* ((field-end (match-end 0))
             (doc-end (monad--layout-field-doc-end field-end line-end))
             doc-start)
        (goto-char field-end)
        (skip-chars-forward " \t" doc-end)
        (setq doc-start (point))
        (cond
         ((>= doc-start doc-end)
          (goto-char field-end))
         ((eq (char-after doc-start) ?:)
          (goto-char field-end))
         (t
          (put-text-property doc-start doc-end
                             'monad-layout-field-docstring t)
          (set-match-data (list doc-start doc-end doc-start doc-end))
          (goto-char doc-end)
          (setq matched t)))))
    matched))

(defun monad--layout-skip-balanced-token-on-line (line-end)
  "Skip one balanced bracket token before LINE-END."
  (when (and (< (point) line-end)
             (memq (char-after) '(?\( ?\[ ?\{)))
    (condition-case nil
        (let ((before (point)))
          (forward-sexp 1)
          (when (> (point) line-end)
            (goto-char line-end))
          (> (point) before))
      (error
       (skip-chars-forward "^ \t([{]" line-end)
       t))))

(defun monad--layout-skip-token-on-line (line-end)
  "Skip one layout metadata token before LINE-END.
This is delimiter-aware: v[x y] is skipped as v plus the whole [x y],
not as v[x followed by leaked doc text."
  (skip-chars-forward " \t" line-end)
  (cond
   ((>= (point) line-end)
    nil)
   ((monad--layout-skip-balanced-token-on-line line-end)
    t)
   (t
    (let ((before (point)))
      (skip-chars-forward "^ \t([{]" line-end)
      (while (and (< (point) line-end)
                  (monad--layout-skip-balanced-token-on-line line-end)))
      (> (point) before)))))

(defun monad--layout-skip-metadata-tail-on-line (line-end)
  "Skip bracket/list tail tokens belonging to one metadata payload."
  (skip-chars-forward " \t" line-end)
  (while (and (< (point) line-end)
              (memq (char-after) '(?\[ ?\( ?\{))
              (not (looking-at-p monad--layout-field-regexp)))
    (monad--layout-skip-token-on-line line-end)
    (skip-chars-forward " \t" line-end)))

(defun monad--layout-skip-metadata-on-line (line-end)
  "Skip leading :metadata payloads before a layout docstring."
  (skip-chars-forward " \t" line-end)
  (while (and (< (point) line-end)
              (eq (char-after) ?:))
    (monad--layout-skip-token-on-line line-end)
    (skip-chars-forward " \t" line-end)
    (when (and (< (point) line-end)
               (not (eq (char-after) ?:))
               (not (eq (char-after) ?\[))
               (not (eq (char-after) ?\;))
               (not (looking-at-p monad--layout-field-regexp)))
      (monad--layout-skip-token-on-line line-end)
      (monad--layout-skip-metadata-tail-on-line line-end))
    (skip-chars-forward " \t" line-end)))

(defun monad--layout-header-doc-start (line-end)
  "Return the start of an inline layout header docstring, or nil."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t" line-end)
    (when (looking-at "\\(?:([ \t]*\\)?layout\\_>")
      (goto-char (match-end 0))
      (skip-chars-forward " \t" line-end)
      (condition-case nil
          (forward-sexp 1)
        (error
         (goto-char line-end)))
      (skip-chars-forward " \t" line-end)
      (monad--layout-skip-metadata-on-line line-end)
      (let ((doc-start (point)))
        (unless (or (>= doc-start line-end)
                    (eq (char-after doc-start) ?\[)
                    (eq (char-after doc-start) ?:)
                    (eq (char-after doc-start) ?\;))
          doc-start)))))

(defun monad--layout-doc-end-on-line (doc-start line-end)
  "Return the end of a layout docstring starting at DOC-START."
  (save-excursion
    (goto-char doc-start)
    (let ((field-start (monad--layout-real-field-start-on-line line-end)))
      (goto-char (or field-start line-end))
      (skip-chars-backward " \t" doc-start)
      (point))))

(defun monad--layout-match-block-doc-on-line (line-end)
  "Match one whole-layout docstring line before LINE-END."
  (let* ((header-doc-start (monad--layout-header-doc-start line-end))
         (doc-start nil)
         (doc-end nil))
    (beginning-of-line)
    (skip-chars-forward " \t" line-end)
    (setq doc-start (or header-doc-start (point)))
    (goto-char doc-start)
    (setq doc-end (monad--layout-doc-end-on-line doc-start line-end))
    (cond
     ((>= doc-start doc-end)
      nil)
     ((and (not header-doc-start)
           (looking-at-p "\\(?:([ \t]*\\)?layout\\_>"))
      nil)
     ((eq (char-after doc-start) ?\[)
      nil)
     ((eq (char-after doc-start) ?:)
      nil)
     ((eq (char-after doc-start) ?\;)
      nil)
     (t
      (put-text-property doc-start doc-end
                         'monad-layout-docstring t)
      (set-match-data (list doc-start doc-end doc-start doc-end))
      (goto-char doc-end)
      t))))

(defun monad-layout-docstring-matcher (limit)
  "Match layout field and whole-layout docstrings up to LIMIT.
Inside a layout, text after a field closing bracket is a field docstring.
Indented non-field, non-metadata text lines are whole-layout docstrings.
This matcher always moves forward and does not call `up-list'."
  (when monad-highlight-layout-field-docstrings
    (let ((matched nil))
      (while (and (not matched) (< (point) limit))
        (let* ((scan-start (point))
               (line-start (line-beginning-position))
               (line-end (min (line-end-position) limit))
               (content-start
                (save-excursion
                  (beginning-of-line)
                  (skip-chars-forward " \t" line-end)
                  (point))))
          (cond
           ((>= scan-start line-end)
            (forward-line 1))
           ((not (monad--layout-line-in-layout-p))
            (forward-line 1))
           ((monad--layout-match-field-doc-on-line line-end)
            (setq matched t))
           ((or (monad--layout-header-line-p)
                (<= scan-start content-start))
            (goto-char line-start)
            (if (monad--layout-match-block-doc-on-line line-end)
                (setq matched t)
              (goto-char line-start)
              (forward-line 1)))
           (t
            (goto-char line-start)
            (forward-line 1)))))
      matched)))

(defconst monad-metadata-keyword-regexp
  ":\\(?:\\sw\\|\\s_\\)+"
  "Regexp matching Monad metadata keywords like :doc and :alias.")

(defun monad-metadata-keyword-matcher (limit)
  "Match Monad metadata keywords up to LIMIT."
  (catch 'found
    (while (re-search-forward monad-metadata-keyword-regexp limit t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (when (monad--font-lock-code-position-p beg)
          (set-match-data (list beg end beg end))
          (throw 'found t))))
    nil))

(defun monad-alias-value-matcher (limit)
  "Match the value after :alias up to LIMIT."
  (catch 'found
    (while (re-search-forward ":alias\\_>" limit t)
      (let ((line-end (min (line-end-position) limit))
            alias-start
            alias-end)
        (if (not (monad--font-lock-code-position-p (match-beginning 0)))
            (goto-char line-end)
          (skip-chars-forward " \t" line-end)
          (setq alias-start (point))
          (unless (or (>= alias-start line-end)
                      (eq (char-after alias-start) ?:)
                      (eq (char-after alias-start) ?\;))
            (skip-chars-forward "^ \t\n;" line-end)
            (setq alias-end (point)))
          (if (and alias-start alias-end (< alias-start alias-end))
              (progn
                (set-match-data (list alias-start alias-end alias-start alias-end))
                (goto-char alias-end)
                (throw 'found t))
            (goto-char line-end)))))
    nil))

(defun monad-doc-metadata-payload-matcher (limit)
  "Match :doc metadata and its payload up to LIMIT."
  (let (found)
    (while (and (not found)
                (re-search-forward ":doc\\_>" limit t))
      (let ((doc-keyword-start (match-beginning 0))
            (doc-keyword-end (match-end 0))
            (line-end (min (line-end-position) limit))
            doc-start
            doc-end)
        (if (not (monad--font-lock-code-position-p doc-keyword-start))
            (goto-char line-end)
          (skip-chars-forward " \t" line-end)
          (cond
           ((eq (char-after) ?\")
            (setq doc-start (point))
            (condition-case nil
                (progn
                  (forward-sexp 1)
                  (when (<= (point) limit)
                    (setq doc-end (point))))
              (error nil)))
           ((< (point) line-end)
            (setq doc-start (point)
                  doc-end line-end)))
          (if (and doc-start doc-end (< doc-start doc-end))
              (progn
                (set-match-data
                 (list doc-keyword-start doc-end
                       doc-keyword-start doc-keyword-end
                       doc-start doc-end))
                (goto-char doc-end)
                (setq found t))
            (goto-char line-end)))))
    found))

(defun monad-font-lock-keywords ()
  "Return font-lock keywords for Wisp mode."
  (append
   (list
    '(monad-type-arrow-matcher (0 'monad-type-arrow-face t))
    '(monad-arrow-matcher (0 'monad-arrow-face t))
    '(monad-guard-rail-matcher (0 'monad-guard-rail-face t))
    '(monad-box-comment-matcher (0 font-lock-comment-face t))
    '(monad-block-comment-matcher (0 (progn
                                       (put-text-property (match-beginning 0)
                                                          (match-end 0)
                                                          'font-lock-multiline t)
                                       font-lock-comment-face)
                                   t))
    '(monad-commentary-section-matcher (0 font-lock-comment-face t))
    '("^[ \t]*define[ \t]+[^ \t\n|]+[ \t]+[^ \t\n|]+[ \t]*\\(|.*\\)$" (1 font-lock-comment-face t))
    '(monad-wisp-typed-value-docstring-matcher (0 font-lock-doc-face t))
    '(monad-refinement-type-docstring-matcher (0 font-lock-doc-face t))
    '(monad-layout-docstring-matcher (0 font-lock-doc-face t))
    '(monad-path-literal-matcher (0 'monad-path-literal-face t))
    '(monad-escape-literal-matcher (0 'monad-escape-literal-face t))
    '("\\<include\\s-+\\(<[^>\n]+>\\)"
      (1 font-lock-string-face t))

    ;; Wisp-style (Haskell-like) defines — order matters: -> check before plain ::
    '("^\\s-*\\(?:define\\|method\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+::[^\n]*->"
      (1 font-lock-function-name-face))
    '("^\\s-*\\(?:define\\|method\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+::[^\n]*"
      (1 font-lock-function-name-face))
    '("^\\s-*\\(?:define\\|method\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-function-name-face))
    '("^\\s-*\\(?:define\\|method\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+->"
      (1 font-lock-function-name-face))

    '("(\\s-*\\(asm\\)\\_>" 1 font-lock-keyword-face)
    (cons (regexp-opt (remove "asm" monad-keywords) 'symbols)
          'font-lock-keyword-face)
    '("::" . font-lock-builtin-face)
    '(":\\(?:\\sw\\|\\s_\\)+"
      (0 font-lock-builtin-face))
    '("\\(:doc\\_>\\)[ \t\n]+\\(\"\\(?:[^\"\\]\\|\\\\.\\)*\"\\)"
      (1 font-lock-builtin-face t)
      (2 font-lock-doc-face t))
    '(monad-error-string-matcher
      (1 'default t)
      (2 'error t))
    '(monad-error-delimited-text-matcher
      (0 'default t))
    '(monad-test-string-matcher
      (0 font-lock-doc-face t))
    '(monad-char-literal-matcher . font-lock-string-face)
    '("\\<_\\>" . 'shadow)
    '("#\\+\\>" . 'shadow)
    '("\\(#\\+\\)\\(\\sw+\\)"
      (1 'shadow t)
      (2 font-lock-function-name-face t))
    '("#-\\sw+" . 'shadow)
    '("#-+\\>" . 'shadow)
    '("(\\(define\\|method\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))
    '("(\\(define\\|method\\)\\s-+\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))
   (let ((asm-keywords (monad-asm-get-font-lock-keywords)))
     (mapcar (lambda (keyword)
               (let* ((matcher (if (consp keyword) (car keyword) keyword))
                      (highlighter (if (consp keyword) (cdr keyword)
                                     'font-lock-keyword-face))
                      (face-spec (if (and (consp highlighter)
                                         (integerp (car highlighter)))
                                    highlighter
                                  `(0 ,highlighter)))
                      (subexp (car face-spec))
                      (face (cadr face-spec)))
                 (list
                  (lambda (limit)
                    (let (found)
                      (while (and (not found) (< (point) limit))
                        (if (monad-in-asm-form-p)
                            (let ((region-end (or (next-single-property-change
                                                  (point) 'monad-asm-region nil limit)
                                                 limit)))
                              (if (re-search-forward matcher region-end t)
                                  (setq found t)
                                (goto-char region-end)))
                          (goto-char (or (next-single-property-change
                                        (point) 'monad-asm-region nil limit)
                                       limit))))
                      found))
                  (list subexp face nil t))))
             asm-keywords))
   nil))

;; Set up docstring detection
(put 'lambda 'monad-doc-string-elt 2)
(put 'define 'monad-doc-string-elt
     (lambda ()
       (forward-comment (point-max))
       (cond
        ((eq (char-after) ?\() 2)
        (t
         (condition-case nil
             (progn
               (forward-sexp 1)
               (forward-comment (point-max))
               (forward-sexp 1)
               (forward-comment (point-max))
               (if (eq (char-after) ?\")
                   3
                 0))
           (error 0))))))

(put 'type 'monad-doc-string-elt
     (lambda ()
       (forward-comment (point-max))
       (condition-case nil
           (progn
             (forward-sexp 1) ; skip name
             (forward-comment (point-max))
             (if (eq (char-after) ?\{)
                 (progn
                   (forward-sexp 1) ; skip { ... }
                   (forward-comment (point-max))
                   (if (eq (char-after) ?\") 3 0))
               (if (eq (char-after) ?\") 2 0)))
         (error 0))))

;;; Eldoc integration
;;
;; - No eldoc when the cursor is immediately after (touching) a symbol.
;;   "After a symbol" means: the char before point is a word/symbol
;;   constituent AND point is NOT preceded by whitespace or an open paren.
;;   In practice: `x|' → silent; `(f |' → show f's signature.
;;
;; - When inside a function call `(fn arg1 arg2 ...)' the current parameter
;;   (zero-based index among the arguments) is highlighted with
;;   font-lock-variable-name-face; all other parameter names use the
;;   default face — matching emacs-lisp-mode behaviour.

(defun monad--point-after-symbol-p ()
  "Return non-nil when point is immediately after a symbol/word token.
Suppresses eldoc for `x|' (cursor at the trailing edge of a token)."
  (and (> (point) (point-min))
       (memq (char-syntax (char-before)) '(?w ?_))))

(defun monad--point-on-argument-symbol-p ()
  "Return non-nil when point is anywhere ON a symbol in argument position.
Covers single-char tokens, start-of-token, and interior-of-token.
Uses the nearest enclosing PAREN (not bracket) to find the call head."
  (when-let* ((sym (symbol-at-point)))
    (let* ((sym-start (save-excursion
                        (skip-syntax-backward "w_") (point)))
           ;; Walk up paren levels until we find a `(' (not `[')
           (fn-end (save-excursion
                     (condition-case nil
                         (progn
                           (let ((found nil))
                             (while (not found)
                               (up-list -1)
                               (when (eq (char-after) ?\()
                                 (setq found t)))
                             (forward-char 1)
                             (skip-chars-forward "
")
                             (skip-syntax-forward "w_")
                             (point)))
                       (error nil)))))
      (and fn-end (> sym-start fn-end)))))

(defun monad--point-in-define-annotation-p ()
  "Return non-nil when point is inside a typed `define' annotation bracket.
Matches `(define [name :: Type] value)' -- point is anywhere inside [...].
Returns non-nil when the condition holds, nil otherwise."
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1)                        ; go up into enclosing [...]
          (when (eq (char-after) ?\[)
            (up-list -1)                      ; go up into enclosing (define ...)
            (when (eq (char-after) ?\()
              (forward-char 1)
              (skip-chars-forward "
")
              (when (looking-at "define\_>")
                t))))                         ; confirmed: inside (define [...])
      (error nil))))

(defun monad--point-is-function-name-in-call-p ()
  "Return non-nil when point is ON the function-name token of a call form.
Example: in `(multo x y)', if point is anywhere inside `multo', returns t.
Requires the enclosing delimiter to be `(' -- bracket-enclosed symbols like
`i' in `[i :: Int]' are NOT considered function names."
  (save-excursion
    (condition-case nil
        (let ((orig-start
               (progn (skip-syntax-forward "w_")
                      (skip-syntax-backward "w_")
                      (point))))
          (up-list -1)
          ;; Must be a paren, not a bracket -- [i :: Int] is not a call form.
          (when (eq (char-after) ?\()
            (forward-char 1)
            (skip-chars-forward "
")
            (= (point) orig-start)))
      (error nil))))

(defun monad--depth-face (depth)
  "Return the rainbow-delimiters face for DEPTH."
  (intern (format "rainbow-delimiters-depth-%d-face"
                  (max 1 (min 9 (1+ depth))))))

(defun monad--propertize-signature (sig)
  "Return SIG as a propertized string with syntax colours."
  (let ((s (copy-sequence sig))
        (depth 0))
    ;; Delimiters first
    (dotimes (i (length s))
      (let ((ch (aref s i)))
        (cond
         ((memq ch '(?\( ?\[))
          (setq depth (1+ depth))
          (add-face-text-property i (1+ i) (monad--depth-face depth) t s))
         ((memq ch '(?\) ?\]))
          (add-face-text-property i (1+ i) (monad--depth-face depth) t s)
          (setq depth (max 0 (1- depth)))))))
    ;; :: operator
    (let ((i 0))
      (while (string-match "::" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'font-lock-builtin-face t s)
        (setq i (match-end 0))))

    ;; Type arrows
    (let ((i 0))
      (while (string-match "->" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'monad-type-arrow-face t s)
        (setq i (match-end 0))))

    ;; Function name
    (when (string-match "^(\\([A-Za-z_][A-Za-z0-9_'!?$%&*/+<=>.^~-]*\\)" s)
      (add-face-text-property (match-beginning 1) (match-end 1)
                              'font-lock-function-name-face t s))
    ;; Variable name at top-level "[name :: ..." (typed variable, not a param list)
    ;; Only apply when the signature is NOT a function header (no leading `(')
    (when (and (> (length s) 0) (not (eq (aref s 0) ?\())
               (string-match "^\\[\\([A-Za-z_][A-Za-z0-9_'!?$%&*/+<=>.^~-]*\\)[ \t]*::" s))
      (add-face-text-property (match-beginning 1) (match-end 1)
                              'font-lock-variable-name-face t s))
    ;; Parameter names inside [...] are intentionally NOT colored here.
    ;; monad--propertize-signature-with-active-param handles them with
    ;; per-slot highlighting (active = variable-name-face, others = default).
    ;; String literals
    (let ((i 0) (len (length s)))
      (while (< i len)
        (if (eq (aref s i) ?\")
            (let ((start i))
              (setq i (1+ i))
              (while (and (< i len)
                          (not (and (eq (aref s i) ?\")
                                    (not (eq (aref s (1- i)) ?\\)))))
                (setq i (1+ i)))
              (when (< i len) (setq i (1+ i)))
              (add-face-text-property start i 'font-lock-string-face t s))
          (setq i (1+ i)))))
    ;; Character literals
    (let ((i 0))
      (while (string-match "'\\(.\\)'" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'font-lock-string-face t s)
        (setq i (match-end 0))))
    s))

;;;; Parameter extraction

(defun monad--parse-param-names (header-str)
  "Parse parameter names from a function HEADER-STR.
Example: \"(fn [x :: T] -> [y :: T] -> R)\".
Returns a list of parameter name strings in order."
  (let (params (i 0))
    (while (string-match "\\[\\([a-z_][A-Za-z0-9_']*\\)[ \t]*::" header-str i)
      (push (match-string-no-properties 1 header-str) params)
      (setq i (match-end 0)))
    (nreverse params)))

(defun monad--current-arg-index ()
  "Return the zero-based index of the argument position at point.
Returns nil when point is not inside a function application form.
The index is the number of complete sexps between the function position
and point (i.e. 0 means we haven't typed any argument yet, 1 means we
are past the first argument, etc.)."
  (save-excursion
    (condition-case nil
        (let ((orig (point)))
          (up-list -1)
          (forward-char 1)
          (skip-chars-forward " \t\n")
          ;; Skip the function name; if it fails, signal so outer handler returns nil
          (condition-case nil
              (forward-sexp 1)
            (error (error "Skip-fn-failed")))
          ;; Count argument sexps between function name and orig
          (catch 'monad--arg-done
            (let ((idx 0))
              (while (and (< (point) orig) (not (eobp)))
                (skip-chars-forward " \t\n")
                (when (>= (point) orig)
                  (throw 'monad--arg-done idx))
                (condition-case nil
                    (progn
                      (forward-sexp 1)
                      (when (<= (point) orig)
                        (setq idx (1+ idx))))
                  (error (throw 'monad--arg-done idx))))
              idx)))
      (error nil))))

;;;; Propertize signature with active-parameter highlighting

(defun monad--propertize-signature-with-active-param (sig active-idx)
  "Return SIG propertized with ACTIVE-IDX parameter highlighted.
ACTIVE-IDX is zero-based.  Pass -1 to put ALL param names in default face
\(used when hovering the function name — no slot is being filled)."
  (let ((s (monad--propertize-signature sig))
        (param-spans '())
        (i 0))
    (while (string-match "\\[\\([a-z_][A-Za-z0-9_']*\\)[ \t]*::" sig i)
      (push (cons (match-beginning 1) (match-end 1)) param-spans)
      (setq i (match-end 0)))
    (setq param-spans (nreverse param-spans))
    (cl-loop for span in param-spans
             for param-idx from 0
             do (let ((b (car span))
                      (e (cdr span)))
                  (if (and (>= active-idx 0) (= param-idx active-idx))
                      (add-face-text-property b e 'font-lock-variable-name-face nil s)
                    (add-face-text-property b e 'default nil s))))
    s))

;;;; Context detection: are we in a function call?

(defun monad--enclosing-call-function-name ()
  "Return the name of the function being called in the enclosing sexp, or nil.
Returns nil when point is at the function-name position itself (i.e. arg-idx=0
would mean we haven't started arguments yet and we ARE the function name).
Only returns a name when we are inside the argument list of a call."
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1)
          (forward-char 1)
          (skip-chars-forward " \t\n")
          ;; Read what's at the function position
          (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
            (let ((fn-name (match-string-no-properties 0)))
              ;; Make sure point (before up-list) was PAST the function name
              ;; i.e. we are in the argument portion of the form, not on the fn itself
              fn-name)))
      (error nil))))

;;;; Main eldoc function

(defun monad--extract-function-header (name)
  "Return the header sexp string for function NAME, or nil."
  (or
   (monad--haskell-define-signature name)
   (save-excursion
     (goto-char (point-min))
     (let ((rx (concat "^(define[ \t\n]+(\\("
                       (regexp-quote name)
                       "\\)\\b")))
       (when (re-search-forward rx nil t)
         (goto-char (match-beginning 0))
         (condition-case nil
             (progn
               (down-list 1)
               (forward-sexp 1)
               (skip-chars-forward " \t\n")
               (let ((hdr-start (point)))
                 (forward-sexp 1)
                 (buffer-substring-no-properties hdr-start (point))))
           (error nil)))))))

(defun monad--extract-variable-info (name)
  "Return eldoc string for a variable NAME, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((typed-rx (concat "^(define[ \t\n]+\\(\\["
                            (regexp-quote name)
                            "[ \t]*::[^]\n]+\\]\\)")))
      (if (re-search-forward typed-rx nil t)
          (let ((annotation (match-string-no-properties 1)))
            (skip-chars-forward " \t\n")
            (condition-case nil
                (let ((val-start (point)))
                  (forward-sexp 1)
                  (concat annotation " "
                          (string-trim
                           (buffer-substring-no-properties val-start (point)))))
              (error nil)))
        (goto-char (point-min))
        (let ((plain-rx (concat "^(define[ \t\n]+"
                                (regexp-quote name)
                                "\\b")))
          (when (re-search-forward plain-rx nil t)
            (unless (string-match-p "(define[ \t\n]+(" (match-string 0))
              (skip-chars-forward " \t\n")
              (condition-case nil
                  (let ((val-start (point)))
                    (forward-sexp 1)
                    (string-trim
                     (buffer-substring-no-properties val-start (point))))
                (error nil)))))))))

(defun monad--extract-enclosing-param-type (name)
  "Return \"[NAME :: Type]\" if NAME is a typed parameter of the enclosing function."
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-defun)
          (when (looking-at "(define[ \t\n]*(")
            (down-list 1)
            (forward-sexp 1)
            (skip-chars-forward " \t\n")
            (when (eq (char-after) ?\()
              (let ((hdr-start (point)))
                (forward-sexp 1)
                (let ((hdr-str (buffer-substring-no-properties
                                (1+ hdr-start)
                                (1- (point)))))
                  (with-temp-buffer
                    (insert hdr-str)
                    (goto-char (point-min))
                    (let ((pat (concat "\\[" (regexp-quote name)
                                       "[ \t]*::[^]\n]+\\]")))
                      (when (re-search-forward pat nil t)
                        (match-string-no-properties 0)))))))))
      (error nil))))

(defun monad--eldoc-from-module-file (file bare-name)
  "Return an eldoc string for BARE-NAME found in FILE, or nil."
  (when (and file (file-readable-p file))
    (with-current-buffer (find-file-noselect file)
      (or (monad--extract-function-header bare-name)
          (monad--extract-variable-info   bare-name)))))

(defun monad--eldoc-from-imports (name)
  "Return a raw eldoc string for NAME found in any imported module."
  (let* ((imports (monad--parse-imports))
         (qualified-p (string-match "^\\([A-Z][^.]*\\)\\.\\(.+\\)$" name))
         (mod-name    (and qualified-p (match-string 1 name)))
         (bare        (if qualified-p (match-string 2 name) name))
         result)
    (if qualified-p
        (let ((file (monad--module-file mod-name)))
          (setq result (monad--eldoc-from-module-file file bare)))
      (cl-dolist (entry imports)
        (let* ((plist     (cdr entry))
               (qualified (plist-get plist :qualified))
               (file      (plist-get plist :file)))
          (unless qualified
            (when-let* ((doc (monad--eldoc-from-module-file file bare)))
              (setq result doc)
              (cl-return))))))
    result))

(defun monad--hover-doc (name)
  "Return a propertized eldoc string for NAME as a hover (no param highlight).
Checks enclosing-param annotation, then function header, then variable info,
then imports.  Returns nil if nothing is found."
  (or
   (when-let* ((ann (monad--extract-enclosing-param-type name)))
     (monad--propertize-signature ann))
   (when-let* ((hdr (monad--extract-function-header name)))
     (monad--propertize-signature hdr))
   (when-let* ((info (monad--extract-variable-info name)))
     (monad--propertize-signature info))
   (when-let* ((info (monad--eldoc-from-imports name)))
     (monad--propertize-signature info))))

(defun monad--call-signature (fn-name arg-idx)
  "Return a propertized signature for FN-NAME with ARG-IDX highlighted.
When ARG-IDX is -1 no parameter is highlighted (all use default face)."
  (let ((hdr (or (monad--extract-function-header fn-name)
                 (monad--eldoc-from-imports fn-name))))
    (when hdr
      (monad--propertize-signature-with-active-param hdr arg-idx))))

(defun monad--eldoc-get-doc ()
  "Return a propertized eldoc string for the current context, or nil.

Priority rules:
  1. Trailing edge of a symbol and NOT on an argument symbol => nil (silent).
  2. Point ON the function-name token of a call => signature, all params dimmed.
  3. Point ON an argument symbol => hover: show that symbol type/value.
  4. Point in a gap inside a call => signature with current param highlighted.
  5. Fallback: hover over any symbol at point."
  (let* ((sym      (symbol-at-point))
         (sym-name (and sym (symbol-name sym))))
    (cond
     ;; Rule 1: trailing edge, not sitting on an argument token -> silent,
     ;; UNLESS we are not inside any call at all (top-level hover is always ok).
     ((and (monad--point-after-symbol-p)
           (not (monad--point-on-argument-symbol-p))
           (not (monad--point-is-function-name-in-call-p))
           ;; Still show hover when there is no enclosing call context at all
           ;; (e.g. hovering `i' at the end of `(define [i :: Int] 20)').
           (monad--current-arg-index))
      nil)

     ;; Rule 2: cursor ON the function-name token -> show signature, all params dimmed.
     ((monad--point-is-function-name-in-call-p)
      (when sym-name
        (monad--call-signature sym-name -1)))

     ;; Rule 2.5: cursor inside a `(define [name :: Type] ...)' annotation bracket.
     ;; Show the variable's own type+value via hover, not a call signature.
     ((monad--point-in-define-annotation-p)
      (when sym-name
        (monad--hover-doc sym-name)))

     ;; Rule 3: cursor ON a written argument symbol -> hover for that symbol.
     ((monad--point-on-argument-symbol-p)
      (when sym-name
        (or (monad--hover-doc sym-name)
            ;; Symbol has no definition info; fall back to call signature.
            (let ((arg-idx (monad--current-arg-index))
                  (fn-name (monad--enclosing-call-function-name)))
              (when (and fn-name arg-idx)
                (monad--call-signature fn-name arg-idx))))))

     ;; Rule 4: gap inside a call -> highlighted parameter signature.
     (t
      (let* ((arg-idx (monad--current-arg-index))
             (fn-name (when arg-idx (monad--enclosing-call-function-name))))
        (or
         (when (and fn-name arg-idx)
           (monad--call-signature fn-name arg-idx))
         ;; Rule 5: outside any call form, generic hover.
         (when sym-name
           (monad--hover-doc sym-name))))))))

(defun monad-eldoc-function (callback &rest _ignored)
  "Call CALLBACK with the eldoc documentation string for the current context."
  (when-let* ((doc (monad--eldoc-get-doc)))
    (funcall callback doc)
    t))

(defun monad-mode-variables ()
  "Set up variables for Monad mode."
  (set-syntax-table monad-mode-syntax-table)
  (cond
   ((fboundp 'flycheck-mode)
    (flycheck-mode 1)
    (with-eval-after-load 'flycheck
      (flycheck-select-checker 'monad)))
   ((fboundp 'flymake-mode)
    (monad--setup-flymake)
    (flymake-mode 1)))
  (setq local-abbrev-table monad-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'monad-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local lisp-indent-function 'scheme-indent-function)
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-create-index-function #'monad--imenu-build-index)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local completion-extra-properties
              (list :annotation-function #'monad-imenu-annotate))

  (cond
   ((boundp 'marginalia-annotator-registry)
    (add-to-list 'marginalia-annotator-registry
                 '(imenu monad-imenu-annotate builtin none)))
   ((boundp 'marginalia-annotators)
    (add-to-list 'marginalia-annotators
                 '(imenu monad-imenu-annotate builtin none))))

  (add-hook 'after-change-functions
            (lambda (&rest _) (monad--invalidate-docstring-cache))
            nil t)
  (add-hook 'after-change-functions #'monad--guard-rail-after-change nil t)
  (setq-local syntax-propertize-function #'monad-syntax-propertize)
  (setq-local font-lock-extend-region-functions
              (list #'monad-font-lock-extend-region
                    #'font-lock-extend-region-wholelines
                    #'font-lock-extend-region-multiline))
  (setq font-lock-defaults
        '((monad-font-lock-keywords)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . monad-syntactic-face-function)))
  (setq-local lisp-doc-string-elt-property 'monad-doc-string-elt)
  (local-set-key (kbd "M-;") #'monad-comment-dwim)
  (local-set-key (kbd "C-o") #'monad-open-line)
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'insert (kbd "C-o") #'monad-open-line))
  (add-hook 'xref-backend-functions #'monad-xref-backend nil t)
  (add-hook 'completion-at-point-functions #'monad-completion-at-point nil t)
  (add-hook 'post-self-insert-hook #'monad-post-self-insert nil t)
  (add-hook 'electric-pair-mode-hook #'monad--setup-electric-pair nil t)
  (monad--setup-electric-pair)
  (rainbow-delimiters-mode 1)
  (when (fboundp 'porg-mode)
    (porg-mode 1))
  (eldoc-mode 1)
  (setq-local eldoc-documentation-functions '(monad-eldoc-function))
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-default))

(defun monad-xref-go-back ()
  "Go back with xref, keeping the REPL in its own window."
  (interactive)
  (let* ((repl-buf    (and (boundp 'monad-repl-buffer-name)
                           (get-buffer monad-repl-buffer-name)))
         (history     (funcall xref-history-storage))
         (top-marker  (car (car history)))
         (top-buf     (and (markerp top-marker) (marker-buffer top-marker)))
         (origin-win  (selected-window)))
    (cond
     ;; History points back to the REPL
     ((and repl-buf top-buf (eq top-buf repl-buf))
      ;; Don't call xref-go-back, handle the pop ourselves
      (let* ((marker (pop (car (funcall xref-history-storage))))
             (win    (or (get-buffer-window repl-buf)
                         (let ((w (split-window-sensibly origin-win)))
                           (or w (split-window origin-win nil 'below))))))
        (select-window win)
        (switch-to-buffer repl-buf)
        (goto-char (marker-position marker))
        (run-hooks 'xref-after-return-hook)))
     ;; Normal xref go back
     (t
      (xref-go-back)))))

;;; Module system

(defun monad--current-file-dir ()
  "Return the directory of the current buffer's file, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defcustom monad-core-lib-path "/usr/local/lib/monad"
  "Path to the installed Monad core library."
  :type 'string
  :group 'monad)

(defun monad--module-file (module-name)
  "Return the path to MODULE-NAME's .mon file, or nil if not found.
Searches the current buffer's directory first, then subdirectories,
then the installed core library at `monad-core-lib-path'."
  (let ((filename (concat module-name ".mon")))
    (or
     ;; 1. Sibling of current file
     (when-let* ((dir (monad--current-file-dir)))
       (let ((f (expand-file-name filename dir)))
         (when (file-readable-p f) f)))
     ;; 2. Core library — search recursively
     (when (file-directory-p monad-core-lib-path)
       (let ((found nil))
         (dolist (f (directory-files-recursively monad-core-lib-path "\\.mon$"))
           (when (string= (file-name-nondirectory f) filename)
             (setq found f)))
         found)))))

(defun monad--parse-exports (file)
  "Return the list of exported symbol names from FILE.
Each symbol is propertized with the correct `company-kind': `function' or
`variable', determined by inspecting the actual definitions in FILE."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward
           "^(module\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+\\[\\([^]]*\\)\\]" nil t)
          (let* ((export-names (split-string (match-string-no-properties 1)
                                             "[ \t\n]+" t))
                 ;; Build a kind-lookup table from the actual definitions in this file.
                 (defines (monad--collect-defines))
                 (kind-table (make-hash-table :test #'equal)))
            (dolist (d defines)
              (puthash d (get-text-property 0 'company-kind d) kind-table))
            (mapcar (lambda (s)
                      (propertize s 'company-kind
                                  (or (gethash s kind-table) 'function)))
                    export-names))
        ;; No explicit export list -- return all defines with their real kinds.
        (monad--collect-defines)))))

(defun monad--collect-defines ()
  "Collect all top-level define names in the current buffer."
  (let (names)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (monad--define-name-regexp) nil t)
        (let* ((name-start (match-beginning 1))
               (name      (match-string-no-properties 1))
               (kind      (if (or (eq (char-before name-start) ?\()
                                   (save-excursion
                                     (goto-char (match-beginning 0))
                                     (looking-at
                                      (concat "^define\\s-+"
                                              (regexp-quote name)
                                              "\\_>\\s-+::"))))
                              'function
                            'variable)))
          (when name
            (push (propertize name 'company-kind kind) names)))))
    (nreverse names)))

(defun monad--parse-imports ()
  "Return an alist of imported modules in the current buffer."
  (let (imports)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^(import\\s-+\\(qualified\\s-+\\)?\\(\\(?:\\sw\\|\\s_\\)+\\)\\(.*\\))"
              nil t)
        (let* ((qualified  (match-string-no-properties 1))
               (modname    (match-string-no-properties 2))
               (rest       (string-trim (match-string-no-properties 3)))
               (alias      (when (string-match ":as\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" rest)
                             (match-string-no-properties 1 rest)))
               (hiding     (when (string-match "hiding\\s-+\\[\\([^]]*\\)\\]" rest)
                             (split-string (match-string-no-properties 1 rest)
                                           "[ \t\n]+" t)))
               (only       (when (and (not hiding)
                                      (string-match "\\[\\([^]]*\\)\\]" rest))
                             (split-string (match-string-no-properties 1 rest)
                                           "[ \t\n]+" t)))
               (file       (monad--module-file modname)))
          (push (list modname
                      :qualified (if qualified t nil)
                      :alias     alias
                      :hiding    hiding
                      :only      only
                      :file      file)
                imports))))
    (nreverse imports)))

(defun monad--import-completions (imports)
  "Return completion candidates derived from IMPORTS."
  (let (candidates)
    (dolist (entry imports)
      (let* ((modname   (car entry))
             (plist     (cdr entry))
             (qualified (plist-get plist :qualified))
             (alias     (plist-get plist :alias))
             (hiding    (plist-get plist :hiding))
             (only      (plist-get plist :only))
             (file      (plist-get plist :file))
             (exports   (monad--parse-exports file))
             (symbols   (cond
                         (hiding (cl-remove-if (lambda (s) (member s hiding)) exports))
                         (only   (cl-remove-if-not (lambda (s) (member s only)) exports))
                         (t      exports)))
             (prefix    (or alias modname)))
        (unless qualified
          (dolist (sym symbols)
            (push sym candidates)))
        (dolist (sym symbols)
          (let ((kind (get-text-property 0 'company-kind sym)))
            (push (propertize (concat prefix "." sym) 'company-kind kind)
                  candidates)))))
    (nreverse candidates)))

(defun monad--asm-first-token-p (start)
  "Return t if START is the first token position on its line in an asm block."
  (save-excursion
    (goto-char start)
    (skip-chars-backward " \t")
    (bolp)))

(defun monad--asm-operand-candidates ()
  "Completion candidates for asm operand positions."
  (delete-dups
   (append
    (mapcar (lambda (r) (propertize r 'company-kind 'variable))
            monad-asm-registers)
    (monad--collect-defines))))

(defun monad--asm-candidates ()
  "Return asm completion candidates."
  (delete-dups
   (append
    (mapcar (lambda (i) (propertize i 'company-kind 'monad-asm))
            monad-asm-instructions)
    (mapcar (lambda (r) (propertize r 'company-kind 'variable))
            monad-asm-registers)
    (monad--collect-defines))))

(defun monad--keyword-candidates ()
  "Return monad keywords as propertized candidates."
  (mapcar (lambda (kw) (propertize kw 'company-kind 'keyword))
          monad-keywords))

(defun monad--all-completions ()
  "Return all Monad completion candidates."
  (delete-dups
   (append (monad--keyword-candidates)
           (monad--collect-defines)
           (monad--import-completions (monad--parse-imports)))))

;;;; Completion-at-point

(defun monad--kind-function (candidates)
  "Return a `company-kind' function over CANDIDATES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (c candidates)
      (puthash c (get-text-property 0 'company-kind c) table))
    (lambda (cand) (gethash cand table))))

(when (boundp 'nerd-icons-corfu-mapping)
  (add-to-list 'nerd-icons-corfu-mapping
               '(monad-asm :fn (lambda (_cand)
                                 (concat (nerd-icons-sucicon "nf-seti-asm") " "))
                           :face font-lock-keyword-face)))

(defun monad-completion-at-point ()
  "Completion-at-point for Monad mode."
  (let ((ppss (syntax-ppss)))
    (unless (or (nth 3 ppss)
                (nth 4 ppss))
      (let* ((end   (point))
             (start (save-excursion
                      (skip-syntax-backward "w_%")
                      (when (and (> (point) (point-min))
                                 (eq (char-before) ?.))
                        (skip-syntax-backward "w_"))
                      (point)))
             (in-asm (monad-in-asm-form-p start))
             (candidates
              (cond
               ((and in-asm (monad--asm-first-token-p start))
                (mapcar (lambda (i) (propertize i 'company-kind 'monad-asm))
                        monad-asm-instructions))
               (in-asm
                (monad--asm-operand-candidates))
               (t
                (monad--all-completions))))
             (kind-fn (monad--kind-function candidates)))
        (list start end
              (completion-table-dynamic (lambda (_) candidates))
              :exclusive    'no
              :company-kind kind-fn
              :annotation-function
              (lambda (cand)
                (pcase (funcall kind-fn cand)
                  ('function
                   (if-let* ((hdr (monad--extract-function-header cand)))
                       (concat " " hdr)
                     " <function>"))
                  ('variable
                   (if-let* ((info (monad--extract-variable-info cand)))
                       (concat " " info)
                     " <variable>"))
                  ('monad-asm " <asm>")
                  ('keyword   " <keyword>")
                  (_          ""))))))))

;;; Xref backend

(defun monad-xref-backend ()
  "Monad backend for xref."
  'monad)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql monad)))
  "Return the identifier at point."
  (let* ((sym (symbol-at-point))
         (name (and sym (symbol-name sym))))
    (when name
      (save-excursion
        (let ((end (point)))
          (skip-syntax-backward "w_")
          (when (and (> (point) (point-min))
                     (eq (char-before) ?.))
            (backward-char 1)
            (skip-syntax-backward "w_")
            (setq name (buffer-substring-no-properties (point) end)))))
      name)))

(defun monad--find-all-defines (buf symbol)
  "Return a list of all xref locations where SYMBOL is defined in BUF."
  (let (locs)
    (with-current-buffer buf
      (dolist (rx (monad--definition-name-regexps symbol))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward rx nil t)
            (push (xref-make symbol
                             (xref-make-buffer-location buf (match-beginning 1)))
                  locs)))))
    (sort locs (lambda (a b)
                 (< (marker-position
                     (xref-location-marker (xref-item-location a)))
                    (marker-position
                     (xref-location-marker (xref-item-location b))))))))

(defun monad--find-define-pos (buf symbol)
  "Return the first xref location for SYMBOL's definition in BUF, or nil."
  (car (monad--find-all-defines buf symbol)))

(defun monad--find-parameters (symbol)
  "Search for SYMBOL as a typed parameter in the enclosing define."
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-defun)
          (when (looking-at "(define[ \t\n]*(")
            (down-list 1)
            (forward-sexp 1)
            (skip-chars-forward " \t\n")
            (when (eq (char-after) ?\()
              (let* ((hdr-open  (point))
                     (hdr-open1 (1+ hdr-open))
                     (_dummy    (forward-sexp 1))
                     (hdr-close (1- (point)))
                     (hdr-str   (buffer-substring-no-properties
                                 hdr-open1 hdr-close))
                     (pat       (concat "\\[" (regexp-quote symbol)
                                        "[ \t]*::"))
                     result)
                (with-temp-buffer
                  (insert hdr-str)
                  (goto-char (point-min))
                  (when (re-search-forward pat nil t)
                    (setq result (+ hdr-open1 (match-beginning 0)))))
                (when result
                  (xref-make (concat symbol " (parameter)")
                             (xref-make-buffer-location
                              (current-buffer) result)))))))
      (error nil))))

(defun monad--find-in-file (file symbol)
  "Search FILE for all definitions of SYMBOL."
  (when (and file (file-readable-p file))
    (with-current-buffer (find-file-noselect file)
      (let ((bare (if (string-match "\\." symbol)
                      (replace-regexp-in-string "^[^.]+\\." "" symbol)
                    symbol)))
        (monad--find-all-defines (current-buffer) bare)))))

(defun monad--find-module-location (module-name)
  "Return an xref location for MODULE-NAME inside its module declaration."
  (when-let* ((file (monad--module-file module-name)))
    (when (file-readable-p file)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward
                 (concat "^(module\\s-+\\(" (regexp-quote module-name) "\\)") nil t)
            (xref-make (concat "module " module-name)
                       (xref-make-buffer-location
                        (current-buffer)
                        (match-beginning 1)))))))))

(cl-defmethod xref-backend-definitions ((_backend (eql monad)) symbol)
  "Find definitions of SYMBOL."
  (catch 'monad-xref-done
    (let (defs)
      (when (and (string-match-p "^[A-Z]" symbol)
                 (not (string-match-p "\\." symbol)))
        (when-let* ((loc (monad--find-module-location symbol)))
          (push loc defs)))
      (when (string-match "^\\([A-Z][^.]*\\)\\.\\(.+\\)$" symbol)
        (let* ((modname (match-string 1 symbol))
               (bare    (match-string 2 symbol))
               (file    (monad--module-file modname)))
          (dolist (loc (monad--find-in-file file bare))
            (push loc defs))))
      (when (null defs)
        (when-let* ((param-loc (monad--find-parameters symbol)))
          (throw 'monad-xref-done (list param-loc)))
        (dolist (loc (monad--find-all-defines (current-buffer) symbol))
          (push loc defs))
        (when (null defs)
          (dolist (entry (monad--parse-imports))
            (let ((file (plist-get (cdr entry) :file)))
              (dolist (loc (monad--find-in-file file symbol))
                (push loc defs))))))
      (nreverse defs))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql monad)))
  "Return all identifiers."
  (let* ((imports (monad--parse-imports))
         (mods    (mapcar (lambda (m) (propertize (car m) 'company-kind 'module))
                          imports)))
    (delete-dups
     (append (monad--keyword-candidates)
             (monad--collect-defines)
             mods
             (monad--import-completions imports)))))

;;; Flycheck backend

(defcustom monad-compiler-executable "monad"
  "Path to the Monad compiler executable."
  :type 'string
  :group 'monad)

(with-eval-after-load 'flycheck
  (flycheck-define-checker monad
    "A Monad syntax/type checker using the monad compiler."
    :command ("monad" source "-o" "/dev/null")
    :error-patterns
    ((error   line-start (file-name) ":" line ":" column ": error: "   (message) line-end)
     (warning line-start (file-name) ":" line ":" column ": warning: " (message) line-end)
     (info    line-start (file-name) ":" line ":" column ": note: "    (message) line-end))
    :modes monad-mode)
  (add-to-list 'flycheck-checkers 'monad))

;;; Flymake backend

(defvar-local monad--flymake-proc nil)

(defun monad-flymake-backend (report-fn &rest _args)
  "Flymake backend for Monad.  REPORT-FN is called with diagnostics."
  (unless (executable-find monad-compiler-executable)
    (error "Cannot find monad compiler '%s'" monad-compiler-executable))

  ;; Kill any running check process
  (when (process-live-p monad--flymake-proc)
    (kill-process monad--flymake-proc))

  (let* ((source (current-buffer))
         (tmpfile (make-temp-file "monad-flymake-" nil ".mon")))

    ;; Write buffer contents to temp file
    (write-region nil nil tmpfile nil 'silent)

    (setq monad--flymake-proc
          (make-process
           :name "monad-flymake"
           :noquery t
           :connection-type 'pipe
           :buffer (generate-new-buffer " *monad-flymake*")
           :stderr (generate-new-buffer " *monad-flymake*")
           :command (list monad-compiler-executable tmpfile)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (if (with-current-buffer source
                         (eq proc monad--flymake-proc))
                       (with-current-buffer (process-buffer proc)
                         (goto-char (point-min))
                         (let (diags)
                           (while (re-search-forward
                                   ;; Pattern: file:line:col: level: message
                                   (rx bol
                                       (? (group (+ (not ":"))) ":")  ; optional file
                                       (group (+ digit)) ":"          ; line
                                       (group (+ digit)) ": "         ; col
                                       (group (or "error" "warning" "note")) ": "
                                       (group (+ any)) eol)
                                   nil t)
                             (let* ((line    (string-to-number (match-string 2)))
                                    (col     (string-to-number (match-string 3)))
                                    (level   (match-string 4))
                                    (msg     (match-string 5))
                                    (type    (pcase level
                                               ("error"   :error)
                                               ("warning" :warning)
                                               (_         :note)))
                                    (region  (flymake-diag-region source line col)))
                               (push (flymake-make-diagnostic
                                      source
                                      (car region) (cdr region)
                                      type
                                      msg)
                                     diags)))
                           (funcall report-fn (nreverse diags))))
                     (flymake-log :warning "Obsolete monad-flymake process"))
                 (ignore-errors (delete-file tmpfile))
                 (kill-buffer (process-buffer proc)))))))))

(defun monad--setup-flymake ()
  "Register the Monad Flymake backend in the current buffer."
  (add-hook 'flymake-diagnostic-functions #'monad-flymake-backend nil t))

(defun monad--read-full-docstring-at-point ()
  "Return the full docstring text for the define form at point."
  (condition-case nil
      (progn
        (down-list 1)
        (forward-sexp 1)
        (skip-chars-forward " \t\n")
        (let ((is-function (eq (char-after) ?\()))
          (forward-sexp 1)
          (unless is-function
            (skip-chars-forward " \t\n")
            (condition-case nil (forward-sexp 1) (error nil))))
        (let (doc done)
          (while (not done)
            (skip-chars-forward " \t\n")
            (cond
             ((looking-at ":doc[ \t\n]+\"")
              (goto-char (match-end 0))
              (let ((s (1- (point))))
                (goto-char s)
                (forward-sexp 1)
                (setq doc  (buffer-substring-no-properties (1+ s) (1- (point)))
                      done t)))
             ((looking-at ":\\(?:\\sw\\|\\s_\\)+")
              (forward-sexp 1)
              (skip-chars-forward " \t\n")
              (condition-case nil (forward-sexp 1) (error (setq done t))))
             ((looking-at "\"")
              (let ((s (point)))
                (forward-sexp 1)
                (setq doc  (buffer-substring-no-properties (1+ s) (1- (point)))
                      done t)))
             (t (setq done t))))
          (and doc (not (string-empty-p doc)) (string-trim doc))))
    (error nil)))

(defun monad--full-docstring-for (name)
  "Return the full docstring text for NAME, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((fn-rx  (concat "^(define[ \t\n]+(\\(" (regexp-quote name) "\\)\\b"))
          (var-rx (concat "^(define[ \t\n]+\\[?\\(" (regexp-quote name) "\\)\\b")))
      (let ((pos (or (and (re-search-forward fn-rx nil t)  (match-beginning 0))
                     (progn (goto-char (point-min))
                            (and (re-search-forward var-rx nil t) (match-beginning 0))))))
        (when pos
          (goto-char pos)
          (monad--read-full-docstring-at-point))))))

(defun monad-show-docstring ()
  "Show the full docstring of the symbol at point in the minibuffer."
  (interactive)
  (let* ((sym  (symbol-at-point))
         (name (and sym (symbol-name sym))))
    (unless name
      (user-error "No symbol at point"))
    (let ((doc (monad--full-docstring-for name)))
      (unless doc
        (user-error "No docstring for `%s'" name))
      (message "%s" (propertize doc 'face 'font-lock-doc-face)))))

(defun monad-comment-dwim ()
  "Insert -| comment, or close with |- if current line has an unclosed -|."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
    (if (string-match-p "-|" line)
        (if (and (> (point) (line-beginning-position))
                 (not (eq (char-before) ?\s)))
            (insert " |-")
          (insert "|-"))
      (insert "-| "))))

(defun monad-compile-and-run ()
  "Compile and run current Monad file using compilation mode."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((file (file-name-nondirectory buffer-file-name))
        (exe (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
    (compile (format "monad %s && ./%s" file exe))))

(defun monad-compile-and-run-tests ()
  "Compile with --test flag and run current Monad file."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((file (file-name-nondirectory buffer-file-name))
        (exe (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
    (compile (format "monad --test %s && ./%s" file exe))))

(defun monad-goto-tests ()
  "Move cursor to the opening parenthesis of the (tests form."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^(tests\\_>" nil t)
      (progn
        (goto-char (match-beginning 0))
        (recenter-top-bottom 0))
    (user-error "No (tests form found in buffer")))

(defun monad-goto-module ()
  "Move cursor to the opening parenthesis of the (module form."
  (interactive)
  (goto-char (point-min))
  (if (re-search-forward "^(module\\_>" nil t)
      (goto-char (match-beginning 0))
    (user-error "No (module form found in buffer")))

;; Modal i guess
(defun monad-insert-or (key action)
  "Bind KEY to insert itself or run ACTION on region.
Fully preserves undo behavior for self-insertion."
  (if (string= key (char-to-string (string-to-char key))) ; single character keys only
      (define-key (current-global-map) (kbd key)
                  `(menu-item "" nil :filter
                              (lambda (&optional _)
                                (if (use-region-p)
                                    (progn
                                      (setq this-command ',action)
                                      ',action)
                                  (setq this-command 'self-insert-command)
                                  (let ((last-command-event ,(string-to-char key)))
                                    'self-insert-command)))))
    (define-key (current-global-map) (kbd key)
                `(lambda ()
                   (interactive)
                   (if (use-region-p)
                       (call-interactively ',action)
                     (insert ,key))))))

(defun monad-newline ()
  "Insert a newline smartly based on context.
- Inside layout/module blocks: inserts [] and copies types.
- Inside pattern match clauses: auto-inserts [] for list types.
- Inside guards: auto-continues the | symbol.
- Before guards: detects clause heads and auto-indents with |."
  (interactive)
  (let* ((at-eol (save-excursion (skip-chars-forward " \t)") (eolp)))
         (ppss (syntax-ppss))
         (paren-depth (car ppss))
         (at-target-entry-end
          (save-excursion
            (skip-chars-forward " \t")
            (when (eq (char-after) ?\])
              (forward-char 1)
              (skip-chars-forward " \t)")
              (eolp))))
         (bare-block-header
          (and at-eol
               (save-excursion
                 (beginning-of-line)
                 (when (looking-at (concat "^[ \t]*(?\\(layout\\|type\\)\\s-+"
                                           "\\(" monad--identifier-regexp "\\)"
                                           "\\_>[ \t)]*$"))
                   (list (match-string-no-properties 1)
                         (match-beginning 2)
                         (current-indentation))))))
         ;; Renamed variable to reflect it handles multiple block types
         (in-target-block (or (monad--in-layout-block-p)
                              (save-excursion
                                (catch 'found
                                  (condition-case nil
                                      (while t
                                        (up-list -1)
                                        (when (and (eq (char-after) ?\()
                                                   (save-excursion
                                                     (forward-char 1)
                                                     (skip-chars-forward " \t\n")
                                                     (looking-at "\\(?:layout\\|module\\)\\_>")))
                                          (throw 'found t)))
                                    (error nil)))))))

    ;; 1. If currently inside an empty target [] block, just jump inside it
    (if (and in-target-block
             (save-excursion
               (beginning-of-line)
               (looking-at "[ \t]*\\[[ \t]*\\][ \t)]*$")))
        (progn
          (beginning-of-line)
          (search-forward "["))

      ;; 2. Target block layout type inheritance
      (when in-target-block
        (let (prev-type target-col)
          (save-excursion
            (forward-line -1)
            (beginning-of-line)
            (when (re-search-forward "::[ \t]*\\([^]\n]+?\\)[ \t]*\\]" (line-end-position) t)
              (setq prev-type (match-string 1)
                    target-col (save-excursion
                                 (goto-char (match-beginning 0))
                                 (current-column)))))
          (when prev-type
            (save-excursion
              (beginning-of-line)
              (cond
               ((re-search-forward "\\[[ \t]*[^: \t]+[ \t]*::\\([ \t]*\\)\\][ \t)]*$" (line-end-position) t)
                (delete-region (match-beginning 1) (match-end 1))
                (goto-char (match-beginning 1))
                (insert " " prev-type))
               ((progn
                  (beginning-of-line)
                  (re-search-forward "\\[[ \t]*[^]: \t]+\\([ \t]*\\)\\][ \t)]*$" (line-end-position) t))
                (delete-region (match-beginning 1) (match-end 1))
                (goto-char (match-beginning 1))
                (insert (make-string (max 1 (- target-col (current-column))) ?\s)
                        ":: " prev-type)))))))

      ;; 3. Smart Newline Execution
      (when (and in-target-block at-target-entry-end)
        (search-forward "]" (line-end-position) t))
      (cond
       ;; Refinement type set body: split before the closing brace and start a guard.
       ((save-excursion
          (and (condition-case nil
                   (save-excursion
                     (up-list -1)
                     (eq (char-after) ?{))
                 (error nil))
               (let ((line-before-point
                      (buffer-substring-no-properties (line-beginning-position)
                                                      (point))))
                 (and (string-match-p "∈" line-before-point)
                      (not (string-match-p "^\\s-*|" line-before-point))))
               (progn
                 (skip-chars-forward " \t")
                 (eq (char-after) ?}))))
        (delete-region (point)
                       (save-excursion
                         (skip-chars-forward " \t")
                         (point)))
        (newline)
        (indent-to (save-excursion
                     (forward-line -1)
                     (current-indentation)))
        (insert "|  ")
        (backward-char))

       ;; Refinement guard line: split before the closing brace and indent continuation.
       ((save-excursion
          (and (condition-case nil
                   (save-excursion
                     (up-list -1)
                     (eq (char-after) ?{))
                 (error nil))
               (string-match-p "^\\s-*|"
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (point)))
               (progn
                 (skip-chars-forward " \t")
                 (eq (char-after) ?}))))
        (let ((indent (+ (current-indentation) 2)))
          (delete-region (point)
                         (save-excursion
                           (skip-chars-forward " \t")
                           (point)))
          (newline)
          (indent-to indent)))

       ;; If not at EOL, fallback to standard newline to avoid messing up mid-line splits
       ((and (not at-eol) (not (and in-target-block at-target-entry-end)))
        (newline-and-indent))

       ;; Bare layout/type headers use the same two-space body indentation as define.
       (bare-block-header
        (pcase-let ((`(,kind ,name-start ,indent) bare-block-header))
          (monad--capitalize-initial-at name-start)
          (newline)
          (indent-to (+ indent 2))
          (pcase kind
            ("layout"
             (insert "[]")
             (backward-char))
            ("type"
             (insert "{  }")
             (backward-char 2)))))

       ;; Where opens a plain block, including inside layout blocks.
       ((string= (string-trim (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
                 "where")
        (let ((indent (current-indentation)))
          (newline)
          (indent-to (+ indent 2))))

       ;; Pattern matching / Guard logic (only trigger if we are at top-level depth)
       ((and (<= paren-depth 1) (not in-target-block))
        (let ((line-str (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (indent (current-indentation)))
          (cond
           ;; Case A: Continuing a guard line (starts with |)
           ((and (string-match-p "^|" line-str)
                 (not (string-match-p "}" line-str)))
            (newline)
            (indent-to indent)
            (insert "| "))

           ;; Case B: End of an inline guard arm.
           ((string-match-p "\\_<|\\_>.*->" line-str)
            (let ((guard-column
                   (save-excursion
                     (beginning-of-line)
                     (search-forward "|" (line-end-position) t)
                     (1- (current-column)))))
              (newline)
              (indent-to guard-column)
              (insert "| ")))

           ;; Case C: End of a pattern clause (has ->, does not start with |)
           ((string-match-p "->" line-str)
            (newline-and-indent)
            ;; Check if the first arg in the define signature is a List or [a]
            (when (save-excursion
                    (when (re-search-backward "^\\s-*\\(?:(define\\|define\\)\\_>" nil t)
                      ;; Look up to 2 lines ahead to catch signatures split across a newline
                      (re-search-forward "::\\s-*\\(\\[.*?\\]\\|List\\b\\)" (save-excursion (forward-line 2) (point)) t)))
              (insert "[]")
              (backward-char)))

           ;; Case D: Start of guards (clause head without ->)
           ((and (not (string-empty-p line-str))
                 (not (string-match-p "->" line-str))
                 (not (string-match-p "::" line-str)))
            (let ((should-guard
                   (save-excursion
                     (forward-line -1)
                     (while (and (not (bobp)) (looking-at "^\\s-*$"))
                       (forward-line -1))
                     ;; Trigger if peer line has -> OR parent define/match line has ->
                     (or (and (= (current-indentation) indent)
                              (string-match-p "->" (thing-at-point 'line t)))
                         (and (< (current-indentation) indent)
                              (looking-at "^\\s-*\\(?:(define\\|define\\|case\\|match\\)\\_>")
                              (string-match-p "->" (thing-at-point 'line t)))))))
              (if should-guard
                  (progn
                    (newline)
                    (indent-to (+ indent 2))
                    (insert "| "))
                (newline-and-indent))))

           ;; Default: normal newline
           (t (newline-and-indent)))))

       ;; 4. Fallback for Target blocks (insert [])
       (t
        (newline-and-indent)
        (when (and (or at-eol at-target-entry-end) in-target-block)
          (insert "[]")
          (backward-char)))))))


(defvar-keymap monad-unicode-map
  :doc "Keymap for inserting Unicode symbols in Monad mode."
  "o" (lambda () (interactive) (insert "∘"))
  "e" (lambda () (interactive) (insert "∈"))
  "E" (lambda () (interactive) (insert "∃"))
  "A" (lambda () (interactive) (insert "∀"))
  "p" (lambda () (interactive) (insert "π"))
  "P" (lambda () (interactive) (insert "Π"))
  "t" (lambda () (interactive) (insert "θ"))
  "T" (lambda () (interactive) (insert "Θ"))
  "u" (lambda () (interactive) (insert "∪"))
  "n" (lambda () (interactive) (insert "∩"))
  "l" (lambda () (interactive) (insert "λ"))
  "L" (lambda () (interactive) (insert "Λ"))
  "S" (lambda () (interactive) (insert "Σ"))
  "s" (lambda () (interactive) (insert "σ"))
  "d" (lambda () (interactive) (insert "Δ"))
  "a" (lambda () (interactive) (insert "α"))
  "b" (lambda () (interactive) (insert "β"))
  "g" (lambda () (interactive) (insert "γ"))
  "G" (lambda () (interactive) (insert "Γ"))
  "m" (lambda () (interactive) (insert "μ"))
  "x" (lambda () (interactive) (insert "×"))
  "*" (lambda () (interactive) (insert "×"))
  "-" (lambda () (interactive) (insert "→"))
  "_" (lambda () (interactive) (insert "↦"))
  "=" (lambda () (interactive) (insert "⇒"))
  "<" (lambda () (interactive) (insert "⟨"))
  ">" (lambda () (interactive) (insert "⟩"))
  "[" (lambda () (interactive) (progn (insert "⌜⌝") (backward-char)))
  "]" (lambda () (interactive) (insert "⌝"))
  "{" (lambda () (interactive) (progn (insert "⌞⌟") (backward-char)))
  "}" (lambda () (interactive) (insert "⌟")))



;;; Matrix literal support

(defvar-local monad--matrix-start nil
  "Marker at the beginning of the first matrix content line.")

(defvar-local monad--matrix-bottom nil
  "Marker at the beginning of the ─╯ line.")

(defun monad--matrix-update-bottom ()
  "Rewrite the ─╯ line to align with the widest content line."
  (when (and monad--matrix-start
             monad--matrix-bottom
             (marker-buffer monad--matrix-start)
             (marker-buffer monad--matrix-bottom))
    (save-excursion
      (let ((max-len 0)
            (pos (marker-position monad--matrix-start))
            (bottom (marker-position monad--matrix-bottom)))
        ;; Scan every content line between start and bottom markers
        (goto-char pos)
        (while (< (point) bottom)
          (let ((len (- (line-end-position) (line-beginning-position))))
            (when (> len max-len)
              (setq max-len len)))
          (forward-line 1))
        ;; Rewrite the bottom line
        (goto-char bottom)
        (delete-region (line-beginning-position) (line-end-position))
        (insert (make-string max-len ? ) "─╯")))))

(defun monad--matrix-post-command ()
  "Hook: update matrix bottom delimiter after any command."
  (when (and monad--matrix-start
             monad--matrix-bottom
             (marker-buffer monad--matrix-start)
             (marker-buffer monad--matrix-bottom))
    (let ((pos (point))
          (bottom (marker-position monad--matrix-bottom)))
      (if (< pos bottom)
          (monad--matrix-update-bottom)
        (set-marker monad--matrix-start nil)
        (set-marker monad--matrix-bottom nil)
        (remove-hook 'post-command-hook #'monad--matrix-post-command t)))))

(defun monad-insert-matrix ()
  "Insert a matrix literal and track the bottom delimiter automatically."
  (interactive)
  (when (markerp monad--matrix-start)  (set-marker monad--matrix-start nil))
  (when (markerp monad--matrix-bottom) (set-marker monad--matrix-bottom nil))
  (remove-hook 'post-command-hook #'monad--matrix-post-command t)
  (insert "╭─\n  \n   ─╯")
  (forward-line -1)
  (end-of-line)
  (setq monad--matrix-start (copy-marker (line-beginning-position) nil))
  (save-excursion
    (forward-line 1)
    (setq monad--matrix-bottom (copy-marker (line-beginning-position) nil)))
  (run-with-timer 0 nil
                  (lambda ()
                    (add-hook 'post-command-hook #'monad--matrix-post-command nil t))))

;;; Fraction literal editor

(defconst monad--fraction-rule-char #x2500
  "Character used for Monad fraction rules.")

(defvar-local monad--fraction-rail nil
  "Marker at the start of the active fraction rule line.")

(defvar-local monad--fraction-overlay nil
  "Overlay covering the active fraction editor.")

(defvar-local monad--fraction-field 'top
  "Current active fraction field.  Either `top' or `bottom'.")

(defvar-local monad--fraction-updating nil
  "Non-nil while the active fraction editor is rerendering.")

(defun monad--fraction-active-p ()
  "Return non-nil when there is an active fraction editor."
  (and (markerp monad--fraction-rail)
       (marker-buffer monad--fraction-rail)))

(defun monad--fraction-rule (width)
  "Return a fraction rule string of WIDTH rule characters."
  (make-string (max 1 width) monad--fraction-rule-char))

(defun monad--fraction-rule-line-p ()
  "Return non-nil when the current line is a fraction rule line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (let ((start (point)))
      (while (eq (char-after) monad--fraction-rule-char)
        (forward-char 1))
      (and (> (point) start)
           (progn
             (skip-chars-forward " \t")
             (eolp))))))

(defun monad--fraction-rule-column-at (rail)
  "Return the display column of the fraction rule at RAIL."
  (save-excursion
    (goto-char rail)
    (if (eq (char-after) monad--fraction-rule-char)
        (current-column)
      (current-indentation))))

(defun monad--fraction-context-at-point ()
  "Return fraction context at point, or nil.
The returned plist has :rail, :col, and :field.  :field is nil when point
is on the rule line itself."
  (or
   (when (and (monad--fraction-active-p)
              (overlayp monad--fraction-overlay)
              (overlay-buffer monad--fraction-overlay)
              (>= (point) (overlay-start monad--fraction-overlay))
              (<= (point) (overlay-end monad--fraction-overlay)))
     (let* ((rail (marker-position monad--fraction-rail))
            (rail-line (line-number-at-pos rail))
            (here-line (line-number-at-pos (point)))
            (field (cond
                    ((= here-line (1- rail-line)) 'top)
                    ((= here-line (1+ rail-line)) 'bottom)
                    ((= here-line rail-line) nil))))
       (list :rail rail
             :col (monad--fraction-rule-column-at rail)
             :field field)))
   (let (rail field)
     (save-excursion
       (beginning-of-line)
       (cond
        ((monad--fraction-rule-line-p)
         (setq rail (line-beginning-position)
               field nil))
        ((save-excursion
           (forward-line -1)
           (monad--fraction-rule-line-p))
         (forward-line -1)
         (setq rail (line-beginning-position)
               field 'bottom))
        ((save-excursion
           (forward-line 1)
           (monad--fraction-rule-line-p))
         (forward-line 1)
         (setq rail (line-beginning-position)
               field 'top))))
     (when rail
       (list :rail rail
             :col (monad--fraction-rule-column-at rail)
             :field field)))))

(defun monad--fraction-start-at (rail &optional field)
  "Make the fraction whose rule starts at RAIL active.
FIELD remembers the field point is currently editing."
  (monad-fraction-finish)
  (setq monad--fraction-rail (copy-marker rail nil)
        monad--fraction-field (or field 'top))
  (monad--fraction-update-overlay)
  (add-hook 'post-command-hook #'monad--fraction-post-command nil t)
  t)

(defun monad--fraction-activate-at-point ()
  "Activate the fraction under point, if point is over one."
  (let* ((ctx (monad--fraction-context-at-point))
         (rail (plist-get ctx :rail))
         (field (plist-get ctx :field)))
    (when ctx
      (if (and (monad--fraction-active-p)
               (= (marker-position monad--fraction-rail) rail))
          (progn
            (when field
              (setq monad--fraction-field field))
            (monad--fraction-update-overlay))
        (monad--fraction-start-at rail field))
      t)))

(defun monad--fraction-center (text width)
  "Return TEXT centered inside WIDTH display columns."
  (let* ((text-width (string-width text))
         (pad (max 0 (- width text-width)))
         (left (/ pad 2))
         (right (- pad left)))
    (concat (make-string left ?\s)
            text
            (make-string right ?\s))))

(defun monad--fraction-line-start (offset)
  "Return the start position of fraction line OFFSET from the rule line."
  (save-excursion
    (goto-char (marker-position monad--fraction-rail))
    (forward-line offset)
    (line-beginning-position)))

(defun monad--fraction-read-line (offset col)
  "Read and trim the fraction line OFFSET starting at display column COL."
  (save-excursion
    (goto-char (monad--fraction-line-start offset))
    (move-to-column col t)
    (string-trim
     (buffer-substring-no-properties (point) (line-end-position)))))

(defun monad--fraction-current-field ()
  "Return the fraction field containing point, or the remembered field."
  (let* ((ctx (monad--fraction-context-at-point))
         (field (plist-get ctx :field)))
    (or field monad--fraction-field 'top)))

(defun monad--fraction-point-index (field col text)
  "Return point index inside FIELD content at display column COL."
  (if (not (eq field (monad--fraction-current-field)))
      (length text)
    (save-excursion
      (let ((target (point)))
        (goto-char (marker-position monad--fraction-rail))
        (forward-line (if (eq field 'top) -1 1))
        (move-to-column col t)
        (let* ((start (point))
               (raw (buffer-substring-no-properties start (line-end-position)))
               (lead (if (string-match "\\`[ \t]*" raw)
                         (match-end 0)
                       0))
               (idx (- target start lead)))
          (max 0 (min (length text) idx)))))))

(defun monad--fraction-replace-tail (offset col text)
  "Replace fraction line OFFSET from display column COL with TEXT."
  (goto-char (marker-position monad--fraction-rail))
  (forward-line offset)
  (move-to-column col t)
  (delete-region (point) (line-end-position))
  (insert text))

(defun monad--fraction-update-overlay ()
  "Update the active fraction overlay."
  (when (monad--fraction-active-p)
    (let ((start (monad--fraction-line-start -1))
          (end (save-excursion
                 (goto-char (monad--fraction-line-start 1))
                 (line-end-position))))
      (unless (overlayp monad--fraction-overlay)
        (setq monad--fraction-overlay (make-overlay start end nil nil nil)))
      (move-overlay monad--fraction-overlay start end)
      (overlay-put monad--fraction-overlay 'priority 1001))))

(defun monad--fraction-point-in-block-p ()
  "Return non-nil when point is inside the active fraction block."
  (and (overlayp monad--fraction-overlay)
       (overlay-buffer monad--fraction-overlay)
       (>= (point) (overlay-start monad--fraction-overlay))
       (<= (point) (overlay-end monad--fraction-overlay))))

(defun monad--fraction-goto-field (field index top bottom width col)
  "Move point to FIELD at INDEX after rendering."
  (let* ((text (if (eq field 'top) top bottom))
         (offset (if (eq field 'top) -1 1))
         (left (/ (max 0 (- width (string-width text))) 2))
         (idx (max 0 (min (length text) index))))
    (goto-char (marker-position monad--fraction-rail))
    (forward-line offset)
    (move-to-column (+ col left idx) t)))

(defun monad--fraction-field-bounds (field col)
  "Return the content bounds of FIELD at display column COL."
  (let ((offset (if (eq field 'top) -1 1)))
    (save-excursion
      (goto-char (marker-position monad--fraction-rail))
      (forward-line offset)
      (move-to-column col t)
      (let ((raw-start (point))
            (raw-end (line-end-position)))
        (skip-chars-forward " \t" raw-end)
        (let ((content-start (point)))
          (if (= content-start raw-end)
              (cons raw-start raw-start)
            (goto-char raw-end)
            (skip-chars-backward " \t" content-start)
            (cons content-start (point))))))))

(defun monad--fraction-select-field (field col)
  "Select FIELD contents, or place point at its start when it is empty."
  (let* ((bounds (monad--fraction-field-bounds field col))
         (beg (car bounds))
         (end (cdr bounds)))
    (goto-char beg)
    (when (< beg end)
      (push-mark end t t))))

(defun monad--fraction-one-line-text (text)
  "Return TEXT as one trimmed line."
  (string-trim
   (replace-regexp-in-string "[\n\r]+" " " text)))

(defun monad--fraction-insert-block-at-point (top col target-field)
  "Insert a fraction block at point with TOP and select TARGET-FIELD."
  (let ((width (+ 2 (max 1 (string-width top))))
        rail)
    (insert top
            "\n" (make-string col ?\s)
            (monad--fraction-rule width)
            "\n" (make-string col ?\s))
    (forward-line -1)
    (move-to-column col t)
    (setq rail (line-beginning-position))
    (monad--fraction-start-at rail target-field)
    (monad--fraction-render target-field 0)
    (monad--fraction-select-field target-field col)))

(defun monad--fraction-render (&optional force-field force-index)
  "Render the active fraction, preserving point as much as possible."
  (when (and (monad--fraction-active-p)
             (not monad--fraction-updating))
    (let* ((col (monad--fraction-rule-column-at
                 (marker-position monad--fraction-rail)))
           (current-field (monad--fraction-current-field))
           (field (or force-field current-field))
           (top (monad--fraction-read-line -1 col))
           (bottom (monad--fraction-read-line 1 col))
           (width (+ 2 (max 1 (string-width top) (string-width bottom))))
           (index (or force-index
                      (monad--fraction-point-index
                       field col (if (eq field 'top) top bottom)))))
      (setq monad--fraction-field field)
      (let ((monad--fraction-updating t)
            (inhibit-modification-hooks t))
        (save-excursion
          (monad--fraction-replace-tail -1 col
                                        (monad--fraction-center top width))
          (goto-char (marker-position monad--fraction-rail))
          (move-to-column col t)
          (delete-region (point) (line-end-position))
          (insert (monad--fraction-rule width))
          (monad--fraction-replace-tail 1 col
                                        (monad--fraction-center bottom width)))
        (monad--fraction-update-overlay)
        (monad--fraction-goto-field field index top bottom width col)))))

(defun monad-fraction-finish ()
  "Finish the active fraction editor."
  (interactive)
  (remove-hook 'post-command-hook #'monad--fraction-post-command t)
  (when (overlayp monad--fraction-overlay)
    (delete-overlay monad--fraction-overlay))
  (when (markerp monad--fraction-rail)
    (set-marker monad--fraction-rail nil))
  (setq monad--fraction-overlay nil
        monad--fraction-field 'top
        monad--fraction-updating nil))

(defun monad--fraction-post-command ()
  "Hook: keep the active fraction centered and resized."
  (when (and (monad--fraction-active-p)
             (not monad--fraction-updating))
    (condition-case nil
        (if (monad--fraction-point-in-block-p)
            (monad--fraction-render)
          (monad-fraction-finish))
      (error
       (monad-fraction-finish)))))

(defun monad-fraction-toggle-field ()
  "Switch between the top and bottom fields of the fraction under point."
  (interactive)
  (if (monad--fraction-activate-at-point)
      (let* ((col (monad--fraction-rule-column-at
                   (marker-position monad--fraction-rail)))
             (from (monad--fraction-current-field))
             (target (if (eq from 'top) 'bottom 'top)))
        (setq monad--fraction-field target)
        (monad--fraction-render target 0)
        (setq col (monad--fraction-rule-column-at
                   (marker-position monad--fraction-rail)))
        (monad--fraction-select-field target col))
    (indent-for-tab-command)))

(defun monad-tab ()
  "Indent normally, or switch fields when point is over a fraction."
  (interactive)
  (if (monad--fraction-activate-at-point)
      (monad-fraction-toggle-field)
    (indent-for-tab-command)))

(defun monad--delete-pending-type-arrow-before-newline ()
  "Delete a trailing pending type arrow before inserting a newline."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position))
        delete-beg)
    (when (and (save-excursion
                 (beginning-of-line)
                 (looking-at-p
                  (concat "^[ \t]*\\(?:define\\|method\\)\\s-+"
                          monad--identifier-regexp
                          "\\s-+::")))
               (save-excursion
                 (skip-chars-forward " \t" line-end)
                 (= (point) line-end)))
      (save-excursion
        (goto-char line-end)
        (skip-chars-backward " \t" line-beg)
        (when (and (>= (- (point) line-beg) 2)
                   (string= (buffer-substring-no-properties
                             (- (point) 2)
                             (point))
                            "->"))
          (goto-char (- (point) 2))
          (skip-chars-backward " \t" line-beg)
          (setq delete-beg (point))))
      (when delete-beg
        (delete-region delete-beg line-end)
        (delete-horizontal-space)
        t))))

(defun monad-ret ()
  "Insert a Monad newline.
When point is over the top field of a fraction, move to the bottom
field.  When point is on a guard rail line, insert a new guard branch.
When a Wisp type signature ends with a pending arrow, remove it first."
  (interactive)
  (cond
   ((monad--fraction-activate-at-point)
    (if (eq (monad--fraction-current-field) 'top)
        (let ((col (monad--fraction-rule-column-at
                    (marker-position monad--fraction-rail))))
          (setq monad--fraction-field 'bottom)
          (monad--fraction-render 'bottom 0)
          (monad--fraction-select-field 'bottom col))
      (monad-fraction-finish)
      (monad-newline)))
   ((monad--guard-rail-ret))
   (t
    (monad--delete-pending-type-arrow-before-newline)
    (monad-newline))))

(defun monad-insert-fraction ()
  "Insert or activate a temporary editable Unicode fraction literal.
With an active region, use the region as the numerator.  Without a
region, use the current line when it has text, otherwise create an empty
fraction.  TAB switches fields and selects the destination field."
  (interactive)
  (cond
   ((monad--fraction-activate-at-point)
    (let ((col (monad--fraction-rule-column-at
                (marker-position monad--fraction-rail)))
          (field (monad--fraction-current-field)))
      (monad--fraction-render field 0)
      (monad--fraction-select-field field col)))
   ((use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (top (monad--fraction-one-line-text
                 (buffer-substring-no-properties beg end)))
           (col (save-excursion
                  (goto-char beg)
                  (current-column))))
      (delete-region beg end)
      (goto-char beg)
      (monad--fraction-insert-block-at-point top col 'bottom)))
   (t
    (monad-fraction-finish)
    (let* ((line-beg (line-beginning-position))
           (line-end (line-end-position))
           (line-raw (buffer-substring-no-properties line-beg line-end))
           (line-text (monad--fraction-one-line-text line-raw))
           (has-top (not (string-empty-p line-text)))
           (col (if has-top (current-indentation) (current-column))))
      (delete-region line-beg line-end)
      (goto-char line-beg)
      (indent-to col)
      (monad--fraction-insert-block-at-point
       (if has-top line-text "")
       col
       (if has-top 'bottom 'top))))))

(defun monad--guard-bar-code-position-p (pos line-depth)
  "Return non-nil when POS is a real guard bar at LINE-DEPTH."
  (let ((state (syntax-ppss pos)))
    (and (= (car state) line-depth)
         (not (nth 3 state))
         (not (nth 4 state)))))

(defun monad--guard-bar-before-point ()
  "Return the nearest real guard bar before point on this line."
  (save-excursion
    (let ((end (point))
          (depth (car (syntax-ppss (line-beginning-position))))
          bar)
      (goto-char (line-beginning-position))
      (while (search-forward "|" end t)
        (let ((pos (1- (point))))
          (when (monad--guard-bar-code-position-p pos depth)
            (setq bar pos))))
      bar)))

(defun monad--guard-bar-at-point-p (square-depth)
  "Return non-nil when point is on a real guard bar."
  (and (eq (char-after) ?|)
       (= square-depth 0)
       (let ((prev (char-before))
             (next (char-after (1+ (point)))))
         (and (or (not prev) (memq prev '(?\s ?\t ?\n)))
              (or (not next) (memq next '(?\s ?\t ?\n)))))))

(defun monad--guard-token-end-at-point (square-depth limit)
  "Return the end position of a guard token at point."
  (let ((pos (point)))
    (cond
     ((monad--guard-bar-at-point-p square-depth)
      (1+ pos))
     ((and (= square-depth 0)
           (looking-at-p (regexp-quote monad--guard-rail-entry)))
      (+ pos (length monad--guard-rail-entry)))
     ((and (= square-depth 0)
           (looking-at-p (regexp-quote monad--guard-rail-branch)))
      (+ pos (length monad--guard-rail-branch)))
     ((and (= square-depth 0)
           (looking-at-p (regexp-quote monad--guard-rail-hanging)))
      (+ pos (length monad--guard-rail-hanging)))
     ((and (= square-depth 0)
           (looking-at-p (monad--guard-fallback-regexp)))
      (save-excursion
        (re-search-forward (monad--guard-fallback-regexp) limit t)
        (point))))))

(defun monad--guard-target-left-of-point ()
  "Return the position after the nearest guard token left of point."
  (let ((limit (point))
        (square-depth 0)
        target)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (< (point) limit)
        (let ((ch (char-after))
              token-end)
          (cond
           ((eq ch ?[)
            (setq square-depth (1+ square-depth)))
           ((eq ch ?])
            (setq square-depth (max 0 (1- square-depth))))
           ((setq token-end (monad--guard-token-end-at-point square-depth limit))
            (setq target
                  (save-excursion
                    (goto-char token-end)
                    (skip-chars-forward " \t" limit)
                    (point))))))
        (forward-char 1)))
    (and target (< target limit) target)))

(defun monad--guard-target-right-of-point ()
  "Return the position after the first guard token right of point."
  (let ((origin (point))
        (limit (line-end-position))
        (square-depth 0)
        target)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (< (point) limit)
                  (not target))
        (let ((ch (char-after))
              token-end)
          (cond
           ((eq ch ?[)
            (setq square-depth (1+ square-depth)))
           ((eq ch ?])
            (setq square-depth (max 0 (1- square-depth))))
           ((setq token-end (monad--guard-token-end-at-point square-depth limit))
            (let ((candidate
                   (save-excursion
                     (goto-char token-end)
                     (skip-chars-forward " \t" limit)
                     (point))))
              (when (> candidate origin)
                (setq target candidate))))))
        (forward-char 1)))
    target))

(defun monad-beginning-of-line (&optional arg)
  "Move to the guard expression on the left, or to beginning of line."
  (interactive "^p")
  (let ((n (or arg 1)))
    (if (= n 1)
        (let ((target (monad--guard-target-left-of-point)))
          (if target
              (goto-char target)
            (move-beginning-of-line 1)))
      (move-beginning-of-line n))))

(defun monad-end-of-line (&optional arg)
  "Move to the guard expression on the right, or to end of line."
  (interactive "^p")
  (let ((n (or arg 1)))
    (if (= n 1)
        (let ((target (monad--guard-target-right-of-point)))
          (if target
              (goto-char target)
            (move-end-of-line 1)))
      (move-end-of-line n))))

(defvar-keymap monad-mode-map
  :doc "Keymap for Monad mode."
  :parent lisp-mode-shared-map
  "C-a" #'monad-beginning-of-line
  "C-e" #'monad-end-of-line
  "C-o" #'monad-open-line
  "RET" #'monad-ret
  "S-<return>" #'monad-shift-ret
  "|" #'monad-pipe
  "M-|" #'monad-hanging-pipe
  "TAB" #'monad-tab
  "<tab>" #'monad-tab
  "DEL" #'monad-backward-delete-char-untabify
  "<backspace>" #'monad-backward-delete-char-untabify
  ":" #'monad-colon
  "e" (monad-insert-or "e" 'monad-repl-eval-region)
  "\\" #'monad-insert-lambda
  "C-c C-d" #'monad-show-docstring
  "C-c C-f" #'monad-insert-fraction
  "C-c m" #'monad-insert-matrix
  "C-c C-c" #'monad-compile-and-run
  "C-c t" #'monad-compile-and-run-tests
  "M-g t" #'monad-goto-tests
  "M-g m" #'monad-goto-module
  "M-," #'monad-xref-go-back
  "M-;" #'monad-comment-dwim
  "C-x /" monad-unicode-map)

(define-key monad-mode-map [remap open-line] #'monad-open-line)

;;;###autoload
(define-derived-mode monad-mode prog-mode "Monad"
  "Major mode for editing Monad code.
\\{monad-mode-map}"
  (monad-mode-variables)
  (when (featurep 'monad-repl)
    (monad-repl-setup-keys)))

;; Indentation rules
(put 'lambda 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'layout 'scheme-indent-function 1)
(put 'let 'scheme-indent-function 1)
(put 'let* 'scheme-indent-function 1)
(put 'letrec 'scheme-indent-function 1)
(put 'begin 'scheme-indent-function 0)
(put 'do 'scheme-indent-function 2)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'asm 'scheme-indent-function 0)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mon\\'" . monad-mode))

(provide 'monad-mode)
;;; monad-mode.el ends here
