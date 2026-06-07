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

(defcustom monad-highlight-arrow nil
  "If non-nil, highlight the -> arrow with keyword face."
  :type 'boolean
  :group 'monad)

(defface monad-underscore-face
  '((t :inherit shadow))
  "Face for underscore wildcard pattern."
  :group 'monad)

(defface monad-infix-face
  '((t :inherit font-lock-variable-name-face))
  "Face for infix backtick expressions like `fun`."
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
  '("define" "method" "variable" "def" "lambda" "match" "with" "layout" "type" "data" "deriving"
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
  "Clear the buffer-local docstring cache and column-alignment records."
  (setq monad--docstring-cache nil
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




(defun monad-post-self-insert ()
  "Handle post-insertion actions: asm fontification."
  (when (and (monad-in-asm-form-p)
             (eq (char-before) ?:))
    (save-excursion
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (font-lock-flush line-start line-end)
        (font-lock-fontify-region line-start line-end))))
  (let ((in-module
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
               (error nil))))))
    (unless in-module
      (monad-insert-type-annotation)
      (monad-insert-arrow-annotation))))

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

(defun monad-font-lock-keywords ()
  "Return font-lock keywords for Wisp mode."
  (append
   (list
    '(monad-block-comment-matcher (0 (progn
                                       (put-text-property (match-beginning 0)
                                                          (match-end 0)
                                                          'font-lock-multiline t)
                                       font-lock-comment-face)
                                   t))
    '(monad-refinement-type-docstring-matcher (0 font-lock-doc-face t))
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
    '(":\\sw+" . font-lock-builtin-face)
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
   (when monad-highlight-arrow
     '(("->" . font-lock-keyword-face)))))

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

(defvar-keymap monad-mode-map
  :doc "Keymap for Monad mode."
  :parent lisp-mode-shared-map
  "RET" #'monad-newline
  "DEL" #'monad-backward-delete-char-untabify
  "<backspace>" #'monad-backward-delete-char-untabify
  ":" #'monad-colon
  "e" (monad-insert-or "e" 'monad-repl-eval-region)
  "\\" #'monad-insert-lambda
  "C-c C-d" #'monad-show-docstring
  "C-c m" #'monad-insert-matrix
  "C-c C-c" #'monad-compile-and-run
  "C-c t" #'monad-compile-and-run-tests
  "M-g t" #'monad-goto-tests
  "M-g m" #'monad-goto-module
  "M-," #'monad-xref-go-back
  "M-;" #'monad-comment-dwim
  "C-x /" monad-unicode-map)

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
