;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.6
;; Package-Requires: ((emacs "28.1") (rainbow-delimiters "2.1.3"))
;; Keywords: languages
;; URL: https://github.com/laluxx/monad-mode

;; This file is not part of GNU Emacs.
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a major mode for editing Monad programming language.
;; Monad is a Lisp-like language with Scheme-like syntax and advanced
;; type system features.
;;
;; Features:
;; - Syntax highlighting for keywords, strings, characters, and comments
;; - Docstring recognition for functions, variables and lambdas
;; - Underscore (_) shadow face for pattern matching wildcards
;; - Scheme-compatible indentation
;; - Optional keyword highlighting for "->"
;; - Assembly syntax highlighting inside (asm ...) forms
;; - Infix backtick expressions: `fun` highlighted with variable-name face
;;   with optional backtick hiding (see `monad-infix-hide-backticks')
;; - Eldoc integration: hover a function to see its signature; hover a variable
;;   to see its type annotation and/or value — with preserved syntax colors.
;;   Eldoc does NOT trigger when the cursor is immediately after a symbol token.
;;   When calling a function, the current parameter is highlighted with
;;   font-lock-variable-name-face and all others use the default face,
;;   exactly like emacs-lisp-mode's parameter tracking.
;; - rainbow-delimiters enabled by default with depth-2 for () and depth-3 for []
;; - Imenu shows type signature + first line of docstring (both styles supported)

;;; Code:

(require 'lisp-mode)
(require 'cl-lib)
(require 'eldoc)
(require 'rainbow-delimiters)

(defgroup monad nil
  "Major mode for editing Monad code."
  :prefix "monad-"
  :group 'languages)

(defcustom monad-highlight-arrow nil
  "If non-nil, highlight the -> arrow with keyword face."
  :type 'boolean
  :group 'monad)

(defcustom monad-infix-hide-backticks t
  "If non-nil, hide the surrounding backticks of infix function calls.
Only the function name is shown, colored with `monad-infix-face'.
The backticks become visible again when point is inside the expression."
  :type 'boolean
  :group 'monad
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (derived-mode-p 'monad-mode)
               (font-lock-flush))))))

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
    ;; Backtick: symbol constituent so it doesn't interfere with
    ;; Lisp quasiquote logic and electric-pair can handle it cleanly
    (modify-syntax-entry ?` "_   " st)
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
  '("define" "lambda" "match" "layout" "data" "deriving"
    "let" "letrec" "let*" "if" "cond" "case" "else"
    "and" "or" "not" "quote" "unquote" "quasiquote"
    "begin" "do" "when" "unless" "error" "instance" "asm"
    "module" "import" "qualified" "as" "hiding" )
  "Keywords for the Monad programming language.")

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Imenu — flat index with cached docstring annotations
;;;; ─────────────────────────────────────────────────────────────────────────

(defvar-local monad--docstring-cache nil
  "Cache for definition docstrings.  Hash table mapping name strings to
the first line of their docstring (or nil when there is none).")

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
        (let ((is-function (eq (char-after) ?\())
              (is-typed-var (eq (char-after) ?\[)))
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
      (while (re-search-forward
              "^(define[ \t\n]+\\(?:(\\|\\[?\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
              nil t)
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
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^(define[ \t\n]+\\(?:(\\|\\[?\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
              nil t)
        (let* ((name (match-string-no-properties 1))
               (type (monad--imenu-type-annotation name))
               (tlen (if type (length type) 0)))
          (when (> (length name) max-name) (setq max-name (length name)))
          (when (> tlen max-type)          (setq max-type tlen))
          (push (cons name (copy-marker (match-beginning 1))) index))))
    (setq monad--imenu-max-name-len max-name
          monad--imenu-max-type-len max-type)
    (nreverse index)))

(defun monad--imenu-type-annotation (name)
  "Return a raw (unpropertized) type/signature string for NAME, or nil."
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
            (match-string-no-properties 1)))))))

(defun monad-imenu-annotate (cand)
  "Annotation function for Monad imenu candidates."
  (let* ((type-raw  (monad--imenu-type-annotation cand))
         (doc       (monad--get-cached-docstring cand))
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
  "Match character literals 'x' up to LIMIT."
  (catch 'found
    (while (re-search-forward "'\\(.\\)'" limit t)
      (let ((matched-char (match-string 1)))
        (when (= (length matched-char) 1)
          (throw 'found t))))
    nil))

;;;; Infix backtick support

(defconst monad-infix-regexp "`\\([^`\n]+\\)`"
  "Regexp matching infix backtick expressions.")

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
  "Refresh backtick visibility around point."
  (when (and (derived-mode-p 'monad-mode)
             (not monad-infix--updating))
    (let ((monad-infix--updating t)
          (scan-start (save-excursion (forward-line -3) (point)))
          (scan-end   (save-excursion (forward-line  3) (point))))
      (with-silent-modifications
        (save-excursion
          (goto-char scan-start)
          (while (re-search-forward monad-infix-regexp scan-end t)
            (let ((mstart (match-beginning 0))
                  (mend   (match-end 0)))
              (when monad-infix-hide-backticks
                (put-text-property mstart (1+ mstart) 'invisible t)
                (put-text-property (1- mend) mend     'invisible t)))))))))

(defun monad-infix-schedule-refresh ()
  "Schedule a backtick visibility refresh via idle timer."
  (when (derived-mode-p 'monad-mode)
    (when (timerp monad-infix--idle-timer)
      (cancel-timer monad-infix--idle-timer))
    (setq monad-infix--idle-timer
          (run-with-idle-timer 0.05 nil #'monad-infix--refresh-visibility))))

;;;; Electric pair for backtick

(defun monad--setup-electric-pair ()
  "Setup electric pairing of backticks in monad-mode."
  (when (bound-and-true-p electric-pair-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs '((?` . ?`))))))

;; Assembly syntax highlighting support

(defun monad-syntax-propertize (start end)
  "Apply syntax properties from START to END."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
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
  "Extend font-lock region to cover complete asm forms."
  (let ((changed nil))
    (save-excursion
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
                  changed t)))))
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
                       5)))
              (error 7)))))
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
  (if (monad-in-asm-form-p)
      (monad-asm-indent-line)
    (lisp-indent-line)))

(defun monad-post-self-insert ()
  "Handle post-insertion fontification in asm blocks."
  (when (and (monad-in-asm-form-p)
             (eq (char-before) ?:))
    (save-excursion
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (font-lock-flush line-start line-end)
        (font-lock-fontify-region line-start line-end)))))

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
    ;; ARM
    "swi")
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

(defun monad-font-lock-keywords ()
  "Return font-lock keywords for Monad mode."
  (append
   (list
    (list 'monad-infix-matcher
          '(0 'monad-infix-face t)
          '(1 'monad-infix-face t))
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
    '("(\\(define\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))
    '("(\\(define\\)\\s-+\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))
   (let ((asm-keywords (monad-asm-get-font-lock-keywords)))
     (mapcar (lambda (keyword)
               (let* ((matcher (if (consp keyword) (car keyword) keyword))
                      (highlighter (if (consp keyword) (cdr keyword)
                                     font-lock-keyword-face))
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

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Eldoc integration
;;;;
;;;; - No eldoc when the cursor is immediately after (touching) a symbol.
;;;;   "After a symbol" means: the char before point is a word/symbol
;;;;   constituent AND point is NOT preceded by whitespace or an open paren.
;;;;   In practice: `x|' → silent; `(f |' → show f's signature.
;;;;
;;;; - When inside a function call `(fn arg1 arg2 ...)' the current parameter
;;;;   (zero-based index among the arguments) is highlighted with
;;;;   font-lock-variable-name-face; all other parameter names use the
;;;;   default face — matching emacs-lisp-mode behaviour.
;;;; ─────────────────────────────────────────────────────────────────────────

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
Matches `(define [name :: Type] value)' -- point is anywhere inside the [...].
Returns non-nil (the bracket start position) when the condition holds, nil otherwise."
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

;;;; ── Parameter extraction ─────────────────────────────────────────────────

(defun monad--parse-param-names (header-str)
  "Parse parameter names from a function HEADER-STR like \"(fn [x :: T] -> [y :: T] -> R)\".
Returns a list of parameter name strings in order.
Only names inside [name :: ...] blocks are collected; the return type
and the function name itself are ignored."
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
            (error (error "skip-fn-failed")))
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

;;;; ── Propertize signature with active-parameter highlighting ──────────────

(defun monad--propertize-signature-with-active-param (sig active-idx)
  "Return SIG propertized with ACTIVE-IDX parameter highlighted.
ACTIVE-IDX is zero-based.  Pass -1 to put ALL param names in default face
(used when hovering the function name itself — no slot is being filled)."
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

;;;; ── Context detection: are we in a function call? ───────────────────────

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
            (let ((fn-name (match-string-no-properties 0))
                  (fn-end  (match-end 0)))
              ;; Make sure point (before up-list) was PAST the function name
              ;; i.e. we are in the argument portion of the form, not on the fn itself
              fn-name)))
      (error nil))))

;;;; ── Main eldoc function ──────────────────────────────────────────────────

(defun monad--extract-function-header (name)
  "Return the header sexp string for function NAME, or nil."
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
          (error nil))))))

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
  "Eldoc documentation function for Monad mode."
  (when-let* ((doc (monad--eldoc-get-doc)))
    (funcall callback doc)
    t))

;;;; ─────────────────────────────────────────────────────────────────────────

(defun monad-mode-variables ()
  "Set up variables for Monad mode."
  (set-syntax-table monad-mode-syntax-table)
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
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-annotators
                 '(imenu monad-imenu-annotate builtin none)))
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
  (add-hook 'xref-backend-functions #'monad-xref-backend nil t)
  (add-hook 'completion-at-point-functions #'monad-completion-at-point nil t)
  (add-hook 'post-self-insert-hook #'monad-post-self-insert nil t)
  (add-hook 'post-command-hook #'monad-infix-schedule-refresh nil t)
  (monad--setup-electric-pair)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1)
  (setq-local eldoc-documentation-functions '(monad-eldoc-function))
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-default))

;;;; Module system

(defun monad--current-file-dir ()
  "Return the directory of the current buffer's file, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun monad--module-file (module-name)
  "Return the path to MODULE-NAME's .mon file relative to the current buffer."
  (when-let* ((dir (monad--current-file-dir)))
    (expand-file-name (concat module-name ".mon") dir)))

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
      (while (re-search-forward
              "^(define\\s-+\\(?:(\\(\\(?:\\sw\\|\\s_\\)+\\)\\|\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)\\)"
              nil t)
        (let* ((fn-name  (match-string-no-properties 1))
               (var-name (match-string-no-properties 2))
               (name     (or fn-name var-name))
               (kind     (if fn-name 'function 'variable)))
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
  "Return t if START is the position of the first token on its line in an asm block."
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

(with-eval-after-load 'nerd-icons-corfu
  (add-to-list 'nerd-icons-corfu-mapping
               '(monad-asm :fn (lambda (_cand)
                                 (concat (nerd-icons-sucicon "nf-seti-asm") " "))
                           :face font-lock-keyword-face)))

(defun monad-completion-at-point ()
  "Completion-at-point for Monad mode."
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
              (_          ""))))))

;;;; Xref backend

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
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^(define\\s-+\\(?:"
                        "(\\(" (regexp-quote symbol) "\\)\\>"
                        "\\|\\[\\(" (regexp-quote symbol) "\\)\\s-+::"
                        "\\|\\(" (regexp-quote symbol) "\\)\\>\\)")
                nil t)
          (let ((pos (or (match-beginning 1)
                         (match-beginning 2)
                         (match-beginning 3))))
            (push (xref-make symbol (xref-make-buffer-location buf pos))
                  locs)))))
    (nreverse locs)))

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
  "Return an xref location jumping to the module name inside its declaration."
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

(defvar-keymap monad-mode-map
  :doc "Keymap for Monad mode."
  :parent lisp-mode-shared-map
  ":" #'monad-colon
  "C-c C-d" #'monad-show-docstring)

;;;###autoload
(define-derived-mode monad-mode prog-mode "Monad"
  "Major mode for editing Monad code.
\\{monad-mode-map}"
  (monad-mode-variables))

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
