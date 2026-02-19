;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.5
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
;; - rainbow-delimiters enabled by default with depth-2 for () and depth-3 for []

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

(defun monad--imenu-type-annotation (name)
  "Return a short type/signature string for NAME, or nil.
For functions returns the header sexp, e.g. \"(add [a :: Int] -> Int)\".
For typed variables returns the annotation, e.g. \"[x :: Int]\"."
  (save-excursion
    (goto-char (point-min))
    (let ((fn-rx (concat "^(define[ \t\n]+(\\("
                         (regexp-quote name) "\\)\\b")))
      (if (re-search-forward fn-rx nil t)
          (progn
            (goto-char (match-beginning 0))
            (condition-case nil
                (progn
                  (down-list 1)
                  (forward-sexp 1)        ; skip "define"
                  (skip-chars-forward " \t\n")
                  (let ((hdr-start (point)))
                    (forward-sexp 1)      ; the whole (name ...) header
                    (buffer-substring-no-properties hdr-start (point))))
              (error nil)))
        (goto-char (point-min))
        (let ((tv-rx (concat "^(define[ \t\n]+\\(\\["
                             (regexp-quote name)
                             "[ \t]*::[^]\n]+\\]\\)")))
          (when (re-search-forward tv-rx nil t)
            (match-string-no-properties 1)))))))

(defun monad--imenu-build-index ()
  "Build the imenu index for Monad mode.
Candidate strings carry an `imenu-annotation' text property with
the type signature so that completion UIs that read this property
\(e.g. consult-imenu) can display it.  The index is split into
\"Functions\" and \"Variables\" sub-menus."
  (let (functions variables)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^(define[ \t\n]+\\(?:(\\|\\[?\\)\\(\\(?:\\sw\\|\\s_\\)+\\)"
              nil t)
        (let* ((name    (match-string-no-properties 1))
               (pos     (copy-marker (match-beginning 1)))
               (fn-p    (save-excursion
                          (goto-char (match-beginning 0))
                          (looking-at "(define[ \t\n]*(")))
               (ann     (monad--imenu-type-annotation name))
               (display (if ann
                            (propertize name 'imenu-annotation (concat "  " ann))
                          name)))
          (if fn-p
              (push (cons display pos) functions)
            (push (cons display pos) variables)))))
    (let (result)
      (when variables
        (push (cons "Variables" (nreverse variables)) result))
      (when functions
        (push (cons "Functions" (nreverse functions)) result))
      result)))

(defun monad--marginalia-annotate-imenu (cand)
  "Annotate imenu CAND for Monad mode, or fall back to marginalia's default.
In `monad-mode' buffers, reads the `imenu-annotation' text property
stored by `monad--imenu-build-index' and formats it with `marginalia-type'
face.  In all other modes defers to `marginalia-annotate-imenu'."
  (if (derived-mode-p 'monad-mode)
      (when-let* ((ann (get-text-property 0 'imenu-annotation cand)))
        (marginalia--fields
         (ann :truncate 1.0 :face 'marginalia-type)))
    (marginalia-annotate-imenu cand)))

(with-eval-after-load 'marginalia
  ;; Replace the imenu annotator in `marginalia-annotators' so that
  ;; our function runs for monad-mode buffers and delegates to
  ;; marginalia's built-in annotator for all other modes.
  (when-let* ((entry (assq 'imenu marginalia-annotators)))
    (setcar (cdr entry) #'monad--marginalia-annotate-imenu)))

(defun monad-char-literal-matcher (limit)
  "Match character literals 'x' up to LIMIT.
Only matches single characters between single quotes."
  (catch 'found
    (while (re-search-forward "'\\(.\\)'" limit t)
      (let ((matched-char (match-string 1)))
        (when (= (length matched-char) 1)
          (throw 'found t))))
    nil))

;;;; Infix backtick support

(defconst monad-infix-regexp "`\\([^`\n]+\\)`"
  "Regexp matching infix backtick expressions.
Group 1 is the function name between the backticks.")

(defun monad-infix-matcher (limit)
  "Font-lock matcher for infix backtick expressions up to LIMIT.
Applies initial backtick invisibility when `monad-infix-hide-backticks' is set."
  (let (found)
    (while (and (not found)
                (re-search-forward monad-infix-regexp limit t))
      (unless (nth 3 (syntax-ppss (match-beginning 0)))
        (when monad-infix-hide-backticks
          (put-text-property (match-beginning 0) (1+ (match-beginning 0)) 'invisible t)
          (put-text-property (1- (match-end 0)) (match-end 0)            'invisible t))
        (setq found t)))
    found))

(defvar-local monad-infix--idle-timer nil
  "Idle timer for refreshing infix backtick visibility.")

(defvar-local monad-infix--updating nil
  "Guard flag to prevent recursive updates.")

(defun monad-infix--refresh-visibility ()
  "Refresh backtick visibility around point.
Scans only a few lines around point; safe to call from a timer."
  (when (and (derived-mode-p 'monad-mode)
             (not monad-infix--updating))
    (let ((monad-infix--updating t)
          (pt         (point))
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
  "Setup electric pairing of backticks in monad-mode.
Adds `` (` . `) `` to `electric-pair-pairs' so typing one backtick
inserts a matching one, enabling `fun` infix syntax."
  (when (bound-and-true-p electric-pair-mode)
    (setq-local electric-pair-pairs
                (append electric-pair-pairs '((?` . ?`))))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs '((?` . ?`))))))

;; Assembly syntax highlighting support

(defun monad-syntax-propertize (start end)
  "Apply syntax properties from START to END.
Marks asm form regions and keeps semicolon comments working."
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
  "Indent the current line in an asm block.
Labels (word followed by :) are flushed to column 0.
All other lines align to the column of the first instruction token
after the `asm' keyword on the opening line, giving:
  (asm rdtsc
       shl 32   %rdx
       or  %rdx %rax))"
  (interactive)
  (let* ((savep (point))
         (target-col
          (save-excursion
            ;; Walk back to find the `(asm' opening
            (condition-case nil
                (progn
                  (re-search-backward "(\\s-*asm\\_>" nil t)
                  (goto-char (match-end 0))   ; past `asm'
                  (skip-chars-forward " \t")  ; skip spaces after `asm'
                  ;; If there is a first instruction on the same line, align to it
                  (if (not (eolp))
                      (current-column)
                    ;; Otherwise fall back to (asm-keyword-col + 5)
                    (+ (progn (goto-char (match-beginning 0))
                              (current-column))
                       5)))
              (error 7)))))  ; safe fallback column
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
  "Indent current line.
Uses asm-specific indentation inside asm blocks, lisp indentation outside."
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
  "Determine face for syntax at STATE.
Disables comment/string highlighting inside asm forms."
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
  "Assembly instructions — shared by font-lock highlighting and completion.")

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
  "Assembly registers — shared by font-lock highlighting and completion.")

(defvar monad-asm-font-lock-keywords-cache nil
  "Cached assembly font-lock keywords for performance.")

(defun monad-asm-get-font-lock-keywords ()
  "Get assembly font-lock keywords, reusing `monad-asm-instructions'."
  (or monad-asm-font-lock-keywords-cache
      (setq monad-asm-font-lock-keywords-cache
            (if (and (boundp 'asm-font-lock-keywords)
                     (not (eq asm-font-lock-keywords 'unbound)))
                asm-font-lock-keywords
              (list
               '(";.*$" . font-lock-comment-face)
               '("^\\s-*\\([.a-zA-Z_][a-zA-Z0-9_]*\\):" 1 font-lock-function-name-face)
               ;; Instructions — reuse the shared constant
               (cons (regexp-opt monad-asm-instructions 'symbols)
                     'font-lock-keyword-face)
               ;; Registers — reuse the shared constant (strip % for the regexp)
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
    ;; Infix backtick expressions — must come first so they take priority.
    (list 'monad-infix-matcher
          '(0 'monad-infix-face t)
          '(1 'monad-infix-face t))
    ;; Asm keyword itself
    '("(\\s-*\\(asm\\)\\_>" 1 font-lock-keyword-face)
    ;; Keywords (but not 'asm' since it's handled above)
    (cons (regexp-opt (remove "asm" monad-keywords) 'symbols)
          'font-lock-keyword-face)
    ;; Type annotation operator ::
    '("::" . font-lock-builtin-face)
    ;; Special :keywords (symbols starting with :)
    '(":\\sw+" . font-lock-builtin-face)
    ;; Character literals 'x' (exactly one character)
    '(monad-char-literal-matcher . font-lock-string-face)
    ;; Underscore wildcard
    '("\\<_\\>" . 'shadow)
    ;; #+ by itself
    '("#\\+\\>" . 'shadow)
    ;; #+ with feature name
    '("\\(#\\+\\)\\(\\sw+\\)"
      (1 'shadow t)
      (2 font-lock-function-name-face t))
    ;; #- with feature name
    '("#-\\sw+" . 'shadow)
    ;; #--- style separators
    '("#-+\\>" . 'shadow)
    ;; Function definitions: (define (name ...) ...)
    '("(\\(define\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))
    ;; Variable definitions: (define name ...) and (define [name :: Type] ...)
    '("(\\(define\\)\\s-+\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))
   ;; Assembly syntax inside asm forms
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
   ;; Conditionally add arrow highlighting
   (when monad-highlight-arrow
     '(("->" . font-lock-keyword-face)))))

;; Set up docstring detection like Scheme does
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
;;;; Cases:
;;;;   1. Function: (define (name [p :: T] -> [q :: T] -> RetT) ...)
;;;;      → minibuffer shows: (name [p :: T] -> [q :: T] -> RetT)
;;;;   2. Typed variable: (define [name :: T] value)
;;;;      → minibuffer shows: [name :: T] value
;;;;   3. Plain variable: (define name value)
;;;;      → minibuffer shows: value
;;;; ─────────────────────────────────────────────────────────────────────────

(defun monad--depth-face (depth)
  "Return the rainbow-delimiters face for DEPTH.
Outermost delimiter (depth 1) maps to `rainbow-delimiters-depth-2-face',
next level to depth-3, etc., so the signature colors align with the
buffer's own rainbow-delimiters coloring convention."
  (intern (format "rainbow-delimiters-depth-%d-face"
                  (max 1 (min 9 (1+ depth))))))

(defun monad--propertize-signature (sig)
  "Return SIG as a propertized string with syntax colours.
Delimiter faces are assigned by actual nesting depth (depth 1 = outermost),
using `rainbow-delimiters-depth-N-face'.  Both () and [] contribute to the
depth counter so the correct face is always nesting-relative.

Other faces:
  ::  operator       → `font-lock-builtin-face'
  Function name      → `font-lock-function-name-face'
  Parameter names    → `font-lock-variable-name-face'"
  (let ((s (copy-sequence sig))
        (depth 0))
    ;; ── Delimiters: nesting-depth-based faces ────────────────────────────
    (dotimes (i (length s))
      (let ((ch (aref s i)))
        (cond
         ((memq ch '(?\( ?\[))
          (setq depth (1+ depth))
          (add-face-text-property i (1+ i) (monad--depth-face depth) nil s))
         ((memq ch '(?\) ?\]))
          (add-face-text-property i (1+ i) (monad--depth-face depth) nil s)
          (setq depth (max 0 (1- depth)))))))
    ;; ── :: operator ─────────────────────────────────────────────────────
    (let ((i 0))
      (while (string-match "::" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'font-lock-builtin-face nil s)
        (setq i (match-end 0))))
    ;; ── Function name: first word after leading "(" ──────────────────────
    (when (string-match "^(\\([A-Za-z_][A-Za-z0-9_'!?$%&*/+<=>.^~-]*\\)" s)
      (add-face-text-property (match-beginning 1) (match-end 1)
                              'font-lock-function-name-face nil s))
    ;; ── Variable name at top-level "[name :: ..." ────────────────────────
    (when (string-match "^\\[\\([A-Za-z_][A-Za-z0-9_'!?$%&*/+<=>.^~-]*\\)[ \t]*::" s)
      (add-face-text-property (match-beginning 1) (match-end 1)
                              'font-lock-variable-name-face nil s))
    ;; ── Parameter names inside [...] blocks ─────────────────────────────
    (let ((i 0))
      (while (string-match "\\[\\([a-z_][A-Za-z0-9_']*\\)[ \t]*::" s i)
        (add-face-text-property (match-beginning 1) (match-end 1)
                                'font-lock-variable-name-face nil s)
        (setq i (match-end 0))))
    ;; ── String literals "..." → font-lock-string-face ───────────────────
    ;; Simple scan: find opening ", scan to closing " respecting \".
    (let ((i 0) (len (length s)))
      (while (< i len)
        (if (eq (aref s i) ?\")
            (let ((start i))
              (setq i (1+ i))
              (while (and (< i len)
                          (not (and (eq (aref s i) ?\")
                                    (not (eq (aref s (1- i)) ?\\)))))
                (setq i (1+ i)))
              (when (< i len) (setq i (1+ i))) ; consume closing "
              (add-face-text-property start i 'font-lock-string-face nil s))
          (setq i (1+ i)))))
    ;; ── Character literals 'x' → font-lock-string-face ──────────────────
    (let ((i 0))
      (while (string-match "'\\(.\\)'" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'font-lock-string-face nil s)
        (setq i (match-end 0))))
    s))

(defun monad--extract-function-header (name)
  "Return \"(NAME [p :: T] -> ... -> RetT)\" for function NAME, or nil.
Searches for `(define (NAME ...)' and extracts the header sexp."
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
  "Return eldoc string for a variable NAME, or nil.

Handles these forms (value is always the first sexp after the name/annotation):
  (define [name :: Type] value)         -> \"[name :: Type] value\"
  (define [name :: Type] value \"doc\") -> \"[name :: Type] value\"
  (define name value)                   -> \"value\"
  (define name value \"doc\")           -> \"value\"

The docstring, when present, always comes AFTER the value sexp, so we
simply extract the first sexp following the name as the value and stop.
This works correctly for strings, chars, numbers, and compound expressions."
  (save-excursion
    (goto-char (point-min))
    ;; \u2500\u2500 Typed form: (define [name :: Type] value) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    (let ((typed-rx (concat "^(define[ \t\n]+\\(\\["
                            (regexp-quote name)
                            "[ \t]*::[^]\n]+\\]\\)")))
      (if (re-search-forward typed-rx nil t)
          (let ((annotation (match-string-no-properties 1)))
            (skip-chars-forward " \t\n")
            (condition-case nil
                (let ((val-start (point)))
                  (forward-sexp 1)
                  ;; Always show the value -- it is never a docstring in typed forms.
                  (concat annotation " "
                          (string-trim
                           (buffer-substring-no-properties val-start (point)))))
              (error nil)))
        ;; \u2500\u2500 Plain form: (define name value) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
        (goto-char (point-min))
        (let ((plain-rx (concat "^(define[ \t\n]+"
                                (regexp-quote name)
                                "\\b")))
          (when (re-search-forward plain-rx nil t)
            ;; Skip function definitions: (define (name ...)  ...)
            (unless (string-match-p "(define[ \t\n]+(" (match-string 0))
              (skip-chars-forward " \t\n")
              (condition-case nil
                  (let ((val-start (point)))
                    (forward-sexp 1)
                    ;; The first sexp is always the value; docstrings follow after.
                    (string-trim
                     (buffer-substring-no-properties val-start (point))))
                (error nil)))))))))


(defun monad--extract-enclosing-param-type (name)
  "Return \"[NAME :: Type]\" if NAME is a typed parameter of the enclosing function.
Searches the header `(define (fn ... [NAME :: Type] ...))' of the function
whose body contains point.  Returns nil if NAME is not found as a parameter
or if it has no type annotation."
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-defun)
          ;; Must be a function define: (define (fn ...)  ...)
          (when (looking-at "(define[ \t\n]*(")
            (down-list 1)       ; step inside outer (
            (forward-sexp 1)    ; skip "define"
            (skip-chars-forward " \t\n")
            ;; Now at the header sexp: (fn [x :: T] -> ...)
            (when (eq (char-after) ?\()
              (let ((hdr-start (point)))
                (forward-sexp 1)
                (let ((hdr-end (point))
                      (hdr-str (buffer-substring-no-properties
                                (1+ hdr-start)  ; skip opening (
                                (1- (point))))) ; skip closing )
                  ;; Scan hdr-str for [NAME :: Type]
                  (with-temp-buffer
                    (insert hdr-str)
                    (goto-char (point-min))
                    (let ((pat (concat "\\[" (regexp-quote name)
                                       "[ \t]*::[^]\n]+\\]")))
                      (when (re-search-forward pat nil t)
                        (match-string-no-properties 0)))))))))
      (error nil))))

(defun monad--eldoc-from-module-file (file bare-name)
  "Return an eldoc string for BARE-NAME found in FILE, or nil.
Searches FILE for a function header or variable definition matching BARE-NAME."
  (when (and file (file-readable-p file))
    (with-current-buffer (find-file-noselect file)
      (or (monad--extract-function-header bare-name)
          (monad--extract-variable-info   bare-name)))))

(defun monad--eldoc-from-imports (name)
  "Return a raw (unpropertized) eldoc string for NAME found in any imported module.
Handles both bare names and qualified `Module.name' forms."
  (let* ((imports (monad--parse-imports))
         ;; Split qualified name, e.g. "Math.add" -> mod="Math", bare="add"
         (qualified-p (string-match "^\\([A-Z][^.]*\\)\\.\\(.+\\)$" name))
         (mod-name    (and qualified-p (match-string 1 name)))
         (bare        (if qualified-p (match-string 2 name) name))
         result)
    (if qualified-p
        ;; Qualified: look only in that specific module
        (let ((file (monad--module-file mod-name)))
          (setq result (monad--eldoc-from-module-file file bare)))
      ;; Unqualified: search all non-qualified imports in order
      (cl-dolist (entry imports)
        (let* ((plist     (cdr entry))
               (qualified (plist-get plist :qualified))
               (file      (plist-get plist :file)))
          (unless qualified
            (when-let* ((doc (monad--eldoc-from-module-file file bare)))
              (setq result doc)
              (cl-return))))))
    result))

(defun monad--eldoc-get-doc ()
  "Return a propertized eldoc string for the symbol at point, or nil.
Priority:
  1. NAME is a typed parameter of the enclosing function  → [NAME :: Type]
  2. NAME names a function definition in this buffer      → (NAME header)
  3. NAME names a typed/plain variable in this buffer     → annotation + value
  4. NAME (bare or Module.name) found in an imported file → header or value"
  (when-let* ((sym  (symbol-at-point))
              (name (symbol-name sym)))
    (or
     ;; 1. Parameter of enclosing function (highest priority)
     (when-let* ((ann (monad--extract-enclosing-param-type name)))
       (monad--propertize-signature ann))
     ;; 2. Function header in this buffer
     (when-let* ((hdr (monad--extract-function-header name)))
       (monad--propertize-signature hdr))
     ;; 3. Variable in this buffer
     (when-let* ((info (monad--extract-variable-info name)))
       (monad--propertize-signature info))
     ;; 4. Imported module symbol (bare or qualified)
     (when-let* ((info (monad--eldoc-from-imports name)))
       (monad--propertize-signature info)))))

(defun monad-eldoc-function (callback &rest _ignored)
  "Eldoc documentation function for Monad mode.
Registered on `eldoc-documentation-functions' (Emacs 28+).
Calls CALLBACK with the propertized signature string and returns t,
or returns nil when there is nothing to show.
We never return the doc string itself — that would cause eldoc's
dispatch loop to display it a second time via the return value."
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
  ;; Set up syntax-propertize
  (setq-local syntax-propertize-function #'monad-syntax-propertize)
  ;; Set up font-lock extension for asm forms
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
  ;; Xref backend
  (add-hook 'xref-backend-functions #'monad-xref-backend nil t)
  ;; Completion-at-point (works with Corfu, company, built-in)
  (add-hook 'completion-at-point-functions #'monad-completion-at-point nil t)
  ;; Asm fontification on self-insert
  (add-hook 'post-self-insert-hook #'monad-post-self-insert nil t)
  ;; Infix backtick visibility
  (add-hook 'post-command-hook #'monad-infix-schedule-refresh nil t)
  ;; Electric pair for backtick
  (monad--setup-electric-pair)
  ;; ── Rainbow delimiters ─────────────────────────────────────────────────
  ;; Enable for the buffer so delimiters in source are colored by nesting depth.
  ;; The eldoc propertize function uses the same rainbow-delimiters-depth-N-face
  ;; faces so minibuffer signatures match the editor colors.
  (rainbow-delimiters-mode 1)
  ;; ── Eldoc ──────────────────────────────────────────────────────────────
  ;; Turn eldoc on first (prog-mode may or may not have done so already),
  ;; then *replace* the whole documentation-functions list so that any
  ;; backend eldoc-mode itself appended (e.g. lisp-eldoc-function,
  ;; elisp-eldoc-documentation-function) is evicted.  Doing this after
  ;; `eldoc-mode 1' is the only reliable order because eldoc populates the
  ;; list during its activation hook.
  (eldoc-mode 1)
  (setq-local eldoc-documentation-functions '(monad-eldoc-function))
  ;; Use the simple "eager" strategy: call every function, show first result.
  ;; This prevents eldoc from merging multiple results into one message.
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-default))

;;;; Module system — parsing imports and collecting symbols

(defun monad--current-file-dir ()
  "Return the directory of the current buffer's file, or nil."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun monad--module-file (module-name)
  "Return the path to MODULE-NAME's .mon file relative to the current buffer.
E.g. \"Math\" -> \"/path/to/current/Math.mon\"."
  (when-let* ((dir (monad--current-file-dir)))
    (expand-file-name (concat module-name ".mon") dir)))

(defun monad--parse-exports (file)
  "Return the list of exported symbol names from FILE as propertized strings."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward
           "^(module\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+\\[\\([^]]*\\)\\]" nil t)
          (mapcar (lambda (s) (propertize s 'company-kind 'function))
                  (split-string (match-string-no-properties 1) "[ \t\n]+" t))
        (monad--collect-defines)))))

(defun monad--collect-defines ()
  "Collect all top-level define names in the current buffer.
Returns propertized strings with `company-kind' set to
`function' or `variable'."
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
  "Return completion candidates derived from IMPORTS with `company-kind' properties."
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
  "Return asm completion candidates for use inside (asm ...) blocks."
  (delete-dups
   (append
    (mapcar (lambda (i) (propertize i 'company-kind 'monad-asm))
            monad-asm-instructions)
    (mapcar (lambda (r) (propertize r 'company-kind 'variable))
            monad-asm-registers)
    (monad--collect-defines))))

(defun monad--keyword-candidates ()
  "Return monad keywords as propertized candidates with `company-kind' = `keyword'."
  (mapcar (lambda (kw) (propertize kw 'company-kind 'keyword))
          monad-keywords))

(defun monad--all-completions ()
  "Return all Monad completion candidates (not for asm context)."
  (delete-dups
   (append (monad--keyword-candidates)
           (monad--collect-defines)
           (monad--import-completions (monad--parse-imports)))))

;;;; Completion-at-point (Corfu / built-in)

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
            ;; Like elisp-mode's " <f>" annotation, show a short type
            ;; signature for functions and typed variables, falling back
            ;; to a kind tag for keywords and asm instructions.
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
  "Return the identifier at point, including qualified Module.sym forms."
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
  "Search for SYMBOL as a typed parameter in the enclosing `(define (fn ...))'.
Returns an xref location pointing to the symbol inside the header, or nil.

The header has the form: (fn [a :: T] -> [b :: T] -> RetT)
We extract the raw header string between the outer parens, scan it with a
regexp for `[SYMBOL :: ...' occurrences, then map the match offset back to
a buffer position.  This handles any number of parameters separated by ->."
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-defun)
          ;; Must be a function define: (define (fn ...)  ...)
          (when (looking-at "(define[ \t\n]*(")
            (down-list 1)         ; inside outer (
            (forward-sexp 1)      ; skip "define"
            (skip-chars-forward " \t\n")
            ;; Now at the opening ( of the header sexp
            (when (eq (char-after) ?\()
              (let* ((hdr-open  (point))
                     (hdr-open1 (1+ hdr-open))
                     (_dummy    (forward-sexp 1))
                     (hdr-close (1- (point)))
                     (hdr-str   (buffer-substring-no-properties
                                 hdr-open1 hdr-close))
                     ;; Regexp: [SYMBOL whitespace ::
                     (pat       (concat "\\[" (regexp-quote symbol)
                                        "[ \t]*::"))
                     result)
                (with-temp-buffer
                  (insert hdr-str)
                  (goto-char (point-min))
                  (when (re-search-forward pat nil t)
                    ;; match-beginning 0 is position of [ in temp buffer (1-indexed),
                    ;; so hdr-open1 + (match-beginning 0 - 1) + 1 = hdr-open1 + match-beginning 0.
                    ;; No extra +1 needed — that was causing the off-by-one.
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
  "Return an xref location jumping to the module name inside its (module ...) decl."
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
  "Find definitions of SYMBOL — parameters first, then locals, then imports."
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
  "Return all identifiers: keywords, local defines, imported symbols, module names."
  (let* ((imports (monad--parse-imports))
         (mods    (mapcar (lambda (m) (propertize (car m) 'company-kind 'module))
                          imports)))
    (delete-dups
     (append (monad--keyword-candidates)
             (monad--collect-defines)
             mods
             (monad--import-completions imports)))))

(defvar-keymap monad-mode-map
  :doc "Keymap for Monad mode."
  :parent lisp-mode-shared-map
  ":" #'monad-colon)

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
