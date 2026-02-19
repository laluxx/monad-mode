;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.3
;; Package-Requires: ((emacs "24.3"))
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

;;; Code:

(require 'lisp-mode)
(require 'cl-lib)

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

(defvar monad-imenu-generic-expression
  `((nil
     ,(rx bol (zero-or-more space)
          "(define"
          (one-or-more space)
          (zero-or-one "(")
          (group (one-or-more (or word (syntax symbol)))))
     1))
  "Imenu generic expression for Monad mode.")

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
    ;; Group 0 (whole match including backticks) and group 1 (name only)
    ;; both get monad-infix-face so that when backticks are hidden via
    ;; post-command-hook the name still carries its face independently.
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
  (setq-local imenu-generic-expression monad-imenu-generic-expression)
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

  ;; Infix backtick visibility — use idle timer, not post-command-hook,
  ;; to avoid hangs during fontification and file load.
  (add-hook 'post-command-hook #'monad-infix-schedule-refresh nil t)

  ;; Electric pair for backtick
  (monad--setup-electric-pair))

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
  "Return the list of exported symbol names from FILE as propertized strings.
Reads the (module Name [sym ...]) declaration.  If the export list is
absent the file exports everything defined in it."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (re-search-forward
           "^(module\\s-+\\(?:\\sw\\|\\s_\\)+\\s-+\\[\\([^]]*\\)\\]" nil t)
          ;; Explicit export list — we don't know kinds here so tag as `function'
          ;; (the most common export); variable defines will be re-tagged by
          ;; monad--collect-defines when imported symbols are resolved.
          (mapcar (lambda (s) (propertize s 'company-kind 'function))
                  (split-string (match-string-no-properties 1) "[ \t\n]+" t))
        ;; No export list — collect all defines with proper kinds
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
  "Return an alist of imported modules in the current buffer.
Each element is (MODULE-NAME . PLIST) where PLIST has keys:
  :qualified  t/nil
  :alias      string or nil   (from :as M)
  :hiding     list of strings (from hiding [...])
  :only       list of strings (from a non-qualified non-hiding import with [...])
  :file       path to .mon file"
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
  "Return completion candidates derived from IMPORTS with `company-kind' properties.
Qualified forms (Module.sym) carry the same kind as the bare symbol."
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
          ;; Qualified form inherits kind from the bare symbol
          (let ((kind (get-text-property 0 'company-kind sym)))
            (push (propertize (concat prefix "." sym) 'company-kind kind)
                  candidates)))))
    (nreverse candidates)))

(defun monad--asm-first-token-p (start)
  "Return t if START is the position of the first token on its line in an asm block.
Used to restrict instruction completion to the opcode position."
  (save-excursion
    (goto-char start)
    (skip-chars-backward " \t")
    (bolp)))

(defun monad--asm-operand-candidates ()
  "Completion candidates for asm operand positions (after the instruction).
Includes registers and local Monad variables — but NOT instructions."
  (delete-dups
   (append
    (mapcar (lambda (r) (propertize r 'company-kind 'variable))
            monad-asm-registers)
    (monad--collect-defines))))

(defun monad--asm-candidates ()
  "Return asm completion candidates for use inside (asm ...) blocks.
Instructions get the `monad-asm' kind (nerd-icons-corfu shows nf-seti-asm).
Registers get `variable' kind.
Also includes local defines so Monad variables can be used as asm operands."
  (delete-dups
   (append
    (mapcar (lambda (i) (propertize i 'company-kind 'monad-asm))
            monad-asm-instructions)
    (mapcar (lambda (r) (propertize r 'company-kind 'variable))
            monad-asm-registers)
    ;; Local defines are valid asm operands (Monad passes them as registers)
    (monad--collect-defines))))

(defun monad--keyword-candidates ()
  "Return monad keywords as propertized candidates with `company-kind' = `keyword'."
  (mapcar (lambda (kw) (propertize kw 'company-kind 'keyword))
          monad-keywords))

(defun monad--all-completions ()
  "Return all Monad completion candidates (not for asm context).
Includes keywords, local defines, imported symbols, and qualified forms."
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

;; Tell nerd-icons-corfu how to render the custom `monad-asm' kind.
;; nerd-icons-sucicon is called as (nerd-icons-sucicon "nf-seti-asm").
;; The mapping builds the name as "nf-" + style + "-" + icon, so:
;;   :style "seti" would call nerd-icons-setiicon — doesn't exist
;;   :style "suc"  + :icon "seti-asm" → "nf-suc-seti-asm" — wrong
;; The correct combo: use :fn to call nerd-icons-sucicon directly.
;; This is a no-op if nerd-icons-corfu is not installed.
(with-eval-after-load 'nerd-icons-corfu
  (add-to-list 'nerd-icons-corfu-mapping
               '(monad-asm :fn (lambda (_cand)
                                 (concat (nerd-icons-sucicon "nf-seti-asm") " "))
                           :face font-lock-keyword-face)))

(defun monad-completion-at-point ()
  "Completion-at-point for Monad mode.
Inside an (asm ...) block:
  - First token on a line → instructions only (opcode position)
  - Subsequent tokens → registers and local variables (operand position)
Outside: keywords, functions, variables, and qualified imports.
Works with Corfu, nerd-icons-corfu, and the built-in completion UI."
  (let* ((end   (point))
         (start (save-excursion
                  (skip-syntax-backward "w_%") ; include % for registers
                  (when (and (> (point) (point-min))
                             (eq (char-before) ?.))
                    (skip-syntax-backward "w_"))
                  (point)))
         ;; Check asm context at START (beginning of current token), not END,
         ;; so that completing the last word before )) still works correctly.
         (in-asm (monad-in-asm-form-p start))
         (candidates
          (cond
           ((and in-asm (monad--asm-first-token-p start))
            ;; Opcode position: instructions only
            (mapcar (lambda (i) (propertize i 'company-kind 'monad-asm))
                    monad-asm-instructions))
           (in-asm
            ;; Operand position: registers + local vars, no instructions
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
              ('monad-asm " asm")
              ('keyword   " keyword")
              ('function  " function")
              ('variable  " variable")
              (_          ""))))))

;;;; Xref backend

(defun monad-xref-backend ()
  "Monad backend for xref."
  'monad)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql monad)))
  "Return the identifier at point, including qualified Module.sym forms."
  (let* ((sym (symbol-at-point))
         (name (and sym (symbol-name sym))))
    ;; Check if there's a Module. prefix just before point
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
  "Return a list of all xref locations where SYMBOL is defined in BUF.
Finds every top-level `(define ...)' that matches, so multiple definitions
of the same name all appear in the xref picker."
  (let (locs)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                (concat "^(define\\s-+\\(?:"
                        "(\\(" (regexp-quote symbol) "\\)\\>"   ; (define (NAME
                        "\\|\\[\\(" (regexp-quote symbol) "\\)\\s-+::" ; (define [NAME ::
                        "\\|\\(" (regexp-quote symbol) "\\)\\>\\)")    ; (define NAME
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
  "Search for SYMBOL as a parameter in the enclosing `(define (fn ...))'.
Returns an xref location if found, nil otherwise.
Parameters take priority over global definitions."
  (save-excursion
    ;; Walk up to find the enclosing (define (fn ...)) form
    (condition-case nil
        (progn
          (beginning-of-defun)
          ;; Match (define (fname params...) ...)
          (when (looking-at "(define\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)")
            (let* ((defun-start (point))
                   (header-end  (save-excursion
                                  (down-list)        ; into (define
                                  (forward-sexp 1)   ; skip define
                                  (forward-sexp 1)   ; skip (fname params)
                                  (point)))
                   result)
              ;; Search only inside the parameter list for SYMBOL
              (save-excursion
                (goto-char defun-start)
                (down-list)          ; into (define
                (forward-sexp 1)     ; past `define'
                (down-list)          ; into (fname params...)
                (forward-sexp 1)     ; past function name
                (let ((params-start (point)))
                  (condition-case nil
                      (progn
                        (up-list)    ; to end of param list
                        (let ((params-end (point)))
                          (goto-char params-start)
                          (while (and (< (point) params-end) (not result))
                            (skip-chars-forward " \t\n[")
                            (when (looking-at (concat (regexp-quote symbol) "\\>"))
                              (setq result (xref-make
                                            (concat symbol " (parameter)")
                                            (xref-make-buffer-location
                                             (current-buffer) (point)))))
                            (condition-case nil
                                (forward-sexp 1)
                              (error (goto-char params-end))))))
                    (error nil))))
              result)))
      (error nil))))

(defun monad--find-in-file (file symbol)
  "Search FILE for all definitions of SYMBOL.
Returns a list of xref locations."
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
      ;; 1. Module name (capitalised, no dot) → jump to (module Name ...) decl
      (when (and (string-match-p "^[A-Z]" symbol)
                 (not (string-match-p "\\." symbol)))
        (when-let* ((loc (monad--find-module-location symbol)))
          (push loc defs)))

      ;; 2. Qualified Module.sym → look in the module file
      (when (string-match "^\\([A-Z][^.]*\\)\\.\\(.+\\)$" symbol)
        (let* ((modname (match-string 1 symbol))
               (bare    (match-string 2 symbol))
               (file    (monad--module-file modname)))
          (dolist (loc (monad--find-in-file file bare))
            (push loc defs))))

      ;; 3. Plain symbol
      (when (null defs)
        ;; 3a. Parameter — return immediately, never mix with globals
        (when-let* ((param-loc (monad--find-parameters symbol)))
          (throw 'monad-xref-done (list param-loc)))

        ;; 3b. All global definitions in the current buffer (may be many)
        (dolist (loc (monad--find-all-defines (current-buffer) symbol))
          (push loc defs))

        ;; 3c. If still nothing, search imported modules
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
