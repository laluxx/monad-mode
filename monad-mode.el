;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.2
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

(defface monad-underscore-face
  '((t :inherit shadow))
  "Face for underscore wildcard pattern."
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
    (modify-syntax-entry ?` "'   " st)

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
  '("define" "lambda" "match" "layout"
    "let" "letrec" "let*" "if" "cond" "case" "else"
    "and" "or" "not" "quote" "unquote" "quasiquote"
    "begin" "do" "when" "unless" "error" "instance" "asm")
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
             ;; Mark the content region
             (put-text-property asm-keyword-end asm-end 'monad-asm-region t))
           nil)))))
   start end))

(defun monad-font-lock-extend-region ()
  "Extend font-lock region to cover complete asm forms.
This ensures that when you edit inside an asm form, the entire form is refontified."
  (let ((changed nil))
    (save-excursion
      ;; Extend backward if we start in an asm region
      (goto-char font-lock-beg)
      (when (monad-in-asm-form-p)
        (let ((start (previous-single-property-change (point) 'monad-asm-region)))
          (when start
            ;; Find the opening (asm
            (goto-char start)
            (when (re-search-backward "(\\s-*asm\\_>" (max (point-min) (- start 100)) t)
              (setq font-lock-beg (point)
                    changed t)))))

      ;; Extend forward if we end in an asm region
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
Labels (lines ending with :) are flushed to the left.
Other lines get standard indentation."
  (interactive)
  (let ((savep (point)))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((indent-col
             (cond
              ;; Label: flush to left (column 0)
              ((looking-at "\\sw+:")
               0)
              ;; Regular instruction: indent to column 2
              (t
               2))))
        (if (= (current-indentation) indent-col)
            nil  ; Already correctly indented
          (delete-horizontal-space)
          (indent-to indent-col))))
    ;; If point was in the indentation, move it to the first non-whitespace
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
    ;; Just typed a colon in an asm block - refontify the line
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
    ;; Force refontification of the current line
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

(defvar monad-asm-font-lock-keywords-cache nil
  "Cached assembly font-lock keywords for performance.")

(defun monad-asm-get-font-lock-keywords ()
  "Get assembly font-lock keywords from asm-mode or create fallback."
  (or monad-asm-font-lock-keywords-cache
      (setq monad-asm-font-lock-keywords-cache
            (if (and (boundp 'asm-font-lock-keywords)
                     (not (eq asm-font-lock-keywords 'unbound)))
                ;; Use asm-mode's keywords if available
                asm-font-lock-keywords
              ;; Fallback: basic assembly highlighting
              (list
               ;; Comments (semicolon to end of line)
               '(";.*$" . font-lock-comment-face)
               ;; Labels - match word at start of whitespace followed by colon
               '("^\\s-*\\([.a-zA-Z_][a-zA-Z0-9_]*\\):" 1 font-lock-function-name-face)
               ;; Instructions (common x86/x64)
               (cons (regexp-opt
                      '("mov" "movq" "movl" "movb" "movw" "movabs" "movsx" "movzx"
                        "push" "pop" "pushq" "popq"
                        "add" "addq" "addl" "sub" "subq" "subl"
                        "mul" "imul" "imulq" "div" "idiv"
                        "inc" "incq" "incl" "dec" "decq" "decl"
                        "neg" "not"
                        "and" "andq" "andl" "or" "orq" "orl" "xor" "xorq" "xorl"
                        "shl" "shr" "sal" "sar" "rol" "ror"
                        "cmp" "cmpq" "cmpl" "test" "testq" "testl"
                        "jmp" "je" "jz" "jne" "jnz" "jg" "jge" "jl" "jle"
                        "ja" "jae" "jb" "jbe" "js" "jns" "jo" "jno"
                        "call" "ret" "leave"
                        "lea" "leaq"
                        "nop" "syscall" "int" "int3"
                        "enter" "rep" "repe" "repz" "repne" "repnz"
                        "rdtsc")
                      'symbols)
                     'font-lock-keyword-face)
               ;; Registers
               '("%[er]?[abcd]x\\|%[er]?[sd]i\\|%[er]?[sb]p\\|%r[0-9]+[dwb]?\\|%[abcd][hl]\\|%[er]?ip"
                 . font-lock-variable-name-face)
               ;; Directives
               '("\\.[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-builtin-face)
               ;; Immediate values
               '("\\$-?[0-9]+" . font-lock-constant-face)
               '("\\$0x[0-9a-fA-F]+" . font-lock-constant-face)
               ;; Memory operands with offset
               '("-?[0-9]+(%[a-z]+" . font-lock-constant-face))))))

(defun monad-font-lock-keywords ()
  "Return font-lock keywords for Monad mode."
  (append
   (list
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


    ;; #+ by itself (with nothing after) - shadow the whole thing
    '("#\\+\\>" . 'shadow)

    ;; #+ with feature name: #+ symbol shadow, feature name keyword
    '("\\(#\\+\\)\\(\\sw+\\)"
      (1 'shadow t)
      (2 font-lock-function-name-face t))

    ;; #- with feature name: entire thing shadow
    '("#-\\sw+" . 'shadow)

    ;; #--- style separators (any sequence of dashes after #)
    '("#-+\\>" . 'shadow)

    ;; Function definitions: (define (name ...) ...)
    '("(\\(define\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))

    ;; Variable definitions: (define name ...) and (define [name :: Type] ...)
    '("(\\(define\\)\\s-+\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))

   ;; Assembly syntax inside asm forms - simplified for speed
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
                            ;; In asm region - search within it
                            (let ((region-end (or (next-single-property-change
                                                  (point) 'monad-asm-region nil limit)
                                                 limit)))
                              (if (re-search-forward matcher region-end t)
                                  (setq found t)
                                (goto-char region-end)))
                          ;; Not in asm - jump to next region
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
       ;; The function is called with point right after "define".
       (forward-comment (point-max))
       (cond
        ;; Function definition: (define (name ...) "doc" ...)
        ;; The docstring is at position 2 (0-indexed: define=0, (name...)=1, "doc"=2)
        ((eq (char-after) ?\() 2)
        ;; Variable definition: (define x value "doc") or (define [x :: Type] value "doc")
        ;; Need to check if there's a docstring after the value
        (t
         (condition-case nil
             (progn
               (forward-sexp 1) ; skip name or [name :: Type]
               (forward-comment (point-max))
               (forward-sexp 1) ; skip value
               (forward-comment (point-max))
               ;; If we're looking at a string, it's a docstring
               ;; Position is 3 (0-indexed: define=0, name=1, value=2, "doc"=3)
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
  ;; Add xref backend
  (add-hook 'xref-backend-functions #'monad-xref-backend nil t)
  ;; Add post-self-insert hook for asm fontification
  (add-hook 'post-self-insert-hook #'monad-post-self-insert nil t))

(defun monad-xref-backend ()
  "Monad backend for xref."
  'monad)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql monad)))
  "Return the identifier at point."
  (let ((sym (symbol-at-point)))
    (and sym (symbol-name sym))))

(cl-defmethod xref-backend-definitions ((_backend (eql monad)) symbol)
  "Find definitions of SYMBOL in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((defs nil)
          (case-fold-search nil))
      ;; Search for (define symbol ...) or (define (symbol ...) ...) or (define [symbol :: Type] ...)
      (while (re-search-forward
              (concat "^\\s-*(define\\s-+\\(?:"
                      ;; Function definition: (define (symbol ...) ...)
                      "(\\(" (regexp-quote symbol) "\\)\\>"
                      "\\|"
                      ;; Typed variable: (define [symbol :: Type] ...)
                      "\\[\\(" (regexp-quote symbol) "\\)\\s-+::"
                      "\\|"
                      ;; Regular variable: (define symbol ...)
                      "\\(" (regexp-quote symbol) "\\)\\>"
                      "\\)")
              nil t)
        (let ((pos (or (match-beginning 1)
                      (match-beginning 2)
                      (match-beginning 3))))
          (push (xref-make (format "%s" symbol)
                          (xref-make-buffer-location
                           (current-buffer)
                           pos))
                defs)))

      ;; Also search for function parameters
      ;; Pattern: (define (fname param1 param2 ...) ...)
      ;; or (define (fname param1 param2 -> [ret :: Type]) ...)
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*(define\\s-+(\\([^)]+\\))" nil t)
        (let ((params-str (match-string 1))
              (params-start (match-beginning 1)))
          (with-temp-buffer
            (insert params-str)
            (goto-char (point-min))
            ;; Skip function name
            (forward-sexp 1)
            ;; Now parse parameters
            (while (not (eobp))
              (skip-chars-forward " \t\n")
              (when (looking-at (regexp-quote symbol))
                (let ((offset (- (point) 1)))
                  (push (xref-make (format "%s (parameter)" symbol)
                                  (xref-make-buffer-location
                                   (current-buffer)
                                   (+ params-start offset)))
                        defs)))
              (condition-case nil
                  (forward-sexp 1)
                (error (goto-char (point-max))))))))

      (nreverse defs))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql monad)))
  "Return completion table for identifiers."
  (let ((symbols nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^\\s-*(define\\s-+\\(?:(\\)?\\(\\(?:\\sw\\|\\s_\\)+\\)"
              nil t)
        (push (match-string-no-properties 1) symbols)))
    symbols))

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
