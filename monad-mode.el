;;; monad-mode.el --- Major mode for the Monad programming language -*- lexical-binding: t; -*-

;; Author: Laluxx
;; Version: 0.0.1
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
    "begin" "do" "when" "unless" "error")
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

(defun monad-font-lock-keywords ()
  "Return font-lock keywords for Monad mode."
  (append
   (list
    ;; Keywords
    (cons (regexp-opt monad-keywords 'symbols) 'font-lock-keyword-face)

    ;; Type annotation operator ::
    '("::" . font-lock-builtin-face)

    ;; Special :keywords (symbols starting with :)
    '(":\\sw+" . font-lock-builtin-face)

    ;; Character literals 'x' (exactly one character)
    '(monad-char-literal-matcher . font-lock-string-face)

    ;; Underscore wildcard
    '("\\<_\\>" . 'shadow)

    ;; Function definitions: (define (name ...) ...)
    '("(\\(define\\)\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))

    ;; Variable definitions: (define name ...) and (define [name :: Type] ...)
    '("(\\(define\\)\\s-+\\[?\\(\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face)))
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
  (setq-local indent-line-function 'lisp-indent-line)
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
  (setq font-lock-defaults
        '((monad-font-lock-keywords)
          nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
          beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . lisp-font-lock-syntactic-face-function)))
  (setq-local lisp-doc-string-elt-property 'monad-doc-string-elt)
  ;; Add xref backend
  (add-hook 'xref-backend-functions #'monad-xref-backend nil t))

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
  :parent lisp-mode-shared-map)

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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mon\\'" . monad-mode))

(provide 'monad-mode)

;;; monad-mode.el ends here
