;;; monad-repl.el --- REPL for Monad mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Laluxx
;; Maintainer: Laluxx
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages processes repl
;; URL: https://github.com/laluxx/monad-mode

;;; Commentary:

;; This package provides REPL (Read-Eval-Print Loop) support for monad-mode.
;; It uses comint-mode to provide an interactive Monad interpreter session
;; with features like:
;; - Interactive REPL with completion
;; - Automatic file loading on save
;; - Expression evaluation
;; - Region evaluation
;; - ANSI color support

;;; TODO [0/3]
;; - [ ] When clicking on a warning pulse with the a warning face instead!
;; - [ ] Apropos to search for functions/variables/modules
;; - [ ] It doesn't buttonize errors when loading with ,load E.g: Random.mon:122:23: error: unbound variable: e18

;;; Code:

(require 'comint)
(require 'ansi-color)

(when (boundp 'nerd-icons-corfu-mapping)
  (add-to-list 'nerd-icons-corfu-mapping
               '(c-header :fn (lambda (_cand)
                                (concat (nerd-icons-faicon "nf-fa-h_square") " "))
                          :face nerd-icons-purple)))

;;; Custom variables

(defgroup monad-repl nil
  "REPL support for Monad mode."
  :prefix "monad-repl-"
  :group 'monad)

(defcustom monad-repl-program "monad"
  "Path to the Monad interpreter executable."
  :type 'string
  :group 'monad-repl)

(defcustom monad-repl-arguments '("-i")
  "Arguments to pass to the Monad interpreter for REPL mode."
  :type '(repeat string)
  :group 'monad-repl)

(defcustom monad-repl-buffer-name "*monad-repl*"
  "Name of the Monad REPL buffer."
  :type 'string
  :group 'monad-repl)

(defcustom monad-repl-show-prompt t
  "When non-nil, show the Monad prompt in the Emacs REPL buffer.
Set to nil to suppress the prompt entirely (the Emacs buffer
provides its own visual separation between inputs)."
  :type 'boolean
  :safe 'booleanp
  :group 'monad-repl)

(defcustom monad-repl-load-on-save nil
  "When non-nil, automatically load the current file in REPL on save."
  :type 'boolean
  :safe 'booleanp
  :group 'monad-repl)

(defcustom monad-repl-pop-to-buffer-on-load nil
  "When non-nil, switch to REPL buffer after loading a file."
  :type 'boolean
  :safe 'booleanp
  :group 'monad-repl)

;;; Faces

(defface monad-repl-error-highlight
  '((t :foreground "#ff6c6b" :background "#53383f"))
  "Face used to highlight error positions in the Monad REPL."
  :group 'monad-repl)

(defface monad-repl-shadow
  '((t :inherit shadow))
  "Face used for >>> prompts and separators in the Monad REPL."
  :group 'monad-repl)

;;; Internal variables

(defvar monad-repl-prompt-regexp "^Monad \xce\xbb "
  "Regexp to match the Monad REPL prompt.")

(defvar monad-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "TAB") #'monad-repl--tab-command)
    (define-key map (kbd "RET") #'monad-repl-maybe-send)
    (define-key map (kbd "C-x C-n") #'monad-repl--next-error)
    (define-key map (kbd "C-x C-p") #'monad-repl--prev-error)
    (define-key map (kbd "C-l") #'comint-clear-buffer)
    (define-key map (kbd "C-c C-l") #'monad-repl-load-file)
    (define-key map (kbd "C-c C-z") #'monad-repl-switch-back)
    (define-key map (kbd "M-.") #'monad-repl-find-definition)
    (define-key map (kbd "M-,") #'xref-go-back)
    map)
  "Keymap for `monad-repl-mode'.")

(defvar-local monad-repl--last-source-buffer nil
  "The last `monad-mode' buffer that switched to this REPL.")

(defvar-local monad-repl--completion-cache nil
  "Cache for REPL completions.")

(defvar monad-repl--completion-timeout 0.3
  "Timeout in seconds for completion requests.")

;;; REPL process management

(defun monad-repl-process ()
  "Return the current Monad REPL process, or nil if none."
  (get-buffer-process monad-repl-buffer-name))

(defun monad-repl-buffer ()
  "Return the current Monad REPL buffer, or nil if none."
  (get-buffer monad-repl-buffer-name))

(defun monad-repl-running-p ()
  "Return non-nil if a Monad REPL is currently running."
  (and (monad-repl-buffer)
       (monad-repl-process)
       (process-live-p (monad-repl-process))))

;;;###autoload
(defun monad-repl ()
  "Start or switch to the Monad REPL."
  (interactive)
  (let ((repl-buffer (monad-repl-buffer))
        (source-buffer (current-buffer)))
    (if (and repl-buffer (monad-repl-running-p))
        (progn
          (pop-to-buffer repl-buffer)
          (with-current-buffer repl-buffer
            (setq monad-repl--last-source-buffer source-buffer)))
      (let* ((process-environment
              (append (list "INSIDE_EMACS=1"
                            (if monad-repl-show-prompt
                                "MONAD_NO_PROMPT=0"
                              "MONAD_NO_PROMPT=1"))
                      process-environment))
             (buffer (apply #'make-comint-in-buffer
                            "monad-repl"
                            monad-repl-buffer-name
                            monad-repl-program
                            nil
                            monad-repl-arguments)))
        (with-current-buffer buffer
          (monad-repl-mode)
          (setq monad-repl--last-source-buffer source-buffer))
        (pop-to-buffer buffer)))))

(defun monad-repl-switch-back ()
  "Switch back to the last source buffer that started this REPL."
  (interactive)
  (when monad-repl--last-source-buffer
    (if (buffer-live-p monad-repl--last-source-buffer)
        (pop-to-buffer monad-repl--last-source-buffer)
      (message "Last source buffer is no longer alive"))))

(defun monad-repl-restart ()
  "Restart the Monad REPL."
  (interactive)
  (when (monad-repl-running-p)
    (let ((proc (monad-repl-process)))
      (delete-process proc)))
  (when (monad-repl-buffer)
    (kill-buffer (monad-repl-buffer)))
  (monad-repl))

;;; Multi line editing

(defun monad-repl--nesting-level ()
  "Return the nesting depth of parens at point relative to the prompt.
Skips string literals so quoted parens don't affect the depth count."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (when pmark
      (save-excursion
        (let ((depth 0)
              (in-string nil))
          (goto-char pmark)
          (while (< (point) (point-max))
            (let ((ch (char-after)))
              (cond
               ;; Toggle string mode on unescaped "
               ((and (eq ch ?\") (not in-string))
                (setq in-string t))
               ((and (eq ch ?\") in-string
                     (not (eq (char-before) ?\\)))
                (setq in-string nil))
               ;; Only count parens outside strings
               ((not in-string)
                (cond ((memq ch '(?\( ?\[ ?\{)) (setq depth (1+ depth)))
                      ((memq ch '(?\) ?\] ?\})) (setq depth (1- depth)))))))
            (forward-char 1))
          depth)))))

(defun monad-repl--newline-and-indent ()
  "Insert newline and indent within a multiline expression."
  (interactive)
  (insert "\n")
  (monad-repl--indent-line))

(defun monad-repl-maybe-send ()
  "Send input if balanced and at end, otherwise newline and indent."
  (interactive)
  (let ((level (monad-repl--nesting-level)))
    (cond
     ((and level (> level 0))
      (monad-repl--newline-and-indent))
     ((= (point) (point-max))
      (let* ((proc  (get-buffer-process (current-buffer)))
             (pmark (and proc (process-mark proc))))
        (if (and pmark (> (point-max) pmark))
            ;; Send the entire accumulated input from pmark to point-max
            ;; as one string — handles multiline expressions with strings.
            (let ((input (buffer-substring-no-properties pmark (point-max))))
              (goto-char (point-max))
              (insert "\n")
              (process-send-string proc (concat (string-trim-right input) "\n"))
              (comint-add-to-input-history (string-trim-right input))
              (set-marker (process-mark proc) (point-max)))
          (comint-send-input))))
     (t
      (monad-repl--newline-and-indent)))))

;;; Evaluation and loading

(defun monad-repl-send-string (string)
  "Send STRING to the Monad REPL process."
  (unless (monad-repl-running-p)
    (monad-repl))
  (with-current-buffer (monad-repl-buffer)
    (let* ((proc (monad-repl-process))
           (input (string-trim string)))
      (goto-char (point-max))
      (insert input)
      (insert "\n")
      (comint-add-to-input-history input)
      (process-put proc 'monad-repl-busy t)
      (process-send-string proc (concat input "\n"))
      (set-marker (process-mark proc) (point-max)))))

(defun monad-repl-load-file (&optional filename)
  "Load FILENAME into the Monad REPL.
If FILENAME is nil, use the current buffer's file."
  (interactive)
  (let* ((file (or filename
                  (buffer-file-name)
                  (read-file-name "Load file: ")))
         (expanded-file (expand-file-name file)))
    (unless (file-exists-p expanded-file)
      (error "File does not exist: %s" expanded-file))
    (unless (monad-repl-running-p)
      (monad-repl))
    (monad-repl-send-string (format ",load %s" expanded-file))
    (when monad-repl-pop-to-buffer-on-load
      (pop-to-buffer (monad-repl-buffer)))
    (message "Loaded %s" expanded-file)))

(defun monad-repl-eval-region (start end)
  "Evaluate the region between START and END in the Monad REPL."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         ;; Remove leading/trailing whitespace but preserve internal structure
         (trimmed (string-trim code)))
    ;; Send as a single complete input
    (monad-repl-send-string trimmed)
    (unless monad-repl-pop-to-buffer-on-load
      (display-buffer (monad-repl-buffer) '(display-buffer-at-bottom)))))

(defun monad-repl-eval-buffer ()
  "Evaluate the entire buffer in the Monad REPL."
  (interactive)
  (monad-repl-eval-region (point-min) (point-max)))

(defun monad-repl-eval-defun ()
  "Evaluate the current function definition in the Monad REPL."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (monad-repl-eval-region start (point)))))

(defun monad-repl-eval-last-sexp ()
  "Evaluate the sexp before point in the Monad REPL."
  (interactive)
  (let ((end (point))
        (start (save-excursion
                 (backward-sexp)
                 (point))))
    (monad-repl-eval-region start end)))

;;; Hooks

(defun monad-repl-load-on-save-hook ()
  "Load the current file in the REPL if enabled."
  (when (and (eq major-mode 'monad-mode)
             monad-repl-load-on-save
             (buffer-file-name)
             (monad-repl-running-p))
    (save-excursion
      (monad-repl-load-file (buffer-file-name)))))

;;; Completion - Protocol-based approach

(defun monad-repl--send-completion-request (prefix)
  "Send completion request for PREFIX to REPL and return results."
  (when (and (monad-repl-running-p) prefix
             ;; Never send a completion request while the process is
             ;; busy producing output — the custom filter would swallow it.
             (not (process-get (monad-repl-process) 'monad-repl-busy)))
    (let* ((proc (monad-repl-process))
           (output "")
           (done nil)
           (original-filter (process-filter proc)))
      (unwind-protect
          (progn
            (set-process-filter
             proc
             (lambda (_proc string)
               (setq output (concat output string))
               (when (string-match-p "__END__" output)
                 (setq done t))))
            (process-send-string proc (format ",complete %s\n" prefix))
            ;; Wait for __END__ marker, bail out after timeout
            (let ((deadline (+ (float-time) monad-repl--completion-timeout)))
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output proc 0.05)))
            (monad-repl--parse-completion-output output))
        ;; Always restore the original filter — never let output
        ;; reach comint's default filter which would print it
        (set-process-filter proc original-filter)))))

(defvar-local monad-repl--symbol-table (make-hash-table :test 'equal)
  "Maps symbol names to (kind signature docstring) from the REPL.")

(defun monad-repl--parse-completion-output (output)
  "Parse completion OUTPUT from ,complete command.
Each line: name TAB kind TAB signature TAB docstring
Returns list of name strings; populates `monad-repl--symbol-table'."
  (let ((completions nil)
        (in-completions nil))
    (dolist (line (split-string output "\n" t))
      (let ((clean-line (string-trim (ansi-color-filter-apply line))))
        (cond
         ((string= clean-line "__COMPLETIONS__")
          (setq in-completions t))
         ((string= clean-line "__END__")
          (setq in-completions nil))
         (in-completions
          (when (not (string-empty-p clean-line))
            (let* ((parts (split-string clean-line "\t"))
                   (name  (nth 0 parts))
                   (kind  (nth 1 parts))
                   (sig   (nth 2 parts))
                   (doc   (when-let* ((d (nth 3 parts)))
                            (replace-regexp-in-string "\\\\n" "\n" d)))
                   (hdr   (nth 4 parts)))
              (when (and name (not (string-empty-p name)))
                (push name completions)
                (puthash name (list kind sig doc hdr)
                         monad-repl--symbol-table))))))))
    (nreverse completions)))

(defun monad-repl--refresh-symbol-table ()
  "Request all completions from the REPL to populate the symbol table."
  (when (monad-repl-running-p)
    (monad-repl--get-completions "")))

(defun monad-repl--get-completions (prefix)
  "Get completion candidates for PREFIX.
Uses caching to avoid repeated requests."
  (or (gethash prefix monad-repl--completion-cache)
      (let ((completions (monad-repl--send-completion-request prefix)))
        (when completions
          (puthash prefix completions monad-repl--completion-cache))
        completions)))

(defun monad-repl--clear-completion-cache ()
  "Clear the completion cache."
  (when monad-repl--completion-cache
    (clrhash monad-repl--completion-cache)))

(defun monad-repl--completion-bounds ()
  "Get completion bounds at point.
Returns (START . END) or nil."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (when (and pmark (>= (point) pmark))
      (save-excursion
        (let ((end (point)))
          ;; Move back over valid identifier characters
          (skip-chars-backward "A-Za-z0-9_:.-")
          (cons (point) end))))))

(defun monad-repl--symbol-kind (name)
  "Return a company kind symbol for NAME based on the symbol table."
  (let ((info (and monad-repl--symbol-table
                   (gethash name monad-repl--symbol-table))))
    (if info
        (let ((kind (nth 0 info)))
          (cond
           ((string= kind "func")    'function)
           ((string= kind "builtin") 'function)
           ((string= kind "var")     'variable)
           ((string= kind "keyword") 'keyword)
           (t                        'text)))
      'text)))

(defun monad-repl-completion-at-point ()
  "Completion at point function for Monad REPL."
  (when (derived-mode-p 'monad-repl-mode)
    (let* ((line-start (line-beginning-position))
           (before (buffer-substring-no-properties line-start (point))))
      (unless (or (string-match "\\bimport +" before)
                  (string-match "\\binclude +" before))
        (let ((bounds (or (monad-repl--completion-bounds)
                          (cons (point) (point)))))
          (when bounds
            (let* ((start (car bounds))
                   (end   (cdr bounds)))
              (list start end
                    (completion-table-dynamic
                     (lambda (str)
                       (or (monad-repl--get-completions str) nil)))
                    :exclusive 'no
                    :company-doc-buffer #'ignore
                    :company-kind #'monad-repl--symbol-kind
                    :annotation-function
                    (lambda (cand)
                      (when-let* ((info (monad-repl--lookup cand))
                                  (kind (nth 0 info))
                                  (sig  (nth 1 info)))
                        (unless (string-empty-p sig)
                          (if (string= kind "builtin")
                              (concat " " sig)
                            (let* ((stripped (replace-regexp-in-string
                                              (concat "^(" (regexp-quote cand) " *")
                                              "" sig))
                                   (stripped (string-trim stripped "(" ")")))
                              (concat " " stripped))))))))))))))

;;;; Non protocol completions

(defvar monad-repl--system-headers-cache nil
  "Cached list of system header paths.")

(defun monad-repl--system-headers ()
  "Return cached list of C header paths from /usr/include."
  (or monad-repl--system-headers-cache
      (let ((root "/usr/include")
            (headers nil))
        (dolist (file (directory-files-recursively root "\\.h$"))
          (push (concat "<" (substring file (1+ (length root))) ">") headers))
        (setq monad-repl--system-headers-cache (nreverse headers)))))

(defun monad-repl--include-completion-at-point ()
  "Complete header names after bare 'include'."
  (when (derived-mode-p 'monad-repl-mode)
    (let* ((line-start (line-beginning-position))
           (before (buffer-substring-no-properties line-start (point))))
      (when (string-match "\\binclude +\\([A-Za-z0-9_./-]*\\)$" before)
        (let ((start (+ line-start (match-beginning 1)))
              (end   (+ line-start (match-end 1))))
          (list start end
                (monad-repl--system-headers)
                :exclusive 'yes
                :company-kind (lambda (_) 'file)))))))

(defun monad-repl--installed-modules ()
  "Return list of installed Monad module names from /usr/local/lib/monad."
  (let ((root "/usr/local/lib/monad")
        (modules nil))
    (when (file-directory-p root)
      (dolist (file (directory-files-recursively root "\\.mon$"))
        (push (file-name-sans-extension (file-name-nondirectory file))
              modules)))
    (nreverse modules)))

(defun monad-repl--special-completion-at-point ()
  "Complete module names after import, or headers after include."
  (when (derived-mode-p 'monad-repl-mode)
    (let* ((line-start (line-beginning-position))
           (before (buffer-substring-no-properties line-start (point))))
      (cond
       ((or (string-match "(import +\\([A-Za-z0-9_]*\\)$" before)
            (string-match "\\bimport +\\([A-Za-z0-9_]*\\)$" before))
        (let ((start (+ line-start (match-beginning 1)))
              (end   (+ line-start (match-end 1))))
          (list start end
                (monad-repl--installed-modules)
                :exclusive 'yes
                :company-kind (lambda (_) 'module))))
       ((string-match "\\binclude +\\(<[A-Za-z0-9_./-]*\\|[A-Za-z0-9_./-]*\\)$" before)
        (let ((start (+ line-start (match-beginning 1)))
              (end   (+ line-start (match-end 1))))
          (list start end
                (monad-repl--system-headers)
                :exclusive 'yes
                :company-kind (lambda (_) 'c-header))))))))

;;; Eldoc

(defun monad-repl--depth-face (depth)
  "Return the rainbow-delimiters face for DEPTH."
  (intern (format "rainbow-delimiters-depth-%d-face"
                  (max 1 (min 9 (1+ depth))))))

(defun monad-repl--propertize-sig (sig)
  "Propertize SIG string with syntax colours."
  (let ((s (copy-sequence sig))
        (depth 0))
    (dotimes (i (length s))
      (let ((ch (aref s i)))
        (cond
         ((memq ch '(?\( ?\[))
          (setq depth (1+ depth))
          (add-face-text-property i (1+ i) (monad-repl--depth-face depth) t s))
         ((memq ch '(?\) ?\]))
          (add-face-text-property i (1+ i) (monad-repl--depth-face depth) t s)
          (setq depth (max 0 (1- depth)))))))
    (let ((i 0))
      (while (string-match "\\(::\\|->\\)" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'font-lock-builtin-face t s)
        (setq i (match-end 0))))
    (let ((i 0))
      (while (string-match "\\_<_\\_>" s i)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'shadow t s)
        (setq i (match-end 0))))
    s))

(defun monad-repl--propertize-sig-with-active (sig active-idx)
  "Return SIG propertized with parameter at ACTIVE-IDX highlighted.
Pass -1 to put all params in default face."
  (let ((s (monad-repl--propertize-sig sig))
        (spans nil)
        (i 0))
    ;; Named params: [name :: Type]
    (while (string-match "\\[\\([a-z_][A-Za-z0-9_']*\\)[ \t]*::" sig i)
      (push (cons (match-beginning 1) (match-end 1)) spans)
      (setq i (match-end 0)))
    (if spans
        ;; Named params path
        (progn
          (setq spans (nreverse spans))
          (cl-loop for span in spans
                   for idx from 0
                   do (add-face-text-property
                       (car span) (cdr span)
                       (if (= idx active-idx)
                           'font-lock-variable-name-face
                         'default)
                       nil s)))
      ;; Builtin _ path — highlight each _ slot positionally
      (let ((i 0) (idx 0))
        (while (string-match "\\_<_\\_>" s i)
          (add-face-text-property (match-beginning 0) (match-end 0)
                                  (if (= idx active-idx)
                                      'font-lock-variable-name-face
                                    'shadow)
                                  nil s)
          (setq i (match-end 0)
                idx (1+ idx)))))

    s))

(defun monad-repl--point-after-symbol-p ()
  "Return non-nil when point is at the trailing edge of a symbol."
  (and (> (point) (point-min))
       (let ((syn (char-syntax (char-before))))
         (or (eq syn ?w) (eq syn ?_)))))

(defun monad-repl--point-is-fn-name-p ()
  "Return non-nil when point is ON the function-name token of a (lisp call)."
  (save-excursion
    (condition-case nil
        (let ((orig (progn
                      (skip-chars-forward "A-Za-z0-9_'!?$%&*/+<=>.^~-")
                      (skip-chars-backward "A-Za-z0-9_'!?$%&*/+<=>.^~-")
                      (point))))
          (up-list -1)
          (when (eq (char-after) ?\()
            (forward-char 1)
            (skip-chars-forward " \t\n")
            (= (point) orig)))
      (error nil))))

(defun monad-repl--point-on-arg-p ()
  "Return non-nil when point is on a symbol in argument position (lisp)."
  (when (symbol-at-point)
    (let* ((sym-start (save-excursion
                        (skip-chars-backward "A-Za-z0-9_'!?$%&*/+<=>.^~-")
                        (point)))
           (fn-end (save-excursion
                     (condition-case nil
                         (progn
                           (let ((found nil))
                             (while (not found)
                               (up-list -1)
                               (when (eq (char-after) ?\()
                                 (setq found t)))
                             (forward-char 1)
                             (skip-chars-forward " \t\n")
                             (skip-chars-forward "A-Za-z0-9_'!?$%&*/+<=>.^~-")
                             (point)))
                       (error nil)))))
      (and fn-end (> sym-start fn-end)))))

(defun monad-repl--current-arg-index ()
  "Return zero-based argument index at point inside a (lisp call) form, or nil."
  (save-excursion
    (condition-case nil
        (let ((orig (point)))
          (up-list -1)
          (forward-char 1)
          (skip-chars-forward " \t\n")
          (condition-case nil
              (forward-sexp 1)
            (error (error "No-fn")))
          (catch 'done
            (let ((idx 0))
              (while (and (< (point) orig) (not (eobp)))
                (skip-chars-forward " \t\n")
                (when (>= (point) orig) (throw 'done idx))
                (condition-case nil
                    (progn (forward-sexp 1)
                           (when (<= (point) orig)
                             (setq idx (1+ idx))))
                  (error (throw 'done idx))))
              idx)))
      (error nil))))

(defun monad-repl--enclosing-fn-name ()
  "Return the function name of the enclosing (lisp call) form, or nil."
  (save-excursion
    (condition-case nil
        (progn
          (up-list -1)
          (forward-char 1)
          (skip-chars-forward " \t\n")
          (when (looking-at "\\(?:\\sw\\|\\s_\\|[!?$%&*/+<=>.^~-]\\)+")
            (match-string-no-properties 0)))
      (error nil))))

;; Haskell-style (no-paren) call detection
;; For expressions like: sum xs | ap f '(1 2 3)
(defun monad-repl--haskell-line-start ()
  "Return the position where Haskell-style scanning should begin.
That is the process-mark (start of prompt input) if we are inside one,
otherwise the beginning of the current line."
  (let* ((proc  (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (if (and pmark (>= (point) pmark))
        (marker-position pmark)
      (line-beginning-position))))

(defun monad-repl--haskell-tokens ()
  "Return a list of (START . END) for whitespace-separated tokens on
the current Haskell-style call line, starting from the prompt mark.
Parenthesised sub-expressions are treated as a single token.
Returns nil if the line looks like a lisp call (starts with '(')."
  (save-excursion
    (let ((lstart (monad-repl--haskell-line-start))
          tokens)
      (goto-char lstart)
      (skip-chars-forward " \t")
      ;; If the expression starts with '(' it is a lisp call — bail out.
      (if (eq (char-after) ?\()
          nil
        (while (and (not (eolp)) (< (point) (point-max)))
          (skip-chars-forward " \t")
          (unless (eolp)
            (let ((tok-start (point)))
              (cond
               ;; Parenthesised / bracketed group — skip as one token
               ((memq (char-after) '(?\( ?\[ ?\{))
                (condition-case nil (forward-sexp 1) (error (goto-char (point-max)))))
               ;; Quoted form
               ((eq (char-after) ?\')
                (forward-char 1)
                (condition-case nil (forward-sexp 1) (error (goto-char (point-max)))))
               ;; String literal
               ((eq (char-after) ?\")
                (condition-case nil (forward-sexp 1) (error (goto-char (point-max)))))
               ;; Operator / symbol
               (t
                (skip-chars-forward "^ \t\n")))
              (push (cons tok-start (point)) tokens))))
        (nreverse tokens)))))

(defun monad-repl--haskell-fn-and-arg ()
  "For a Haskell-style call on the current line, return (FN-NAME . ARG-IDX).
FN-NAME is the first token that resolves to a known function.
ARG-IDX is zero-based index of the argument slot at point (not counting the
function name).  -1 means point is ON the function name itself.
Returns nil if we are not inside a recognisable Haskell-style call."
  (catch 'haskell-done
    (let ((tokens (monad-repl--haskell-tokens)))
      (unless tokens (throw 'haskell-done nil))
      (let* ((pt (point))
             ;; Find the last token whose start is <= pt.
             ;; That is the token we are "in or just after".
             ;; We also note whether pt falls strictly inside it.
             (last-start-idx nil)
             (point-inside   nil))
        (cl-loop for (s . e) in tokens
                 for i from 0
                 when (<= s pt)
                 do (setq last-start-idx i
                          point-inside   (< pt e)))
        ;; If point is before the very first token, bail.
        (when (null last-start-idx)
          (throw 'haskell-done nil))
        ;; tok-index: which "slot" is point in?
        ;;   inside token i  -> slot i   (on that token)
        ;;   past   token i  -> slot i+1 (gap = next argument position)
        (let ((tok-index (if point-inside
                             last-start-idx
                           (1+ last-start-idx))))
          ;; Walk tokens left-to-right for the first known function.
          (cl-loop for (s . e) in tokens
                   for fn-idx from 0
                   for candidate = (buffer-substring-no-properties s e)
                   for info = (monad-repl--lookup candidate)
                   when (and info (member (nth 0 info) '("func" "builtin")))
                   return (cond
                           ;; Point is on the function name token itself
                           ((and point-inside (= fn-idx tok-index))
                            (cons candidate -1))
                           ;; Point is on or past an argument token
                           (t
                            (let ((arg-idx (- tok-index fn-idx 1)))
                              (when (>= arg-idx 0)
                                (cons candidate arg-idx)))))))))))

;;; Lookup / formatting helpers

(defun monad-repl--lookup (name)
  "Return (kind sig doc) for NAME from the symbol table, or nil."
  (and monad-repl--symbol-table
       (gethash name monad-repl--symbol-table)))

(defun monad-repl--format-display-sig (name info)
  "Return a display string for NAME given INFO (kind sig doc)."
  (let* ((kind (nth 0 info))
         (sig  (nth 1 info))
         (doc  (nth 2 info))
         (doc-part (when (and doc (not (string-empty-p doc)))
                     (propertize (concat "\"" (car (split-string doc "\n")) "\"")
                                 'face 'font-lock-doc-face))))
    (cond
     ((string= kind "var")
      (let* ((bracket (if (and sig (not (string-empty-p sig)))
                          (format "[%s :: %s]" name sig)
                        (format "[%s]" name)))
             (s (monad-repl--propertize-sig bracket)))
        (add-face-text-property 1 (1+ (length name))
                                'font-lock-variable-name-face t s)
        (concat s (when doc-part (concat " " (car (split-string doc-part "\n")))))))
     ((string= kind "keyword")
      (propertize name 'face 'font-lock-keyword-face))
     ((string= kind "builtin")
      (let* ((sig-part (when (and sig (not (string-empty-p sig)))
                         (monad-repl--propertize-sig
                          (replace-regexp-in-string "^Fn ?" (concat (upcase-initials name) " ") sig)))))
        (when sig-part
          (add-face-text-property 0 (length name) 'font-lock-function-name-face nil sig-part))
        (concat (or sig-part (propertize name 'face 'font-lock-function-name-face))
                (when doc-part (concat " " (car (split-string doc-part "\n")))))))
     (t
      (concat (if (and sig (not (string-empty-p sig)))
                  (monad-repl--propertize-sig sig)
                (propertize name 'face 'font-lock-function-name-face))
              (when doc-part (concat " " (car (split-string doc-part "\n")))))))))

(defun monad-repl--hover-doc (name)
  "Return a propertized eldoc hover string for NAME."
  (when-let* ((info (monad-repl--lookup name)))
    (monad-repl--format-display-sig name info)))


(defun monad-repl--call-sig (fn-name arg-idx)
  "Return propertized signature for FN-NAME with ARG-IDX parameter highlighted."
  (when-let* ((info (monad-repl--lookup fn-name))
              (kind (nth 0 info))
              (sig  (nth 1 info)))
    (when (not (string-empty-p sig))
      (let* ((sig (if (string= kind "builtin")
                      (replace-regexp-in-string "^Fn" fn-name sig)
                    sig))
             (doc      (nth 2 info))
             (doc-part (when (and doc (not (string-empty-p doc)))
                         (propertize (concat "\"" (car (split-string doc "\n")) "\"")
                                     'face 'font-lock-doc-face)))
             (propertized (monad-repl--propertize-sig-with-active sig arg-idx)))
        (when (string= kind "builtin")
          (add-face-text-property 0 (length fn-name)
                                  'font-lock-function-name-face
                                  nil propertized))
        (concat propertized
                (when doc-part (concat " " doc-part)))))))

(defun monad-repl--eldoc-get-doc ()
  "Return propertized eldoc string for the current context, or nil.

Priority rules:
  1. Trailing edge of a symbol, not an arg, not the fn name → nil.
  2. Point ON the function-name token of a (lisp call) → full sig, params dimmed.
  3. Point ON an argument symbol (lisp) → hover for that symbol, fallback sig.
  4. Point in a gap inside a (lisp call) → sig with current param highlighted.
  5. Generic hover over any symbol at point (fallback for lisp).
  6. NEW — Haskell-style call on current line:
       point ON fn-name  → full sig, all params dimmed (-1).
       point ON argument → sig with that arg highlighted.
       hovering a symbol that is itself a known function → its hover doc."
  (let* ((sym      (symbol-at-point))
         (sym-name (and sym (symbol-name sym))))
    (cond
     ;; Rule 1: trailing edge with no useful context → silent
     ((and (monad-repl--point-after-symbol-p)
           (not (monad-repl--point-on-arg-p))
           (not (monad-repl--point-is-fn-name-p))
           (monad-repl--current-arg-index))
      nil)

     ;; Rule 2: ON the function name of a lisp call → full sig, params dimmed
     ((monad-repl--point-is-fn-name-p)
      (when sym-name
        (monad-repl--call-sig sym-name -1)))

     ;; Rule 3: ON an argument symbol in a lisp call → hover / call sig
     ((monad-repl--point-on-arg-p)
      (when sym-name
        (or (monad-repl--hover-doc sym-name)
            (let ((arg-idx (monad-repl--current-arg-index))
                  (fn-name (monad-repl--enclosing-fn-name)))
              (when (and fn-name arg-idx)
                (monad-repl--call-sig fn-name arg-idx))))))

     ;; Rule 4: gap inside a lisp call → highlighted parameter
     ((monad-repl--current-arg-index)
      (let* ((arg-idx (monad-repl--current-arg-index))
             (fn-name (when arg-idx (monad-repl--enclosing-fn-name))))
        (when (and fn-name arg-idx)
          (monad-repl--call-sig fn-name arg-idx))))

     ;; Rule 5: no lisp call context → try generic hover
     (sym-name
      (or (monad-repl--hover-doc sym-name)
          ;; Rule 6 – Haskell-style call
          (when-let* ((fa (monad-repl--haskell-fn-and-arg)))
            (let ((fn-name (car fa))
                  (arg-idx (cdr fa)))
              (if (= arg-idx -1)
                  ;; Point is on the function name itself
                  (monad-repl--call-sig fn-name -1)
                (monad-repl--call-sig fn-name arg-idx))))))

     ;; Rule 6 fallback: no symbol at point but still on a Haskell call line
     (t
      (when-let* ((fa (monad-repl--haskell-fn-and-arg)))
        (let ((fn-name (car fa))
              (arg-idx (cdr fa)))
          (if (= arg-idx -1)
              (monad-repl--call-sig fn-name -1)
            (monad-repl--call-sig fn-name arg-idx))))))))

(defun monad-repl--message-at-window-start (msg &rest args)
  "Display MSG aligned with left edge of selected window."
  (message "%s%s"
           (make-string (floor (window-pixel-left (selected-window))
                               (max 1 (frame-char-width)))
                        ?\s)
           (apply #'format-message msg args)))

(defun monad-repl--eldoc-function (callback &rest _)
  "Eldoc backend for `monad-repl-mode'."
  (when (monad-repl-running-p)
    (when-let* ((doc (monad-repl--eldoc-get-doc)))
      (let ((aligned-doc (with-temp-buffer
                           (monad-repl--message-at-window-start doc)
                           (buffer-string))))
        (funcall callback aligned-doc)
        t))))

;;; ERROR Buttonization

(defun monad-repl--buttonize-errors (output)
  "Buttonize <input>:LINE:COL: error: lines in OUTPUT like `compilation-mode'."
  (let ((result output)
        (start 0))
    ;; Process all matches in the string
    (while (and (or (string-search "<input>:" result start)
                   (string-search ".mon:" result start))
               (string-match "\\(<input>\\|[^:\n]+\\.mon\\):\\([0-9]+\\):\\([0-9]+\\):\\([^\n]*\\)" result start))
      (let* ((location   (match-string 1 result))
             (line       (string-to-number (match-string 2 result)))
             (col        (string-to-number (match-string 3 result)))
             (rest       (match-string 4 result))
             (full-start (match-beginning 0))
             (full-end   (match-end 0))
             (keymap     (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1] #'monad-repl--jump-to-error)
                           (define-key map (kbd "RET") #'monad-repl--jump-to-error)
                           map))
             (props      `(monad-repl-error-location ,location
                           monad-repl-error-line ,line
                           monad-repl-error-col  ,col
                           mouse-face            highlight
                           help-echo             "mouse-1: go to error position"
                           keymap                ,keymap))
             (error-type (if (string-match "warning" rest)
                            'compilation-warning
                          'compilation-error))
             (before (substring result 0 full-start))
             (after (substring result full-end))
             (buttonized (concat
                         (apply #'propertize location
                                'face (list error-type :underline t) props)
                         (apply #'propertize ":"
                                'face '(:underline t) props)
                         (apply #'propertize (number-to-string line)
                                'face '(compilation-line-number :underline t) props)
                         (apply #'propertize ":"
                                'face '(:underline t) props)
                         (apply #'propertize (number-to-string col)
                                'face '(compilation-column-number :underline t) props)
                         (apply #'propertize ":" 'face 'default props)
                         (apply #'propertize rest 'face 'default props))))
        ;; Reconstruct result with this match buttonized, and continue searching after it
        (setq result (concat before buttonized after)
              start (+ (length before) (length buttonized)))))
    result))

(defun monad-repl--lerp-color (from to step steps)
  "Linearly interpolate FROM color toward TO at STEP out of STEPS."
  (let* ((parse  (lambda (c)
                   (list (string-to-number (substring c 1 3) 16)
                         (string-to-number (substring c 3 5) 16)
                         (string-to-number (substring c 5 7) 16))))
         (fc     (funcall parse from))
         (tc     (funcall parse to))
         (t-     (/ (float step) steps))
         (r      (round (+ (nth 0 fc) (* (- (nth 0 tc) (nth 0 fc)) t-))))
         (g      (round (+ (nth 1 fc) (* (- (nth 1 tc) (nth 1 fc)) t-))))
         (b      (round (+ (nth 2 fc) (* (- (nth 2 tc) (nth 2 fc)) t-)))))
    (format "#%02x%02x%02x" r g b)))

(defun monad-repl--pulse-error-region (beg end)
  "Fade BEG..END from error colors to background.
Like `pulse' but interpolates both foreground and background."
  (let* ((ov      (make-overlay beg end))
         (steps   pulse-iterations)
         (delay   pulse-delay)
         (fg-from (face-attribute 'monad-repl-error-highlight :foreground nil t))
         (bg-from (face-attribute 'monad-repl-error-highlight :background nil t))
         (fg-to   (face-attribute 'default :foreground nil t))
         (bg-to   (face-attribute 'default :background nil t))
         (step    0))
    (overlay-put ov 'face `(:foreground ,fg-from :background ,bg-from))
    (letrec ((timer
              (run-with-timer
               delay delay
               (lambda ()
                 (if (>= step steps)
                     (progn (delete-overlay ov)
                            (cancel-timer timer))
                   (overlay-put ov 'face
                                `(:foreground ,(monad-repl--lerp-color fg-from fg-to step steps)
                                  :background ,(monad-repl--lerp-color bg-from bg-to step steps)))
                   (setq step (1+ step)))))))
      timer)))

(defun monad-repl--jump-to-error ()
  "Jump to the error position referenced by the button at point."
  (interactive)
  (let ((location (get-text-property (point) 'monad-repl-error-location))
        (line (get-text-property (point) 'monad-repl-error-line))
        (col  (get-text-property (point) 'monad-repl-error-col)))
    (when (and location line col)
      (if (string= location "<input>")
          ;; Jump to REPL input
          (progn
            (comint-previous-prompt 1)
            (forward-line (1- line))
            (beginning-of-line)
            (forward-char (1- col)))
        ;; Jump to file
        (find-file-other-window location)
        (goto-char (point-min))
        (forward-line (1- line))
        (beginning-of-line)
        (forward-char (1- col)))

      ;; Skip past comment lines and blank lines to find real code
      (while (and (not (eobp))
                  (or (looking-at "^\\s-*$")
                      (looking-at "^\\s-*;")))
        (forward-line 1)
        (beginning-of-line))
      (skip-chars-forward "^A-Za-z0-9_'!?$%&*/+<=>.^~-")
      (let* ((beg (point))
             (end (save-excursion
                    (skip-chars-forward "A-Za-z0-9_'!?$%&*/+<=>.^~-")
                    (if (= (point) beg) (1+ beg) (point)))))
        (monad-repl--pulse-error-region beg end)))))

(defvar-local monad-repl--last-error-pos nil
  "Position of the last error jumped to.")

(defun monad-repl--error-positions ()
  "Return a sorted list of all error button start positions in the buffer."
  (let ((positions nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'monad-repl-error-line)))
        (if next
            (progn
              (when (get-text-property next 'monad-repl-error-line)
                (push next positions))
              (setq pos next))
          (setq pos (point-max)))))
    (nreverse positions)))

(defun monad-repl--next-error (&optional n)
  "Jump to the Nth next error in the REPL buffer."
  (interactive "p")
  (let* ((n (or n 1))
         (positions (monad-repl--error-positions))
         (last monad-repl--last-error-pos)
         (candidates (if last
                         (seq-filter (lambda (p) (> p last)) positions)
                       positions)))
    (if (< n (length candidates))
        (let ((target (nth (1- n) candidates)))
          (setq monad-repl--last-error-pos target)
          (goto-char target)
          (monad-repl--jump-to-error))
      (if candidates
          (let ((target (car candidates)))
            (setq monad-repl--last-error-pos target)
            (goto-char target)
            (monad-repl--jump-to-error))
        (user-error "No more errors")))))

(defun monad-repl--prev-error (&optional n)
  "Jump to the Nth previous error in the REPL buffer."
  (interactive "p")
  (let* ((n (or n 1))
         (positions (monad-repl--error-positions))
         (last monad-repl--last-error-pos)
         (candidates (if last
                         (reverse (seq-filter (lambda (p) (< p last)) positions))
                       (reverse positions))))
    (if candidates
        (let ((target (nth (1- (min n (length candidates))) candidates)))
          (setq monad-repl--last-error-pos target)
          (goto-char target)
          (monad-repl--jump-to-error))
      (user-error "No previous errors"))))

;;; ANSI color support

(defun monad-repl--ansi-color-filter (string)
  "Filter ANSI color codes from STRING."
  (let ((inhibit-read-only t))
    (ansi-color-apply string)))

(defun monad-repl--setup-ansi-colors ()
  "Set up ANSI color support for the REPL."
  (setq-local ansi-color-for-comint-mode t)
  (add-hook 'comint-preoutput-filter-functions
            #'monad-repl--ansi-color-filter nil t))


(defun monad-repl--narrow-to-prompt ()
  "Narrow to active prompt input region, return t if non-empty."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (when pmark
      (let* ((start (marker-position pmark))
             (end (point-max)))
        (when (> end start)
          (narrow-to-region start end)
          t)))))

(defun monad-repl--wrap-fontify-region (beg end &optional loudly)
  "Fontify region between BEG and END.
If LOUDLY is non-nil, print status messages during fontification."
  (save-restriction
    (widen)
    (font-lock-default-fontify-region beg end loudly)))

(defun monad-repl--wrap-unfontify-region (beg end &optional _loudly)
  "Unfontify only within the active prompt, never past output.
BEG and END are the region boundaries."
  (save-restriction
    (when (monad-repl--narrow-to-prompt)
      (let ((font-lock-dont-widen t)
            (beg (max beg (point-min)))
            (end (min end (point-max))))
        (when (< beg end)
          (font-lock-default-unfontify-region beg end))))))

;;; Xref backend

(defun monad-repl-xref-backend ()
  "Xref backend for Monad REPL buffers."
  'monad-repl)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql monad-repl)))
  "Return the symbol at point in the REPL."
  (when-let* ((sym (symbol-at-point)))
    (symbol-name sym)))

(cl-defmethod xref-backend-definitions ((_backend (eql monad-repl)) symbol)
  "Find definitions of SYMBOL, checking open buffers first then core."
  (require 'xref)
  (let (results)
    ;; 0. FFI symbol — jump directly to the C header
    (let* ((info (monad-repl--lookup symbol))
           (hdr  (and info (nth 3 info))))
      (when (and hdr (not (string-empty-p hdr)) (file-exists-p hdr))
        (with-current-buffer (find-file-noselect hdr)
          (save-excursion
            (goto-char (point-min))
            (let* ((c-types-re "\\(?:void\\|int\\|unsigned\\|char\\|float\\|double\\|long\\|short\\|bool\\|size_t\\|uint\\(?:8\\|16\\|32\\|64\\)_t\\|int\\(?:8\\|16\\|32\\|64\\)_t\\|RLAPI\\s-+\\w+\\|RLAPI\\)")
                   (sym-re (regexp-quote symbol))
                   (declaration-re (concat "^[^*\n]*" c-types-re "\\s-+" sym-re "\\s-*("))
                   (fallback-re    (concat "^[^*\n]*[A-Z]\\w+\\s-+" sym-re "\\s-*(")))
              (or (re-search-forward declaration-re nil t)
                  (re-search-forward fallback-re nil t))
              (push (xref-make symbol
                               (xref-make-buffer-location
                                (current-buffer)
                                (match-beginning 0)))
                    results))))))
    ;; 1. Search all open monad-mode buffers first
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (eq major-mode 'monad-mode)
                   (buffer-file-name))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    (concat "^(define[ \t\n]+\\(?:"
                            "(\\(" (regexp-quote symbol) "\\)\\_>"
                            "\\|\\[\\(" (regexp-quote symbol) "\\)[ \t]*::"
                            "\\|\\(" (regexp-quote symbol) "\\)\\_>\\)")
                    nil t)
              (let ((pos (or (match-beginning 1)
                             (match-beginning 2)
                             (match-beginning 3))))
                (push (xref-make symbol
                                 (xref-make-buffer-location buf pos))
                      results)))))))
    ;; 2. Search core library if nothing found yet
    (when (null results)
      (let ((root "/usr/local/lib/monad"))
        (when (file-directory-p root)
          (dolist (file (directory-files-recursively root "\\.mon$"))
            (when (null results)
              (with-current-buffer (find-file-noselect file)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward
                    (concat "^(define[ \t\n]+\\(?:"
                            "(\\(" (regexp-quote symbol) "\\)\\_>"
                            "\\|\\[\\(" (regexp-quote symbol) "\\)[ \t]*::"
                            "\\|\\(" (regexp-quote symbol) "\\)\\_>\\)")
                          nil t)
                    (let ((pos (or (match-beginning 1)
                                   (match-beginning 2)
                                   (match-beginning 3))))
                      (push (xref-make symbol
                                       (xref-make-buffer-location
                                        (current-buffer) pos))
                            results))))))))))
    ;; 3. Also check if symbol is a module name
    (when (null results)
      (let ((root "/usr/local/lib/monad")
            (filename (concat symbol ".mon")))
        (when (file-directory-p root)
          (dolist (file (directory-files-recursively root "\\.mon$"))
            (when (string= (file-name-nondirectory file) filename)
              (with-current-buffer (find-file-noselect file)
                (push (xref-make (concat "module " symbol)
                                 (xref-make-buffer-location (current-buffer) 1))
                      results)))))))
    (nreverse results)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql monad-repl)))
  "Return completion candidates for xref in the REPL."
  (monad-repl--send-completion-request ""))

(defun monad-repl--xref-show-definitions (symbol)
  "Jump to definition of SYMBOL, opening result in a sensible window."
  (let ((xref-show-xrefs-function
         (lambda (fetcher alist)
           (let ((xrefs (funcall fetcher)))
             (if (= (length xrefs) 1)
                 (xref-pop-to-location (car xrefs) 'window)
               (xref--show-xref-buffer fetcher alist)))))
        (xref-show-definitions-function
         (lambda (fetcher alist)
           (let ((xrefs (funcall fetcher)))
             (if (= (length xrefs) 1)
                 (xref-pop-to-location (car xrefs) 'window)
               (xref--show-xref-buffer fetcher alist))))))
    (xref-find-definitions symbol)))

(defun monad-repl-find-definition ()
  "Jump to definition of symbol at point from the REPL."
  (interactive)
  (if-let* ((sym (symbol-at-point)))
      (monad-repl--xref-show-definitions (symbol-name sym))
    (user-error "No symbol at point")))

;;; Mode definition

(defun monad-repl--output-newline (output)
  "Add a blank line after each REPL OUTPUT chunk, when prompt is hidden."
  (if monad-repl-show-prompt
      output
    (concat output "\n")))

(defun monad-repl--indent-line ()
  "Indent current line in the REPL prompt with 2-space indentation."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (when (and pmark (>= (point) pmark))
      (let ((pos (- (point-max) (point))))
        (beginning-of-line)
        (when (> (point) pmark)
          (let ((indent (save-excursion
                          (condition-case nil
                              (progn
                                (up-list -1)
                                (+ (current-column) 2))
                            (error 0)))))
            (delete-horizontal-space)
            (indent-to indent)))
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))))))

(defun monad-repl--tab-command ()
  "Indent line or complete. If already indented, show all completions."
  (interactive)
  (let* ((col (current-column))
         (expected (save-excursion
                     (monad-repl--indent-line)
                     (current-column))))
    (if (= col expected)
        ;; Already at correct indentation — complete with empty prefix
        (let ((bounds (monad-repl--completion-bounds)))
          (if bounds
              (completion-at-point)
            ;; No symbol at point — force all completions
            (let ((monad-repl--force-all-completions t))
              (completion-at-point))))
      ;; Not yet indented — indent
      (monad-repl--indent-line))))

(defun monad-repl--space-or-skip ()
  "Insert space, or skip past closing backtick if point is before one."
  (interactive)
  (if (eq (char-after) ?`)
      (progn (forward-char 1) (insert ?\s))
    (insert ?\s)))

(defun monad-repl--post-self-insert ()
  "Handle post-insertion actions in the REPL for import completion."

  ;; Trigger completion after import/include
  (when (and (eq last-command-event ?\s)
             (or (looking-back "(import +" (line-beginning-position))
                 (looking-back "\\bimport +" (line-beginning-position))
                 (looking-back "\\binclude +" (line-beginning-position))))
    (completion-at-point)))

(define-derived-mode monad-repl-mode comint-mode "Monad-REPL"
  "Major mode for interacting with a Monad REPL.

\\{monad-repl-mode-map}"
  :group 'monad-repl

  ;; Define comment syntax explicitly
  (modify-syntax-entry ?\; "<" (syntax-table))  ; ; starts comment
  (modify-syntax-entry ?\n ">" (syntax-table))  ; newline ends comment
  (modify-syntax-entry ?\r ">" (syntax-table))  ; carriage return ends comment

  ;; TODO Add syntax highlighting for >>>
  ;; Add explicit comment font-lock rules
  (setq-local font-lock-defaults
              '((("\\(;.*\\)$" . font-lock-comment-face)
                 ;; Include other font-lock rules from monad-mode if needed
                 )
                nil nil nil nil
                (font-lock-comment-face . font-lock-comment-face)))

  ;; Comint settings
  (setq-local comint-prompt-regexp monad-repl-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil)    ; nil to allow "hello" => "hello"
  (setq-local comint-use-prompt-regexp nil) ; nil to allow multiline
  (setq-local comint-last-input-start (point-max-marker))

  ;; Input history
  (setq-local comint-input-ignoredups t)
  (setq-local comint-input-ring-size 1000)

  (setq-local indent-line-function #'monad-repl--indent-line)
  (setq-local tab-always-indent 'complete)

  ;; Initialize completion cache
  (setq monad-repl--completion-cache (make-hash-table :test 'equal))

  ;; Set up completion
  (add-hook 'completion-at-point-functions
            #'monad-repl--special-completion-at-point nil t)

  (add-hook 'completion-at-point-functions
            #'monad-repl-completion-at-point nil t)

  (add-hook 'post-self-insert-hook #'monad-repl--post-self-insert nil t)
  (monad--setup-electric-pair)

  ;; Clear cache on user input
  (add-hook 'comint-input-filter-functions
            (lambda (_input)
              (monad-repl--clear-completion-cache)
              nil) ; Return nil to continue normal processing
            nil t)

  (add-hook 'comint-preoutput-filter-functions
            #'monad-repl--buttonize-errors nil t)

  ;; ANSI colors
  (monad-repl--setup-ansi-colors)

  (add-hook 'comint-preoutput-filter-functions
            #'monad-repl--output-newline nil t)

  ;; Font lock - inherit from monad-mode if available
  (when (featurep 'monad-mode)
    (setq-local font-lock-defaults
                (with-current-buffer (get-buffer-create " *monad-mode-temp*")
                  (monad-mode)
                  (prog1 font-lock-defaults
                    (kill-buffer)))))

  (setq-local font-lock-fontify-region-function
              #'monad-repl--wrap-fontify-region)
  (setq-local font-lock-unfontify-region-function
              #'monad-repl--wrap-unfontify-region)
  ;; Limit font-lock to avoid hanging on large output blocks
  (setq-local font-lock-maximum-size nil)
  (setq-local jit-lock-chunk-size 500)


  ;; Xref
  (add-hook 'xref-backend-functions #'monad-repl-xref-backend nil t)

  ;; Eldoc
  (setq-local eldoc-documentation-functions '(monad-repl--eldoc-function))
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-default)
  (eldoc-mode 1)
  (run-with-idle-timer 1 nil #'monad-repl--refresh-symbol-table)

  ;; Populate symbol table eagerly so eldoc works immediately
  (run-with-idle-timer 1 nil #'monad-repl--refresh-symbol-table)

  ;; Welcome message
  (unless (comint-check-proc (current-buffer))
    (let ((inhibit-read-only t))
      (insert (format "Starting Monad REPL: %s %s\n"
                     monad-repl-program
                     (mapconcat #'identity monad-repl-arguments " "))))))

;;; Integration with monad-mode

;;;###autoload
(defun monad-repl-setup-keys ()
  "Set up REPL keybindings in `monad-mode-map'.
This should be called from `monad-mode' initialization."
  (when (boundp 'monad-mode-map)
    (define-key monad-mode-map (kbd "C-c C-z") #'monad-repl)
    (define-key monad-mode-map (kbd "C-c C-r") #'monad-repl-eval-region)
    (define-key monad-mode-map (kbd "C-c C-b") #'monad-repl-eval-buffer)
    (define-key monad-mode-map (kbd "C-x C-h") #'monad-repl-eval-defun)
    (define-key monad-mode-map (kbd "C-x C-e") #'monad-repl-eval-last-sexp)
    (define-key monad-mode-map (kbd "C-c C-l") #'monad-repl-load-file)
    (define-key monad-mode-map (kbd "C-c C-r") #'monad-repl-restart)))

;;;###autoload
(defun monad-repl-setup-hooks ()
  "Set up REPL hooks for `monad-mode'.
This should be called from `monad-mode' initialization."
  (add-hook 'after-save-hook #'monad-repl-load-on-save-hook nil t))

(provide 'monad-repl)

;;; monad-repl.el ends here
