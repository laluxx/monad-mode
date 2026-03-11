;;; monad-repl.el --- REPL for Monad mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Laluxx
;; Maintainer: Laluxx
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages processes repl

;;; Commentary:

;; This package provides REPL (Read-Eval-Print Loop) support for monad-mode.
;; It uses comint-mode to provide an interactive Monad interpreter session
;; with features like:
;; - Interactive REPL with completion
;; - Automatic file loading on save
;; - Expression evaluation
;; - Region evaluation
;; - ANSI color support

;;; Code:

(require 'comint)
(require 'ansi-color)

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

;;; Internal variables

(defvar monad-repl-prompt-regexp "^Monad \xce\xbb "
  "Regexp to match the Monad REPL prompt.")

;; (defvar monad-repl-prompt-regexp "^Monad λ "
;;   "Regexp to match the Monad REPL prompt.")

(defvar monad-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "TAB") #'completion-at-point)
    (define-key map (kbd "RET") #'monad-repl-maybe-send)
    (define-key map (kbd "C-l") #'comint-clear-buffer)
    (define-key map (kbd "C-c C-l") #'monad-repl-load-file)
    (define-key map (kbd "C-c C-z") #'monad-repl-switch-back)
    (define-key map (kbd "M-.") #'monad-repl-find-definition)
    (define-key map (kbd "M-,") #'xref-go-back)
    map)
  "Keymap for `monad-repl-mode'.")

(defvar-local monad-repl--last-source-buffer nil
  "The last monad-mode buffer that switched to this REPL.")

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
  "Return the nesting depth of parens at point relative to the prompt."
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (process-mark proc))))
    (when pmark
      (save-excursion
        (let ((depth 0))
          (goto-char pmark)
          (while (< (point) (point-max))
            (let ((ch (char-after)))
              (cond ((memq ch '(?\( ?\[ ?\{)) (setq depth (1+ depth)))
                    ((memq ch '(?\) ?\] ?\})) (setq depth (1- depth)))))
            (forward-char 1))
          depth)))))

(defun monad-repl--newline-and-indent ()
  "Insert newline and indent within a multiline expression."
  (interactive)
  (save-restriction
    (narrow-to-region comint-last-input-start (point-max))
    (insert "\n")
    (lisp-indent-line)))

(defun monad-repl-maybe-send ()
  "Send input if sexp is balanced AND point is at end, otherwise newline and indent."
  (interactive)
  (let ((level (monad-repl--nesting-level)))
    (cond
     ((and level (> level 0))
      (monad-repl--newline-and-indent))
     ((= (point) (point-max))
      (let* ((proc  (get-buffer-process (current-buffer)))
             (pmark (process-mark proc))
             (input (buffer-substring-no-properties pmark (point-max))))
        (goto-char (point-max))
        (insert "\n")
        (comint-add-to-input-history input)
        (process-send-string proc (concat input "\n"))
        (set-marker pmark (point-max))))
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
  (when (and (monad-repl-running-p) prefix)
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

(defun monad-repl--parse-completion-output (output)
  "Parse completion OUTPUT from ,complete command.
Expected format:
__COMPLETIONS__
candidate1
candidate2
...
__END__"
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
            (push clean-line completions))))))
    (nreverse completions)))

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

(defun monad-repl-completion-at-point ()
  "Completion at point function for Monad REPL."
  (when (derived-mode-p 'monad-repl-mode)
    ;; Don't compete with import completion
    (let* ((line-start (line-beginning-position))
           (before (buffer-substring-no-properties line-start (point))))
      (unless (string-match "(import +" before)
        (let ((bounds (monad-repl--completion-bounds)))
          (when bounds
            (let* ((start (car bounds))
                   (end (cdr bounds))
                   (prefix (buffer-substring-no-properties start end)))
              (list start end
                    (completion-table-dynamic
                     (lambda (str)
                       (or (monad-repl--get-completions str) nil)))
                    :exclusive 'no
                    :company-doc-buffer #'ignore
                    :company-kind (lambda (_) 'variable)))))))))

;;;; Non protocol completions

(defun monad-repl--installed-modules ()
  "Return list of installed Monad module names from /usr/local/lib/monad."
  (let ((root "/usr/local/lib/monad")
        (modules nil))
    (when (file-directory-p root)
      (dolist (file (directory-files-recursively root "\\.mon$"))
        (push (file-name-sans-extension (file-name-nondirectory file))
              modules)))
    (nreverse modules)))

(defun monad-repl--import-completion-at-point ()
  "Complete module names after (import ."
  (when (derived-mode-p 'monad-repl-mode)
    ;; Check if we're in an (import ...) context
    (let* ((line-start (line-beginning-position))
           (before (buffer-substring-no-properties line-start (point))))
      (when (string-match "(import +\\([A-Za-z0-9_]*\\)$" before)
        (let ((start (+ line-start (match-beginning 1)))
              (end   (+ line-start (match-end 1))))
          (list start end
                (monad-repl--installed-modules)
                :exclusive 'yes
                :company-kind (lambda (_) 'module)))))))

(defun monad-repl--post-self-insert ()
  "Trigger completion after typing space following (import."
  (when (and (derived-mode-p 'monad-repl-mode)
             (eq last-command-event ?\s)
             (looking-back "(import +" (line-beginning-position)))
    (completion-at-point)))

;;; ANSI color support

(defun monad-repl--ansi-color-filter (string)
  "Filter ANSI color codes from STRING."
  (ansi-color-apply string))

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
  "Fontify region, but when called for prompt only fontify prompt area."
  (save-restriction
    (widen)
    (font-lock-default-fontify-region beg end loudly)))

(defun monad-repl--wrap-unfontify-region (beg end &optional loudly)
  "Unfontify only within the active prompt, never past output."
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
  "Add a blank line after each REPL output chunk, when prompt is hidden."
  (if monad-repl-show-prompt
      output
    (concat output "\n")))

(define-derived-mode monad-repl-mode comint-mode "Monad-REPL"
  "Major mode for interacting with a Monad REPL.

\\{monad-repl-mode-map}"
  :group 'monad-repl

  ;; Comint settings
  (setq-local comint-prompt-regexp monad-repl-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-process-echoes nil)    ; nil to allow "hello" => "hello"
  (setq-local comint-use-prompt-regexp nil) ; nil to allow multiline
  (setq-local comint-last-input-start (point-max-marker))

  ;; Input history
  (setq-local comint-input-ignoredups t)
  (setq-local comint-input-ring-size 1000)

  ;; Disable indentation
  (setq-local indent-line-function #'ignore)
  (setq-local tab-always-indent nil)

  ;; Initialize completion cache
  (setq monad-repl--completion-cache (make-hash-table :test 'equal))

  ;; Set up completion
  (add-hook 'completion-at-point-functions
            #'monad-repl--import-completion-at-point nil t)

  (add-hook 'completion-at-point-functions
            #'monad-repl-completion-at-point nil t)

  (add-hook 'post-self-insert-hook #'monad-repl--post-self-insert nil t)

  ;; Clear cache on user input
  (add-hook 'comint-input-filter-functions
            (lambda (_input)
              (monad-repl--clear-completion-cache)
              nil) ; Return nil to continue normal processing
            nil t)

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

  ;; Xref
  (add-hook 'xref-backend-functions #'monad-repl-xref-backend nil t)

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
This should be called from monad-mode initialization."
  (when (boundp 'monad-mode-map)
    (define-key monad-mode-map (kbd "C-c C-z") #'monad-repl)
    (define-key monad-mode-map (kbd "C-c C-r") #'monad-repl-eval-region)
    (define-key monad-mode-map (kbd "C-c C-b") #'monad-repl-eval-buffer)
    (define-key monad-mode-map (kbd "C-c C-e") #'monad-repl-eval-defun)
    (define-key monad-mode-map (kbd "C-x C-e") #'monad-repl-eval-last-sexp)
    (define-key monad-mode-map (kbd "C-c C-l") #'monad-repl-load-file)
    (define-key monad-mode-map (kbd "C-c C-c C-r") #'monad-repl-restart)))

;;;###autoload
(defun monad-repl-setup-hooks ()
  "Set up REPL hooks for monad-mode.
This should be called from monad-mode initialization."
  (add-hook 'after-save-hook #'monad-repl-load-on-save-hook nil t))

(provide 'monad-repl)

;;; monad-repl.el ends here
