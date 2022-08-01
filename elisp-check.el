;;; elisp-check.el --- Run Emacs Lisp checks for CI  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/elisp-check
;; Keywords: elisp, lint, lisp, test, tools
;; Package-Version: 1.1.0
;; Package-Requires: ((emacs "24.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is intended to be used in conjuction with the
;; elisp-check GitHub Action.  However, it is also possible to run
;; checks locally using the `elisp-check-run' function.
;;
;; Refer to the repository README.md for documentation.

;;; Code:

;;;; Variables

(defconst elisp-check-alist
  '(("melpa"
     :collection ("load-file" "byte-compile" "package-lint" "checkdoc"))
    ("load-file"
     :function elisp-check-load-file)
    ("byte-compile"
     :function elisp-check-byte-compile
     :require bytecomp)
    ("checkdoc"
     :function elisp-check-checkdoc
     :require checkdoc)
    ("package-lint"
     :function elisp-check-package-lint
     :require package-lint
     :package package-lint)
    ("ert"
     :function elisp-check-ert
     :require ert))
  "Alist from check names to definitions.")

(defvar elisp-check-ignore-warnings nil
  "Variable indicating whether warnings should be ignored.")

(defvar elisp-check-warnings-as-errors nil
  "Variable indicating whether to treat warnings as errors.")

(defvar elisp-check-has-failed nil
  "Variable indicating whether any checks have failed.")

;;;; Implementation

(defun elisp-check-run (name file-or-glob &optional install)
  "Run the check named NAME on entry file FILE-OR-GLOB.

When INSTALL is non-nil, also install all check and file
dependencies using the package.el package manager.

See `elisp-check-alist' for a list of valid check names."
  ;; Reset state
  (setq elisp-check-has-failed nil)
  ;; Install packages
  (when install
    (elisp-check--install-setup name))
  ;; Add repository to load-path
  (add-to-list 'load-path default-directory)
  ;; Require all explicit dependencies
  (mapc #'require (elisp-check--get-props name :require))
  ;; Run checker functions
  (let ((buffers (elisp-check--get-buffers file-or-glob))
        (check-funs (elisp-check--get-props name :function)))
    (unless buffers
      (elisp-check-error "File `%s' does not exist" file-or-glob))
    (unless check-funs
      (elisp-check-error "Check `%s' does not exist" name))
    (if install
        (elisp-check--apply
         buffers
         (cons #'elisp-check--install-package-requires check-funs))
      (elisp-check--apply buffers check-funs))
    ;; Exit
    (when elisp-check-has-failed
      (error "Some checks have failed"))))

(defun elisp-check--apply (buffers check-funs)
  "Apply the given CHECK-FUNS to the given BUFFERS."
  (dolist (buffer buffers)
    (with-current-buffer buffer
      (elisp-check-debug "Checking file: %s" (buffer-file-name))
      (let ((other (elisp-check--get-requires)))
        (dolist (check check-funs)
          (elisp-check-debug "Running check: %s" check)
          (apply check other))))))

(defun elisp-check--get-buffers (file-or-files)
  "Get a list of buffers for the given existing FILE-OR-FILES.
File globbing is supported."
  (let* ((files (elisp-check-listify file-or-files))
         (file-sets (mapcar #'file-expand-wildcards files))
         (files (apply #'append file-sets)))
    (mapcar #'find-file-noselect files)))

(defun elisp-check--get-checks (check)
  "Get a list of check plists for the given CHECK."
  (let* ((check (cons :name (assoc check elisp-check-alist)))
         (deps (plist-get check :collection))
         (dep-values (apply #'append (mapcar #'elisp-check--get-checks deps))))
    (cons check dep-values)))

(defun elisp-check--get-props (check prop)
  "Get a list of PROP properties for the given check CHECK."
  (let* ((checks (elisp-check--get-checks check))
         (fun (lambda (it)
                (elisp-check-listify
                 (plist-get it prop)))))
    (apply #'append (mapcar fun checks))))

(defun elisp-check--get-requires (&optional prefix known-buffers)
  "Return file buffers for local `require' statements in the current buffer.

Only return files in the same directory with the same PREFIX.
Then walk the resulting buffers for more `require' statements.
If PREFIX is not given, extract it from the current file name.

When a buffer is a member of KNOWN-BUFFERS, do not return or
search it for further `require' statements."
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (prefix (or prefix (file-name-sans-extension file)))
         (requires (elisp-check-parse "^[ ]*(require '\\(.*?\\))"))
         (fun (lambda (required)
                (elisp-check--get-require
                 required prefix
                 (cons (current-buffer) known-buffers)))))
    (delete-dups (apply #'append (mapcar fun requires)))))

(defun elisp-check--get-require (name prefix &optional known-buffers)
  "Return required buffers for file with package NAME.

See documentation of `elisp-check--get-requires' for
documentation on the usage of PREFIX and KNOWN-BUFFERS."
  (let ((file (concat name ".el")))
    (when (file-exists-p file)
      (if (string-prefix-p prefix name)
          (with-current-buffer (find-file-noselect file)
            (unless (member (current-buffer) known-buffers)
              (cons (current-buffer)
                    (elisp-check--get-requires prefix known-buffers))))
        (prog1 nil
          (elisp-check-emit
           'warning
           (format
            "Possibly using local file with different prefix `%s'"
            file)
           (buffer-file-name)))))))

(defun elisp-check--install-setup (check)
  "Setup package.el and install packages for the given CHECK."
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (package-refresh-contents)
  (elisp-check--install-packages (elisp-check--get-props check :package)))

(defun elisp-check--install-package-requires (&rest _other)
  "Install packages for Package-Requires for current buffer."
  (let ((pkgs (elisp-check--get-package-requires)))
    (elisp-check--install-packages pkgs)))

(defun elisp-check--get-package-requires ()
  "Get list of packages for Package-Requires for current buffer."
  (condition-case _error
      (let* ((parsed (elisp-check-parse "^;; Package-Requires: \\(.*\\)"))
             (conses (apply #'append (mapcar #'read parsed))))
        (delq 'emacs (mapcar #'car conses)))
    (error (elisp-check-error
            "The `Package-Requires' section for buffer `%s' is malformed"
            (current-buffer)))))

(defun elisp-check--install-packages (packages)
  "Install PKGS using the package.el package manager."
  (let ((errors '()))
    (dolist (package packages)
      (elisp-check-debug "Installing: %s" package)
      (condition-case error
          (package-install package)
        (error
         (push (elisp-check-format-error error) errors))))
    (when errors
      (elisp-check-error
       "Packages could not be installed:\n    %s"
       (mapconcat #'identity errors "\n    ")))))

;;;; Standard library

(defun elisp-check-emit (level message &optional file line column)
  "Emit a CI message for the given arguments.

MESSAGE is the message text, while LEVEL is a symbol corresponding to
a urgency level supported by the CI environment, such as warning
or error.  FILE, LINE and COLUMN may be given to associate the
message with a file.

When LEVEL is error, also set `elisp-check-has-failed'."
  (let* ((message (replace-regexp-in-string "\n" "\\\\n" message))
         (is-error (eq level 'error))
         (is-warning (eq level 'warning))
         (is-debug (eq level 'debug))
         (full (concat
                "::%s "
                (when file (format "file=%s," file))
                (when line (format "line=%s," line))
                (when column (format "col=%s," column))
                (format "::%s" message))))
    (cond
     ((and is-warning elisp-check-ignore-warnings))
     ((or is-error (and is-warning elisp-check-warnings-as-errors))
      (setq elisp-check-has-failed t)
      (message full 'error))
     ((or is-warning is-debug)
      (message full level))
     (t (elisp-check-error "Unsupported urgency level `%s'" level)))))

(defun elisp-check-debug (message &rest objects)
  "Emit a debug MESSAGE formatted with OBJECTS."
  (let ((format (apply #'format message objects)))
    (message "[ELISP-CHECK] %s" format)))

(defun elisp-check-error (message &rest objects)
  "Emit a CI error MESSAGE formatted with OBJECTS, then exit."
  (let ((format (apply #'format message objects)))
    (elisp-check-emit 'error format)
    (error format)))

(defun elisp-check-format-error (error)
  "Format the error ERROR in a visually pleasing manner."
  (let* ((symbol (car error))
         (data (cdr error))
         (message (get symbol 'error-message))
         (data-string (mapconcat #'prin1-to-string data ", ")))
    (if (eq symbol 'error)
        data-string
      (format "%s: %s" message data-string))))

(defun elisp-check-parse (regexp &optional handler)
  "Parse the current buffer for REGEXP.
HANDLER is then called for every match with the all captures as
its arguments.  The default value for HANDLER is `concat'."
  (let ((handler (or handler #'concat))
        (result '()))
    (save-excursion
      (save-match-data
        (goto-char 0)
        (while (re-search-forward regexp nil t)
          (let* ((len (1- (/ (length (match-data)) 2)))
                 (seq (number-sequence 1 len))
                 (captures (mapcar #'match-string-no-properties seq)))
            (push (apply handler captures) result)))
        result))))

(defun elisp-check-listify (value)
  "Return VAL if list, (list VAL) otherwise."
  (if (listp value)
      value
    (list value)))

;;;; Checkers

(defun elisp-check-load-file (&rest _other)
  "Run a `load-file' check on the current buffer."
  (condition-case error
      (load-file (buffer-file-name))
    (error (elisp-check-emit
            'error
            (elisp-check-format-error error)))))

(defun elisp-check-byte-compile (&rest other)
  "Run a `byte-compile-file' check on the current and OTHER buffers."
  (ad-activate 'byte-compile-log-warning)
  (let ((byte-compile-dest-file-function
         (lambda (_file)
           (file-name-directory (make-temp-file "bytecomp-")))))
    (byte-compile-file (buffer-file-name))
    (dolist (buffer other)
      (byte-compile-file (buffer-file-name buffer))))
  (ad-deactivate 'byte-compile-log-warning))

(defun elisp-check--byte-compile-emit (message &optional pos _fill level)
  "Emit a CI message for MESSAGE, POS and LEVEL.
This should be bound to `byte-compile-log-warning-function' in
order to hook `byte-compile-file' into the CI message mechanism."
  (save-excursion
    (goto-char pos)
    (elisp-check-emit
     (if (eq level :error) 'error 'warning)
     message
     byte-compile-current-file
     (line-number-at-pos)
     (current-column))))

;; Advice `byte-compile-log-warning' instead of using the proper
;; variable `byte-compile-log-warning-function' as it does not exist
;; for older versions of Emacs.

(defadvice byte-compile-log-warning
    (around elisp-check--advice-byte-compile-log disable)
  "Emit byte compile errors and warnings as CI messages."
  (elisp-check--byte-compile-emit
   string
   (or byte-compile-last-position 0)
   fill
   level))

;; Only enable the advice to `byte-compile-log-warning' when the
;; variable `byte-compile-log-warning-function' is not available,
;; otherwise use the proper interface.

(eval-after-load 'bytecomp
  '(if (boundp 'byte-compile-log-warning-function)
       (setq byte-compile-log-warning-function #'elisp-check--byte-compile-emit)
     (ad-enable-advice 'byte-compile-log-warning 'around 'elisp-check--advice-byte-compile-log)
     (ad-activate 'byte-compile-log-warning)))

(defun elisp-check-package-lint (&rest other)
  "Run a `package-lint-buffer' check on the current and OTHER buffers."
  (let ((package-lint-main-file (buffer-file-name)))
    (elisp-check--package-lint-buffer)
    (dolist (buffer other)
      (with-current-buffer buffer
        (elisp-check--package-lint-buffer)))))

(defun elisp-check--package-lint-buffer ()
  "Run a `package-lint-buffer' check on the current buffer."
  (dolist (lint (package-lint-buffer))
    (let ((level (nth 2 lint))
          (message (nth 3 lint))
          (file (buffer-file-name))
          (line (nth 0 lint))
          (column (nth 1 lint)))
      (elisp-check-emit level message file line column))))

(defun elisp-check-checkdoc (&rest other)
  "Run a `checkdoc' check on the current and OTHER buffers."
  (let ((checkdoc-autofix-flag 'never)
        (checkdoc-diagnostic-buffer
         (format "*%s doc errors*" (current-buffer))))
    (checkdoc-current-buffer t)
    (dolist (buffer other)
      (with-current-buffer buffer
        (checkdoc-current-buffer t)))
    ;; NOTE: If we expect directories to work, this will not!
    (with-current-buffer checkdoc-diagnostic-buffer
      (elisp-check-parse
       "\\(.*\\):\\(.*\\): \\(.*\\)"
       (lambda (file line message)
         (elisp-check-emit 'warning message file line)))
      (unless noninteractive
        (kill-buffer-and-window)))))

(defun elisp-check-ert (&rest _other)
  "Run a `ert' check on tests defined in the current buffer."
  (ert-delete-all-tests)
  (elisp-check-load-file)
  (let* ((stats (ert-run-tests-batch))
         (unexpected (ert-stats-completed-unexpected stats))
         (total (ert-stats-total stats)))
    (cond
     ((zerop total)
      (elisp-check-emit
       'error "No tests were defined"))
     ((not (zerop unexpected))
      (dolist (test (append (ert--stats-tests stats) nil))
        (let* ((name (ert-test-name test))
               (result (ert-test-most-recent-result test))
               (expected (ert-test-result-expected-p test result)))
          (when (not expected)
            (elisp-check-emit
             'error (format "Test `%s' failed" name)))))))))

(provide 'elisp-check)

;;; elisp-check.el ends here
