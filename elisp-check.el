;;; elisp-check.el --- Run elisp checks for CI  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: elisp, lint, lisp, test, tools
;; Package-Version: 0.1.0
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

(defun elisp-check-run (expr file-or-glob &optional install)
  "Run the given check EXPR with entry file FILE-OR-GLOB.
When INSTALL is non-nil, also install all test and file
dependencies using the package.el package manager."
  ;; Install packages
  (when install
    (elisp-check--install-setup expr))
  ;; Add repository to load-path
  (add-to-list 'load-path default-directory)
  ;; Require all explicit dependencies
  (mapc #'require (elisp-check-get-props expr :require))
  ;; Run checker functions
  (let ((buffers (elisp-check-get-buffers file-or-glob))
        (check-funs (elisp-check-get-props expr :function)))
    (unless buffers
      (elisp-check-error "File `%s' does not exist." file-or-glob))
    (unless check-funs
      (elisp-check-error "Check `%s' does not exist." expr))
    (when install
      (elisp-check--apply
       buffers
       (list #'elisp-check--install-requires)))
    (elisp-check--apply buffers check-funs)))

(defun elisp-check--apply (buffers check-funs)
  "Apply the given CHECK-FUNS to the given BUFFERS."
  (dolist (buffer buffers)
    (with-current-buffer buffer
      (let ((other (elisp-check--get-requires)))
        (dolist (check check-funs)
          (elisp-check-debug "Running check: %s" check)
          (apply check other))))))

(defun elisp-check--install-setup (expr)
  "Setup package.el and install packages for the given EXPR."
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-refresh-contents)
  (mapc #'package-install (elisp-check-get-props expr :package)))

(defun elisp-check--install-requires (&rest _other)
  "Install packages for Package-Requires for buffers."
  (let* ((parsed (elisp-check-parse ";; Package-Requires: \\((.*)\\)"))
         (reqs (apply #'append (mapcar #'read parsed)))
         (pkgs (mapcar #'car reqs)))
    (dolist (pkg pkgs)
      (unless (eq pkg 'emacs)
        (elisp-check-debug "Installing: %s" pkg)
        (condition-case err
            (package-install pkg)
          (error
           (elisp-check-emit
            'error "Package `%s' could not be installed:\n    %s"
            pkg (elisp-check-format-error err))))))))

(defun elisp-check-get-buffers (file-or-files)
  "Get a list of buffers for the given FILE-OR-FILES.
File globbing is supported."
  (let* ((files (elisp-check--listify file-or-files))
         (file-sets (mapcar #'file-expand-wildcards files))
         (files (apply #'append file-sets)))
    (mapcar #'find-file-noselect files)))

(defun elisp-check--get-requires ()
  "Return local files for `require' statements in the current buffer.
Only files in the same directory are returned."
  (let* ((values (elisp-check-parse "^[ ]*(require '\\(.*?\\))"))
         (result '()))
    (dolist (val values)
      (let ((file (concat val ".el")))
        (when (file-exists-p file)
          (push (find-file-noselect file) result))))
    result))

(defun elisp-check-get-checks (expr)
  "Get a list of check plists for the given check EXPR."
  (let* ((check (cons :name (assoc expr elisp-check-alist)))
         (deps (plist-get check :collection))
         (dep-values (apply #'append (mapcar #'elisp-check-get-checks deps))))
    (cons check dep-values)))

(defun elisp-check-get-props (expr prop)
  "Get a list of PROP properties for the given check EXPR."
  (let* ((checks (elisp-check-get-checks expr))
         (fun (lambda (it)
                (elisp-check--listify
                 (plist-get it prop)))))
    (apply #'append (mapcar fun checks))))

(defun elisp-check-emit (level msg &optional file line col)
  "Emit a CI message for the given arguments.
MSG is the message text, while LEVEL corresponds to a urgency
level such as \'warning or \'error.  FILE, LINE and COL may be
given to associate the message with a file."
  (let* ((msg (replace-regexp-in-string "\n" "\\\\n" msg)))
    (message
     (concat
      (format "::%s " level)
      (when file (format "file=%s," file))
      (when line (format "line=%s," line))
      (when col (format "col=%s," col))
      (format "::%s" msg)))))

(defun elisp-check-debug (message &rest objects)
  "Emit a debug MESSAGE formatted with OBJECTS."
  (let ((format (apply #'format message objects)))
    (message "[ELISP-CHECK] %s" format)))

(defun elisp-check-error (message &rest objects)
  "Emit a CI error MESSAGE formatted with OBJECTS, then exit."
  (let ((format (apply #'format message objects)))
    (elisp-check-emit 'error format)
    (error format)))

(defun elisp-check--listify (val)
  "Return VAL if list, \(list VAL) otherwise."
  (if (listp val)
      val
    (list val)))

(defun elisp-check-parse (regexp &optional captures handler)
  "Parse the current buffer for REGEXP.
CAPTURES corresponds to the number of captures, HANDLER is then
called with all captures as its arguments."
  (let ((captures (or captures 1))
        (handler (or handler #'identity))
        (result '()))
    (save-excursion
      (goto-char 0)
      (while (/= (point-at-eol) (point-max))
        (save-excursion
          (save-match-data
            (goto-char (point-at-bol))
            (condition-case err
                (let* ((seq (number-sequence 1 captures))
                       (eol (point-at-eol))
                       (success (re-search-forward regexp eol))
                       (args (mapcar #'match-string-no-properties seq)))
                  (when success
                    (push (apply handler args) result)))
              (error))))
        (forward-line)))
    result))

(defun elisp-check-format-error (err)
  "Format the error ERR in a visually pleasing manner."
  (let* ((symbol (car err))
         (data (cdr err))
         (msg (get symbol 'error-message))
         (data-string (mapconcat #'identity data ", ")))
    (if (eq symbol 'error)
        data-string
      (format "%s: %s" msg data-string))))

(defun elisp-check-package-lint (&rest other)
  "Run a package-lint check on the current and OTHER buffers."
  (let ((package-lint-main-file (buffer-file-name)))
    (elisp-check--package-lint-buffer)
    (dolist (buffer other)
      (with-current-buffer buffer
        (elisp-check--package-lint-buffer)))))

(defun elisp-check--package-lint-buffer ()
  "Run a package-lint check on the current buffer."
  (dolist (lint (package-lint-buffer))
    (let ((level (nth 2 lint))
          (msg (nth 3 lint))
          (file (buffer-file-name))
          (line (nth 0 lint))
          (col (nth 1 lint)))
      (elisp-check-emit level msg file line col))))

(defun elisp-check-byte-compile (&rest other)
  "Run a byte compile check on the current and OTHER buffers."
  (ad-enable-advice
   'byte-compile-log-warning
   'around
   'elisp-check--advice-byte-compile-log)
  (ad-activate 'byte-compile-log-warning)
  (let ((byte-compile-dest-file-function
         (lambda (_file)
           (file-name-directory (make-temp-file "bytecomp-")))))
    (byte-compile-file (buffer-file-name))
    (dolist (buffer other)
      (byte-compile-file (buffer-file-name buffer))))
  (ad-deactivate 'byte-compile-log-warning))

(defun elisp-check--byte-compile-emit (msg &optional pos _fill level)
  "Emit a CI message for MSG, POS and LEVEL.
This should be bound to `byte-compile-log-warning-function' in
order to hook `byte-compile-file' into the CI message mechanism."
  (save-excursion
    (goto-char pos)
    (elisp-check-emit
     (if (eq level :warning) 'warning 'error)
     msg
     byte-compile-current-file
     (line-number-at-pos nil)
     (current-column))))

;; HACK: Advice `byte-compile-log-warning' instead of using the proper
;; variable `byte-compile-log-warning-function' as it does not exist
;; for older versions of Emacs.

(defadvice byte-compile-log-warning
    (around elisp-check--advice-byte-compile-log disable)
  "Emit byte compile errors and warnings as CI messages."
  (elisp-check--byte-compile-emit
   string
   byte-compile-last-position
   fill
   level))

(defun elisp-check-checkdoc (&rest other)
  "Run a checkdoc check on the current and OTHER buffers."
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
       3 (lambda (file line msg)
           (elisp-check-emit 'warning msg file line)))
      (kill-buffer-and-window))))

(defun elisp-check-ert (&rest _other)
  "Run a ERT check on tests defined in the current buffer."
  (ert-delete-all-tests)
  (elisp-check-load-file)
  (let* ((stats (ert-run-tests-batch))
         (unexpected (ert-stats-completed-unexpected stats))
         (total (ert-stats-total stats)))
    (cond
     ((zerop total)
      (elisp-check-emit
       'error "No tests were defined."))
     ((not (zerop unexpected))
      (dolist (test (append (ert--stats-tests stats) nil))
        (let* ((name (ert-test-name test))
               (result (ert-test-most-recent-result test))
               (expected (ert-test-result-expected-p test result)))
          (when (not expected)
            (elisp-check-emit
             'error (format "Test `%s' failed." name)))))))))

(defun elisp-check-load-file (&rest _other)
  "Run a `load-file' check on the current buffer."
  (condition-case err
      (load-file (buffer-file-name))
    (error (elisp-check-emit
            'error
            (elisp-check-format-error err)))))

(provide 'elisp-check)

;;; elisp-check.el ends here
