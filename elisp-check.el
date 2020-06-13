;;; elisp-check.el --- Run elisp checks for GitHub Actions

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: elisp, lint, test
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

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

;; This elisp package is intended to be used in conjuction with the
;; elisp-check GitHub Actions.  As long as you have installed all
;; required packages, you should also be able to run checks locally
;; using the `elisp-check-run' function.
;;
;; Refer to the repository README.md for documentation.

(require 'gv)

;;; Code:

(defvar elisp-check-alist
  '(("melpa"
     :collection ("byte-compile" "checkdoc" "package-lint"))
    ("byte-compile"
     :function elisp-check-byte-compile
     :require bytecomp)
    ("checkdoc"
     :function elisp-check-checkdoc
     :require checkdoc)
    ("package-lint"
     :function elisp-check-package-lint
     :require package-lint
     :package package-lint))
  "Alist from check names to definitions.")

(defun elisp-check-run (expr file)
  "Run the given check EXPR with entry FILE.
File globbing is supported."
  ;; Add repository to load-path
  (add-to-list 'load-path default-directory)
  ;; Require all explicit dependencies
  (elisp-check-emit 'debug "Requiring test dependencies...")
  (mapc #'require (elisp-check-get-props expr :require))
  (elisp-check-emit 'debug "Requiring test dependencies... done")
  ;; Run checker functions
  (elisp-check-emit 'debug "Running all checks...")
  (let ((checks (elisp-check-get-props expr :function))
        (buffers (find-file-noselect file nil nil t)))
    (dolist (buffer (elisp-check--explode buffers))
      (with-current-buffer buffer
        (read-only-mode 1)
        (save-excursion
          (mapc #'funcall checks))
        (read-only-mode -1))))
  (elisp-check-emit 'debug "Running all checks... done"))

(defun elisp-check-install (expr)
  "Install requirements for the given check EXPR and FILE."
  (elisp-check-emit 'debug "Installing required packages...")
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (package-refresh-contents)
  (mapc #'package-install (elisp-check-get-props expr :package))
  (elisp-check-emit 'debug "Installing required packages... done") )

(defun elisp-check-get-checks (expr)
  (let* ((check (alist-get expr elisp-check-alist nil nil #'string=))
         (deps (plist-get check :collection))
         (dep-values (apply #'append (mapcar #'elisp-check-get-checks deps))))
    (cons check dep-values)))

(defun elisp-check-get-props (expr prop)
  (let* ((checks (elisp-check-get-checks expr))
         (fun (lambda (it)
                (elisp-check--explode
                 (plist-get it prop)))))
    (mapcan fun checks)))

(defun elisp-check--explode (val)
  (if (listp val)
      val
    (list val)))

(defun elisp-check-emit (level message &optional file line col)
  "Emit a GitHub Actions message."
  (message
   (concat
    (format "::%s " level)
    (when file (format "file=%s," file))
    (when line (format "line=%s," line))
    (when col (format "col=%s," col))
    (format "::%s" message))))

(defun elisp-check-package-lint ()
  (dolist (lint (package-lint-buffer))
    (let ((level (nth 2 lint))
          (msg (nth 3 lint))
          (file (buffer-file-name))
          (line (nth 0 lint))
          (col (nth 1 lint)))
      (elisp-check-emit level msg file line col))))

(defun elisp-check-byte-compile ()
  (let ((byte-compile-dest-file-function
         (lambda (file)
           (file-name-directory (make-temp-file "bytecomp-"))))
        (byte-compile-log-warning-function #'elisp-check--byte-compile-emit))
    (byte-compile-file (buffer-file-name))))

(defun elisp-check--byte-compile-emit (msg &optional pos _fill level)
  (save-excursion
    (setf (point) pos)
    (elisp-check-emit
     (if (eq level :warning) 'warning 'error)
     msg
     (buffer-file-name)
     (line-number-at-pos nil t)
     (current-column))))

(defun elisp-check-parse (regexp matches handler)
  (save-excursion
    (setf (point) 0)
    (while (/= (point-at-eol) (point-max))
      (save-excursion
        (save-match-data
          (setf (point) (point-at-bol))
          (condition-case err
              (let ((seq (number-sequence 1 matches)))
                (re-search-forward regexp)
                (apply handler (mapcar #'match-string-no-properties seq)))
            (error nil))))
      (forward-line))))

(defun elisp-check-checkdoc ()
  (let ((checkdoc-autofix-flag 'never)
        (checkdoc-diagnostic-buffer
         (format "*%s doc errors*" (current-buffer))))
    (checkdoc-current-buffer t)
    (with-current-buffer checkdoc-diagnostic-buffer
      (elisp-check-parse
       "\\(.*\\):\\(.*\\): \\(.*\\)"
       3 (lambda (file line msg)
           (elisp-check-emit 'warning msg file line))))))

(provide 'elisp-check)

;;; elisp-check.el ends here
