;;; elisp-check.el --- Run elisp checks for GitHub Actions

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/flycheck-aspell
;; Keywords: flymake, spell, aspell
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
  "Run the given check EXPRESSION with entry FILE.
File globbing is supported."
  ;; Add repository to load-path
  (add-to-list 'load-path default-directory)
  ;; Require all explicit dependencies
  (mapc #'require (elisp-check-get-props expr :require))
  ;; Run checker functions
  (let ((checks (elisp-check-get-props expr :function))
        (buffers (find-file-noselect file nil nil t)))
    (dolist (buffer (elisp-check--explode buffers))
      (with-current-buffer buffer
        (read-only-mode 1)
        (save-excursion
          (mapc #'funcall checks))
        (read-only-mode -1)))))

(defun elisp-check-install (expr)
  "Install requirements for the given check EXPRESSION and FILE."
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (mapc #'package-install (elisp-check-get-props expr :require)))

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

(provide 'elisp-check)

;;; elisp-check.el ends here
