;;; elisp-check.el --- Run elisp checks for CI  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2019 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 26 May 2019
;; Homepage: https://github.com/leotaku/elisp-check
;; Keywords: elisp, lint, lisp, test, tools
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))

;;; Code:

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-refresh-contents)
(package-install 'package-lint)

(provide 'elisp-check)

;;; elisp-check.el ends here
