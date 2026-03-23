;;; flymake-janet.el --- Flymake backend for Janet -*- lexical-binding: t -*-

;; Copyright (C) 2026 Gal Buki
;; Author: Gal Buki <jkl@torus.ch>
;; URL: https://github.com/torusjkl/flymake-janet
;; Version: 0.2.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "26.1"))

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Make sure the janet binary is on your path.

(require 'flymake)

(defgroup flymake-janet nil
  "Janet Flymake backend."
  :prefix "flymake-janet-"
  :group 'flymake)

(defcustom flymake-janet-warn-level 'normal
  "Lint level at or below which Janet emits warnings.
Corresponds to `-w LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake-janet)

(defcustom flymake-janet-error-level nil
  "Lint level at or below which Janet promotes warnings to errors.
Corresponds to `-x LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake-janet)


(defcustom flymake-janet-message-strip-regexp
  "^\\(?:compile\\|parse\\) \\(?:warning\\(?: ([^)]+)\\)?\\|error\\): "
  "Regexp applied to the lint message, or nil to show messages as-is."
  :type '(choice (const nil) regexp)
  :group 'flymake-janet)

(defconst flymake-janet--script
  (expand-file-name "flymake-janet-compile-all.janet"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Janet checker script bundled with flymake-janet.")

(defvar-local flymake-janet--proc nil
  "Current Janet checker process for the buffer.")

(defun flymake-janet--parse-buf (buf source)
  "Parse diagnostics from BUF against SOURCE buffer."
  (let (diags)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(?:error: \\)?stdin:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
              nil t)
        (let* ((full-msg (match-string 3))
               (is-warning (string-prefix-p "compile warning" full-msg))
               (severity (if is-warning :warning :error))
               (line    (string-to-number (match-string 1)))
               (col     (string-to-number (match-string 2)))
               (adj-col (if (zerop col) 1 col))
               (pos     (flymake-diag-region source line adj-col)))
          (push (flymake-make-diagnostic
                 source (car pos) (cdr pos) severity
                 (if flymake-janet-message-strip-regexp
                     (replace-regexp-in-string
                      flymake-janet-message-strip-regexp "" full-msg)
                   full-msg))
                diags))))
    (nreverse diags)))

(defun flymake-janet--backend (report-fn &rest _args)
  "Flymake backend for Janet syntax checking.
Runs the bundled script which collects all compile errors and warnings
in a single pass using `run-context' callbacks."
  (when (process-live-p flymake-janet--proc)
    (kill-process flymake-janet--proc))
  (let* ((source (current-buffer))
         (lint-args (append
                     (when flymake-janet-warn-level
                       (list "-w" (symbol-name flymake-janet-warn-level)))
                     (when flymake-janet-error-level
                       (list "-x" (symbol-name flymake-janet-error-level)))))
         (buf (generate-new-buffer " *flymake-janet*")))
    (setq flymake-janet--proc
          (make-process
           :name "flymake-janet"
           :noquery t
           :connection-type 'pipe
           :buffer buf
           :command (append (list "janet" flymake-janet--script) lint-args)
           :stderr buf
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (if (not (with-current-buffer source
                              (eq proc flymake-janet--proc)))
                       (flymake-log :warning "Obsolete flymake-janet process %s" proc)
                     (funcall report-fn
                              (flymake-janet--parse-buf (process-buffer proc) source)))
                 (kill-buffer (process-buffer proc)))))))
    (process-send-region flymake-janet--proc (point-min) (point-max))
    (process-send-eof flymake-janet--proc)))

;;;###autoload
(defun flymake-janet-setup ()
  "Enable Janet Flymake backend in the current buffer."
  (if (executable-find "janet")
      (add-hook 'flymake-diagnostic-functions #'flymake-janet--backend nil t)
    (message "[flymake-janet] Not enabled: `janet' not found in $PATH")))

(provide 'flymake-janet)
;;; flymake-janet.el ends here
