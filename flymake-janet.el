;;; flymake-janet.el --- Flymake backend for Janet -*- lexical-binding: t -*-

;; Copyright (C) 2026 Gal Buki
;; Author: Gal Buki <jkl@torus.ch>
;; URL: https://github.com/torusjkl/flymake-janet
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: (emacs "30.0")

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
Corresponds to `janet -k -w LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake-janet)

(defcustom flymake-janet-error-level nil
  "Lint level at or below which Janet promotes warnings to errors.
Corresponds to `janet -k -x LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag (no lint errors)."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake)

(defconst flymake-janet--script
  (expand-file-name "flymake-janet-check.janet"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the Janet checker script bundled with flymake-janet.")

(defvar-local flymake-janet--proc nil
  "Current Janet checker script process for the buffer.")

(defvar-local flymake-janet--proc-jk nil
  "Current `janet -k' process for the buffer (started after the script).")

(defun flymake-janet--parse-buf (buf source &optional only-warnings)
  "Parse diagnostics from BUF against SOURCE buffer.
If ONLY-WARNINGS is non-nil, skip error-severity diagnostics."
  (let (diags)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(?:error: stdin\\|stdin\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
              nil t)
        (let* ((full-msg (match-string 3))
               (is-warning (string-prefix-p "compile warning" full-msg)))
          (unless (and only-warnings (not is-warning))
            (let* ((severity (if is-warning :warning :error))
                   (msg (replace-regexp-in-string
                         "^compile \\(?:warning ([^)]+)\\|error\\): " "" full-msg))
                   (line    (string-to-number (match-string 1)))
                   (col     (string-to-number (match-string 2)))
                   (adj-col (if (zerop col) 1 col))
                   (pos     (flymake-diag-region source line adj-col)))
              (push (flymake-make-diagnostic
                     source (car pos) (cdr pos) severity msg)
                    diags))))))
    (nreverse diags)))

(defun flymake-janet--error-lines (buf)
  "Return list of line numbers that have compile errors in BUF."
  (let (lines)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward
              "^error: stdin:\\([0-9]+\\):[0-9]+: compile error:" nil t)
        (push (string-to-number (match-string 1)) lines)))
    lines))

(defun flymake-janet--clean-source (source error-lines)
  "Return SOURCE buffer content with ERROR-LINES replaced by blank lines."
  (with-current-buffer source
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (ln 1))
      (string-join
       (mapcar (lambda (line)
                 (prog1
                     (if (member ln error-lines) "" line)
                   (setq ln (1+ ln))))
               (split-string content "\n"))
       "\n"))))

(defun flymake-janet--backend (report-fn &rest _args)
  "Flymake backend for Janet syntax checking.
Runs the bundled script to collect all compile errors, then runs `janet -k'
on the source with those lines blanked to collect warnings."
  (when (process-live-p flymake-janet--proc)
    (kill-process flymake-janet--proc))
  (when (process-live-p flymake-janet--proc-jk)
    (kill-process flymake-janet--proc-jk))
  (let* ((source (current-buffer))
         (lint-args (append
                     (when flymake-janet-warn-level
                       (list "-w" (symbol-name flymake-janet-warn-level)))
                     (when flymake-janet-error-level
                       (list "-x" (symbol-name flymake-janet-error-level)))))
         (buf-script (generate-new-buffer " *flymake-janet*")))
    (setq flymake-janet--proc
          (make-process
           :name "flymake-janet"
           :noquery t
           :connection-type 'pipe
           :buffer buf-script
           :command (append (list "janet" flymake-janet--script) lint-args)
           :stderr buf-script
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (unwind-protect
                   (if (not (with-current-buffer source
                              (eq proc flymake-janet--proc)))
                       (flymake-log :warning "Obsolete flymake-janet process %s" proc)
                     (let* ((error-lines (flymake-janet--error-lines
                                          (process-buffer proc)))
                            (error-diags (flymake-janet--parse-buf
                                          (process-buffer proc) source))
                            (cleaned     (flymake-janet--clean-source
                                          source error-lines))
                            (buf-jk      (generate-new-buffer " *flymake-janet-jk*"))
                            (proc-jk
                             (with-current-buffer source
                               (setq flymake-janet--proc-jk
                                     (make-process
                                      :name "flymake-janet-jk"
                                      :noquery t
                                      :connection-type 'pipe
                                      :buffer buf-jk
                                      :command (append '("janet" "-k") lint-args)
                                      :stderr buf-jk
                                      :sentinel
                                      (lambda (proc2 _event2)
                                        (when (memq (process-status proc2)
                                                    '(exit signal))
                                          (unwind-protect
                                              (if (not (with-current-buffer source
                                                         (eq proc2 flymake-janet--proc-jk)))
                                                  (flymake-log :warning
                                                               "Obsolete flymake-janet process %s"
                                                               proc2)
                                                (funcall report-fn
                                                         (append
                                                          error-diags
                                                          (flymake-janet--parse-buf
                                                           (process-buffer proc2)
                                                           source))))
                                            (kill-buffer (process-buffer proc2))))))))))
                       (process-send-string proc-jk cleaned)
                       (process-send-eof proc-jk)))
                 (kill-buffer (process-buffer proc)))))))
    (process-send-region flymake-janet--proc (point-min) (point-max))
    (process-send-eof flymake-janet--proc)))

;;;###autoload
(defun flymake-janet-setup ()
  "Enable Janet Flymake backend in the current buffer."
  (add-hook 'flymake-diagnostic-functions #'flymake-janet--backend nil t)
  (when flymake-mode
    (flymake-start)))

(provide 'flymake-janet)
;;; flymake-janet.el ends here
