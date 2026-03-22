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

(defcustom flymake-janet-warn-level 'normal
  "Lint level at or below which Janet emits warnings.
Corresponds to `janet -k -w LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake)

(defcustom flymake-janet-error-level nil
  "Lint level at or below which Janet promotes warnings to errors.
Corresponds to `janet -k -x LEVEL'. Levels in increasing strictness:
`relaxed', `normal', `strict'. Set to nil to omit the flag (no lint errors)."
  :type '(choice (const relaxed) (const normal) (const strict) (const nil))
  :group 'flymake)

(defvar-local flymake-janet--proc nil
  "Current flymake-janet process for the buffer.")

(defun flymake-janet--backend (report-fn &rest _args)
  "Flymake backend for Janet syntax checking with `janet -k`."
  ;; Kill any stale process before starting a new one
  (when (process-live-p flymake-janet--proc)
    (kill-process flymake-janet--proc))
  (let* ((source (current-buffer))
         (buf (generate-new-buffer " *flymake-janet*"))
         (sentinel
          (lambda (proc _event)
            (when (memq (process-status proc) '(exit signal))
              (unwind-protect
                  (with-current-buffer (process-buffer proc)
                    (goto-char (point-min))
                    (let (diags)
                      (while (re-search-forward
                              "^\\(?:error: stdin\\|stdin\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                              nil t)
                        (let* ((full-msg (match-string 3))
                               (severity (if (string-prefix-p "compile warning" full-msg)
                                             :warning
                                           :error))
                               (msg (if (eq severity :warning)
                                        (replace-regexp-in-string
                                         "^compile warning ([^)]+): " "" full-msg)
                                      full-msg))
                               (line (string-to-number (match-string 1)))
                               (col  (string-to-number (match-string 2)))
                               ;; Adjust col if 0 → use 1 to avoid invalid pos
                               (adj-col (if (zerop col) 1 col))
                               (pos (flymake-diag-region source line adj-col))
                               (beg (car pos))
                               (end (cdr pos)))
                          (push (flymake-make-diagnostic
                                 source beg end severity
                                 (format "Janet: %s" msg))
                                diags)))
                      (funcall report-fn (nreverse diags))))
                (kill-buffer (process-buffer proc)))))))
    (setq flymake-janet--proc
          (make-process
           :name "flymake-janet"
           :noquery t
           :connection-type 'pipe
           :buffer buf
           :command (list "sh" "-c"
                          (concat (mapconcat #'shell-quote-argument
                                             (append '("janet" "-k")
                                                     (when flymake-janet-warn-level
                                                       (list "-w" (symbol-name flymake-janet-warn-level)))
                                                     (when flymake-janet-error-level
                                                       (list "-x" (symbol-name flymake-janet-error-level))))
                                             " ")
                                  " 2>&1"))
           :sentinel sentinel))
    ;; Pipe buffer contents to janet -k stdin
    (process-send-region flymake-janet--proc (point-min) (point-max))
    (process-send-eof flymake-janet--proc)
    flymake-janet--proc))

;;;###autoload
(defun flymake-janet-setup ()
  "Enable Janet Flymake backend in the current buffer."
  (add-hook 'flymake-diagnostic-functions #'flymake-janet--backend nil t))

(provide 'flymake-janet)
;;; flymake-janet.el ends here
