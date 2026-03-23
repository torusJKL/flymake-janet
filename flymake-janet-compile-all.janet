#!/usr/bin/env janet
# flymake-janet-compile-all.janet — part of flymake-janet
# Version: 0.2.0
# Copyright (C) 2026 Gal Buki
# SPDX-License-Identifier: GPL-3.0-or-later
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Collects all compile errors, warnings, and parse errors in one pass
# using run-context callbacks.
#
# Usage: janet flymake-janet-compile-all.janet [-w LEVEL] [-x LEVEL]
#   Reads Janet source from stdin, prints diagnostics to stderr.

(var warn-level nil)
(var error-level nil)
(def cli-args (dyn :args))

(var i 1)
(while (< i (length cli-args))
  (def arg (cli-args i))
  (cond
    (= arg "-w") (do (set warn-level (keyword (cli-args (+ i 1)))) (+= i 2))
    (= arg "-x") (do (set error-level (keyword (cli-args (+ i 1)))) (+= i 2))
    (+= i 1)))

(when warn-level  (setdyn :lint-warn  warn-level))
(when error-level (setdyn :lint-error error-level))

(def stdin-src (string (file/read stdin :all)))

# Definition forms that are safe to execute so later forms can reference them
(def- flymake-def-forms
  {'defn true 'defn- true 'varfn true
   'defmacro true 'defmacro- true
   'def true 'def- true 'var true 'var- true
   'defglobal true 'varglobal true})

(defn- flymake-safe-evaluator [thunk source _env _where]
  (when (and (tuple? source) (flymake-def-forms (source 0)))
    (try (thunk) ([_] nil))))

(var had-error false)
(var src-sent false)

(run-context
  {:chunks
   (fn [buf _]
     (when (not src-sent)
       (set src-sent true)
       (buffer/push-string buf stdin-src)
       (buffer/push-string buf "\n")))

   :on-compile-error
   (fn [msg _ _ line col]
     (set had-error true)
     (eprin (string/format "error: stdin:%d:%d: compile error: %s\n"
                           (or line 1) (or col 1) msg)))

   :on-compile-warning
   (fn [msg _ _ line col]
     (eprin (string/format "stdin:%d:%d: compile warning: %s\n"
                           (or line 1) (or col 1) msg)))

   :on-parse-error
   (fn [p _]
     (set had-error true)
     (def [line col] (parser/where p))
     (eprin (string/format "error: stdin:%d:%d: parse error: %s\n"
                           (or line 1) (or col 1) (parser/error p))))

   :evaluator flymake-safe-evaluator
   :source "stdin"})

(os/exit (if had-error 1 0))
