#!/usr/bin/env janet
# flymake-janet-compile-all.janet
#
# Collects all compile errors, warnings, and parse errors in one pass
# using run-context callbacks.
#
# Usage: janet flymake-janet-compile-all.janet [-w LEVEL] [-x LEVEL]
#   Reads Janet source from stdin, prints diagnostics to stderr.

(var warn-level nil)
(var error-level nil)
(def args (dyn :args))

(var i 1)
(while (< i (length args))
  (def arg (args i))
  (cond
    (= arg "-w") (do (set warn-level (keyword (args (+ i 1)))) (+= i 2))
    (= arg "-x") (do (set error-level (keyword (args (+ i 1)))) (+= i 2))
    (+= i 1)))

(when warn-level  (setdyn :lint-warn  warn-level))
(when error-level (setdyn :lint-error error-level))

(def src (string (file/read stdin :all)))

# Definition forms that are safe to execute so later forms can reference them
(def- def-forms
  {'defn true 'defn- true 'varfn true
   'defmacro true 'defmacro- true
   'def true 'def- true 'var true 'var- true
   'defglobal true 'varglobal true})

(defn- safe-evaluator [thunk source _env _where]
  (when (and (tuple? source) (def-forms (source 0)))
    (try (thunk) ([_] nil))))

(var had-error false)
(var src-sent false)

(run-context
  {:chunks
   (fn [buf _]
     (when (not src-sent)
       (set src-sent true)
       (buffer/push-string buf src)
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

   :evaluator safe-evaluator
   :source "stdin"})

(os/exit (if had-error 1 0))
