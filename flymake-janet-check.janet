#!/usr/bin/env janet
# flymake-janet-check.janet
#
# Drop-in replacement for `janet -k` that continues past compile errors,
# so all warnings and errors in the file are reported.
#
# Usage: janet flymake-janet-check.janet [-w LEVEL] [-x LEVEL]
#   Reads Janet source from stdin, prints diagnostics to stderr.

(var warn-level :normal)
(var error-level nil)
(def args (dyn :args))

# args[0] is the script name; parse from index 1
(var i 1)
(while (< i (length args))
  (def arg (args i))
  (cond
    (= arg "-w") (do (set warn-level (keyword (args (+ i 1)))) (+= i 2))
    (= arg "-x") (do (set error-level (keyword (args (+ i 1)))) (+= i 2))
    (+= i 1)))

(setdyn :lint-warn warn-level)
(when error-level
  (setdyn :lint-error error-level))

# Read all of stdin
(def src (string (file/read stdin :all)))

# Parse phase
(def p (parser/new))
(parser/consume p src)

(when (= (parser/status p) :error)
  (eprin (string "error: stdin:1:1: parse error: " (parser/error p) "\n"))
  (os/exit 1))

# Compile each top-level form individually, continuing past errors
(def env (make-env))
(var had-error false)

(while (parser/has-more p)
  (def form (parser/produce p))
  (when (not= form nil)
    (def result (compile form env "stdin"))
    (if (function? result)
      # Successfully compiled: run it so later forms can see its definitions
      (try (result) ([_] nil))
      # Compile error: report it and continue
      (do
        (set had-error true)
        (def line (or (get result :line) 1))
        (def col  (or (get result :column) 1))
        (def err  (or (get result :error) "unknown error"))
        (eprin (string/format "error: stdin:%d:%d: compile error: %s\n"
                              line col err))))))

(os/exit (if had-error 1 0))
