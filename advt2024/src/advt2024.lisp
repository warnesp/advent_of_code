(in-package :advt2024)

;; Define your project functionality here...

(defun help ()
  (format t "~&Usage:

  advt2024 day [-t] [-p2]~&"))

(defun invalid-day (day)
(format t "Invalid day: ~a~&" day)
  (help)
    (uiop:quit))

(defun build-fn (day part test)
  "builds the day function to call, can call part 1 or 2 and regular or test"
  (concatenate 'string "(advt2024-d" day ":run-" part (if test "-test" "") ")"))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (let ((day-i (or (parse-integer (first argv) :junk-allowed t) 0) )
        (day (first argv))
        (test (member "-t" argv :test #'equal))
        (part (if (member "-p2" argv :test #'equal) "p2" "p1") )
        )
    (format t "~a~&" (build-fn day part test))
    (if (and (>= day-i 1) (<= day-i 25)) 
        (eval (read-from-string (build-fn day part test)))
        (invalid-day day))
     (uiop:quit)
    
    )
  )

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
