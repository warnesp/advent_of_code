(in-package :advt2024)

;; Define your project functionality here...

(defun greet (&optional (name "Paul Warnes"))
  (format t "Hello ~a from ~a!~&" name "advt2024"))

(defun help ()
  (format t "~&Usage:

  advt2024 [day]~&"))

(defun invalid-day (day)
(format t "Invalid day: ~a~&" day)
    (uiop:quit))

(defun %main (argv)
  "Parse CLI args."
  (when (member "-h" argv :test #'equal)
    ;; To properly parse command line arguments, use a third-party library such as
    ;; clingon, unix-opts, defmain, adopt… when needed.
    (help)
    (uiop:quit))
  (let ((day (parse-integer (first argv) :junk-allowed t)))
    (case day
      (1 (day01))
      (t (invalid-day day)))
    )
  )

(defun main ()
  "Entry point for the executable.
  Reads command line arguments."
  ;; uiop:command-line-arguments returns a list of arguments (sans the script name).
  ;; We defer the work of parsing to %main because we call it also from the Roswell script.
  (%main (uiop:command-line-arguments)))
