;; 2023 Advent of Code Day 2

(load  "utilities.lisp")

(ql:quickload "cl-ppcre")

(defparameter *max-red* 12)
(defparameter *max-green* 13)
(defparameter *max-blue* 14)



(defun process-games (lines)
  (princ lines)
  )

(defun read-pieces (str)
  (cons 'red (parse-integer str :junk-allowed t)))

(defun read-game (str)
  (let ((pieces (cl-ppcre:split "," str)))
    (mapcar #'read-pieces pieces)))

(defun read-games (line)
  (mapcar #'read-game (cl-ppcre:split ";" line)))

(defun read-data (line)
  (let ((s1 (cl-ppcre:split ":" line)))
    (list  (parse-integer (subseq (car s1) 5)) (read-games (cadr s1)))))

(defun day2-test1 ()
   (process-games (read-file "inputs/day2-test1" #'read-data)))

(defun day2-real1 ()
  (process-games (read-file "inputs/day2" #'read-data)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;




(defun day2-test2 ()
   (process-games (read-file "inputs/day2-test2" #'read-data)))

(defun day2-real2 ()
  (process-games (read-file "inputs/day2" #'read-data)))
