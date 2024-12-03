;; Advent of Code 2024 day 2

(in-package :advt2024-d2)

(defvar *part1* "inputs/day02-part1")
(defvar *part1-test* "inputs/day02-part1-test")

(defun p1-process-line (line)
  (mapcar #'parse-integer (str:words line)))

(defun line-direction-p (line)
  "make sure the line always goes in the same direction"
  (loop for x in line
        for y in (cdr line)
        count (> x y) into dec
        count (< x y) into inc
        when (and (> dec 0 ) (> inc 0)) return nil
        when (= x y) return nil
        finally (return t)))

(defun line-in-range-p (line)
  "makes sure the delta is within 3"
  (loop for x in line
        for y in (cdr line)
        for delta = (abs (- x y))
        when (or (> delta 3) )
          return nil 
        finally (return t)))

(defun test-line-p (line)
  (and (line-in-range-p line) (line-direction-p line)))

(defun run-p1 (file) 
  (let ((data (read-file file #'p1-process-line)))
    (apply #'+ (mapcar (lambda (line) (if (test-line-p line) 1 0)) data))))

(defun test-line-p2 (line)
  (or (test-line-p (cdr line))
      (test-line-p (cdr (reverse line)))
  (loop for back on line
        collect (car back) into front
        when (test-line-p (concatenate 'list front (cddr back)))
          return t
        finally (return nil)
  )))

(defun run-p2 (file) 
  (let ((data (read-file file #'p1-process-line)))
    (loop for line in data
          count (test-line-p2 line))))

(defun run-p1-real ()
  "624"
  (run-p1 *part1*))

(defun run-p1-test ()
  (= 2 (run-p1 *part1-test*)))

(defun run-p2-real ()
  "658"
  (run-p2 *part1*))

(defun run-p2-test ()
   (= 4 (run-p2 *part1-test*)))
