;; Advent of Code 2024 day 3

(in-package :advt2024-d3)

(defvar *part1* "inputs/day03-part1")
(defvar *part1-test* "inputs/day03-part1-test")
(defvar *part2-test* "inputs/day03-part2-test")


(defun p1-process-line (line)
  line
  )

(defun run-p1 (file) 
  (let ((data (read-file file #'p1-process-line)))
    ))

(defun p2-process-line (line)
  )

(defun run-p2 (file) 
  )

(defun run-p1-real ()
  (run-p1 *part1*))

(defun run-p1-test ()
  (run-p1 *part1-test*))

(defun run-p2-real ()
  (run-p2 *part1*))

(defun run-p2-test ()
  (run-p2 *part2-test*))
