;; 2023 Advent of Code Day 4

(load  "utilities.lisp")

(defun read-calibration (line)
  (remove nil (mapcar #'digit-char-p (coerce line 'list))))

(defun day4-test1 ()
   (sum-cals (read-file "inputs/day2-test" #'read-calibration)))

(defun day4-real1 ()
  (sum-cals (read-file "inputs/day2" #'read-calibration)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;




(defun day4-test2 ()
   (sum-cals (read-file "inputs/day2-test2" #'read-calibration-alt)))

(defun day4-real2 ()
  (sum-cals (read-file "inputs/day2" #'read-calibration-alt)))
