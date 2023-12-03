;; 2023 Advent of Code Day 3

(load  "utilities.lisp")

(defun read-calibration (line)
  (remove nil (mapcar #'digit-char-p (coerce line 'list))))

(defun day3-test1 ()
   (sum-cals (read-file "inputs/day2-test" #'read-calibration)))

(defun day3-real1 ()
  (sum-cals (read-file "inputs/day2" #'read-calibration)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;




(defun day3-test2 ()
   (sum-cals (read-file "inputs/day2-test2" #'read-calibration-alt)))

(defun day3-real2 ()
  (sum-cals (read-file "inputs/day2" #'read-calibration-alt)))
