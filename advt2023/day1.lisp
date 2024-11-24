;; 2023 Advent of Code Day 1

(load  "utilities.lisp")

;; gets a two digit number from the given list by taking the first digit and last digit
;; and using them as the tens and ones digit respectively
(defun extract-cal (line-numbers)
  (+ (* 10 (car line-numbers)) (car (last line-numbers))))

;; takes in a list of list of numbers, gets the calibration from each sub list and sums them together
(defun sum-cals (lines)
  (apply #'+ (mapcar #'extract-cal lines)))

;; read calibration for part 1
;; takes in a string, converts it to a list of numbers
(defun read-calibration (line)
  (remove nil (mapcar #'digit-char-p (coerce line 'list))))

(defun day1-test1 ()
   (sum-cals (read-file "inputs/day1-test" #'read-calibration)))

(defun day1-real1 ()
  (sum-cals (read-file "inputs/day1" #'read-calibration)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;


;; returns t if the in-list starts with to-find
(defun soft-match (to-find in-list)
  (cond
    ((eq nil to-find) t)
    ((eq nil in-list) nil)
    ((not (equal (car to-find) (car in-list))) nil)
    (t (soft-match (cdr to-find) (cdr in-list)))))

(defparameter *number-map* '(("zero" . 0)
                             ("one" . 1)
                             ("two" . 2)
                             ("three" . 3)
                             ("four" . 4)
                             ("five" . 5)
                             ("six" . 6)
                             ("seven" . 7)
                             ("eight" . 8)
                             ("nine" . 9)
                             ))
;; returns a number if the given list starts with a number character or a word that spells a single digit
(defun convert-number-word (lst)
  (if (digit-char-p (car lst)) (digit-char-p (car lst))
  (loop for nm in *number-map*
        when (soft-match (coerce (car nm) 'list) lst)
          return (cdr nm))))

(defun read-calibration-alt (line)
  (let ((lst (coerce line 'list)))
    (remove nil (maplist #'convert-number-word lst))))



(defun day1-test2 ()
   (sum-cals (read-file "inputs/day1-test2" #'read-calibration-alt)))

(defun day1-real2 ()
  (sum-cals (read-file "inputs/day1" #'read-calibration-alt)))
