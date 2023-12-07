;; 2023 Advent of Code Day 6


(defparameter *day6-test1-input* '((7 9) (15 40) (30 200)))
(defparameter *day6-real1-input* '((44 202) (82 1076) (69 1138) (81 1458)))
(defparameter *day6-test2-input* '(71530 940200))
(defparameter *day6-real2-input* '(44826981 202107611381458))

(defun calc-race-margin (race)
  (let ((race-time (car race)) (best (cadr race)))
    (loop for hold-time from 1 to race-time
          when (> (* hold-time (- race-time hold-time)) best)
            sum 1
          )
    )
  )

;; 288
(defun day6-test1 ()
  (apply #'* (mapcar #'calc-race-margin *day6-test1-input*))
  )
;; 588588
(defun day6-real1 ()
  (apply #'* (mapcar #'calc-race-margin *day6-real1-input*))
  )

;; 71503
(defun day6-test2 ()
  (calc-race-margin *day6-test2-input*))

;; 34655848
(defun day6-real2 ()
  (calc-race-margin *day6-real2-input*))
