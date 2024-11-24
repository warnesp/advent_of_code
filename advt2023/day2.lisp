;; 2023 Advent of Code Day 2

(load  "utilities.lisp")


(defparameter *max-red* 12)
(defparameter *max-green* 13)
(defparameter *max-blue* 14)

(defun red-in-bounds (round)
  (let ((v (assoc 'red round)))
    (or (eq v nil) (>= *max-red* (cdr v)))))

(defun green-in-bounds (round)
  (let ((v (assoc 'green round)))
    (or (eq v nil) (>= *max-green* (cdr v)))))

(defun blue-in-bounds (round)
  (let ((v (assoc 'blue round)))
    (or (eq v nil) (>= *max-blue* (cdr v)))))

(defun all-in-bounds (round)
  (and (red-in-bounds round) (green-in-bounds round) (blue-in-bounds round)))

(defun game-in-bounds (game)
  (notany #'null (mapcar #'all-in-bounds (cadr game))))

(defun process-games (lines)
  (loop for game in lines
        when (game-in-bounds game)
          sum (car game)))

(defun read-color (str)
  (read-from-string (caddr (cl-ppcre:split " " str))))

(defun read-pieces (str)
  (cons (read-color str) (parse-integer str :junk-allowed t)))

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

(defun get-value (c round)
  (let ((value (assoc c round)))
    (if value (cdr value) 0)))

(defun find-max (color game)
  (loop for match in game
          maximizing (get-value color match)))


(defun cube-power (game)
  (* (find-max 'red game) (find-max 'green game) (find-max 'blue game) ))

(defun process-games-alt (lines)
  (loop for game in lines
        sum (cube-power (cadr game))))


(defun day2-test2 ()
   (process-games-alt (read-file "inputs/day2-test1" #'read-data)))

(defun day2-real2 ()
  (process-games-alt (read-file "inputs/day2" #'read-data)))
