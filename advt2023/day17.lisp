;; Advent of Code Day 17

(load  "utilities.lisp")

(defpackage :aoc-2023-day17
  (:use :cl :aoc-utils)
  (:export 
    #:day17-test1
    #:day17-real1
    #:day17-test2
    #:day17-real2
           ))
(in-package :aoc-2023-day17)

(defun read-map (line)
  (coerce line 'list))

(defun dijk-3-step (heat-map source target)
  (let (
        (dist (make-array (array-dimensions heat-map) :initial-element 999999))
        (prev (make-array (array-dimensions heat-map) :initial-element nil))
        (Q (loop for r from 0 below (car (array-dimensions heat-map))
                 nconcing (loop for c from 0 below (cadr (array-dimensions heat-map))
                                collect (list r c))))
        (u source)
        
        )
    (setf (aref dist (car source) (cdr source)) 0)
    (loop when (equalp u target)
          return dist
          do (progn 
               ;; u <- vertext in Q with min dist[u]
               ;; remove u from Q

               )
          )

    ))

;; Part 1

(defun day17-part1 (file)
  (let ((initial-pos '(0 0))
        (heat-map (list-to-2d-array (read-file file #'read-map))))
    heat-map))

;; 
(defun day17-test1 ()
  (day17-part1 "inputs/day17-test1"))

;; 
(defun day17-real1 ()
  (day17-part1 "inputs/day17"))

;; Part 2


(defun day17-part2 (file)
  (read-file file #'read-map))

;; 
(defun day17-test2 ()
  (day17-part2 "inputs/day17-test2"))

;; 
(defun day17-real2 ()
  (day17-part2 "inputs/day17"))

