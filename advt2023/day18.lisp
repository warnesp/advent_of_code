;; Advent of Code Day 18

(load  "utilities.lisp")

(defpackage :aoc-2023-day18
  (:use :cl :aoc-utils)
  (:export 
    #:test1
    #:real1
    #:test2
    #:real2
           ))
(in-package :aoc-2023-day18)

(defstruct instr dir value color)

(defun read-dirs (line)
  "Dir Number (#color)"
  (let ((parts (str:split " " line)))
    (make-instr 
      :dir (read-from-string (car parts)) 
      :value (parse-integer (cadr parts) )
      :color (caddr parts))
    ))

(defun instr-to-bounds (instrs)
  (let (
        (curX 0)
        (curY 0)
        (minX 0)
        (maxX 0)
        (minY 0)
        (maxY 0)
        )
    (loop 
      for instr in instrs 
      for dir = (instr-dir instr)
      for val integer = (instr-value instr)
      when (eq 'R dir)
      do (setf curX (+ curX val))
      and do (setf maxX (max maxX curX))
      when (eq 'L dir)
      do (setf curX (- curX val))
      and do (setf minX (min minX curX))
      when (eq 'D dir)
      do (progn (setf curY (+ curY val))
                (setf maxY (max maxY curY)))
      when (eq 'U dir)
      do (progn (setf curY (- curY val))
                (setf maxY (min maxY curY)))
      finally (return (list minX minY maxX maxY))
      )
    )
  )

;; Part 1
(defun part1 (file)
  (let ((instrs (read-file file #'read-dirs)))
    (instr-to-bounds instrs)
    ))

;; 
(defun test1 () (part1 "inputs/day18-test1"))

(test1)

;; Part 2

