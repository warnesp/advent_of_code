;; Advent of Code Day 16

(load  "utilities.lisp")

(defpackage :aoc-2023-day16
  (:use :cl :aoc-utils)
  (:export 
    #:day16-test1
    #:day16-real1
    #:day16-test2
    #:day16-real2
           ))
(in-package :aoc-2023-day16)

;; dir -> 'N 'S 'W 'E
(defstruct beam dir row col)

(defun arr-beam (arr beam)
  (aref arr (beam-row beam) (beam-col beam)))



(defun read-map (line)
  (coerce line 'list))

(defun beam-north (beam instr energized)
  (decf (beam-row beam))
  (unless (find 'N (aref energized (beam-row beam) (beam-col beam)))
    (push 'N (aref energized (beam-row beam) (beam-col beam)))
    (case (aref instr (beam-row beam) (beam-col beam))
      (#\/ (setf (beam-dir beam) 'E) (list beam))
      (#\\ (setf (beam-dir beam) 'W) (list beam))
      (#\-  
       (let ((beam2 (copy-structure beam)))
         (push 'S (aref energized (beam-row beam) (beam-col beam)))
         (setf (beam-dir beam) 'E)
         (setf (beam-dir beam2) 'W)
         (list beam beam2)
         ))
      (otherwise (list beam)))))

(defun beam-south (beam instr energized)
  (incf (beam-row beam))
  (unless (find 'S (aref energized (beam-row beam) (beam-col beam)))
    (push 'S (aref energized (beam-row beam) (beam-col beam)))
    (case (aref instr (beam-row beam) (beam-col beam))
      (#\/ (setf (beam-dir beam) 'W) (list beam))
      (#\\ (setf (beam-dir beam) 'E) (list beam))
      (#\-  
       (let ((beam2 (copy-structure beam)))
         (push 'N (aref energized (beam-row beam) (beam-col beam)))
         (setf (beam-dir beam) 'E)
         (setf (beam-dir beam2) 'W)
         (list beam beam2)
         ))
      (otherwise (list beam)))))

(defun beam-west (beam instr energized)
  (decf (beam-col beam))
  (unless (find 'W (aref energized (beam-row beam) (beam-col beam)))
    (push 'W (aref energized (beam-row beam) (beam-col beam)))
    (case (aref instr (beam-row beam) (beam-col beam))
      (#\/ (setf (beam-dir beam) 'S) (list beam))
      (#\\ (setf (beam-dir beam) 'N) (list beam))
      (#\|  
       (let ((beam2 (copy-structure beam)))
         (push 'E (aref energized (beam-row beam) (beam-col beam)))
         (setf (beam-dir beam) 'N)
         (setf (beam-dir beam2) 'S)
         (list beam beam2)
         ))
      (otherwise (list beam)))))

(defun beam-east (beam instr energized)
  (incf (beam-col beam))
  (unless (find 'E (aref energized (beam-row beam) (beam-col beam)))
    (push 'E (aref energized (beam-row beam) (beam-col beam)))
    (case (aref instr (beam-row beam) (beam-col beam))
      (#\/ (setf (beam-dir beam) 'N) (list beam))
      (#\\ (setf (beam-dir beam) 'S) (list beam))
      (#\|  
       (let ((beam2 (copy-structure beam)))
         (push 'W (aref energized (beam-row beam) (beam-col beam)))
         (setf (beam-dir beam) 'N)
         (setf (beam-dir beam2) 'S)
         (list beam beam2)
         ))
      (otherwise (list beam)))))

(defun update-beam (beam instr energized) 
  (case (beam-dir beam)
    (N (when (> (beam-row beam) 0) 
         (beam-north beam instr energized)))
    (S (when (< (beam-row beam) (1- (car (array-dimensions instr))))
         (beam-south beam instr energized)))
    (W (when (> (beam-col beam) 0)
         (beam-west beam instr energized)))
    (E (when (< (beam-col beam) (1- (cadr (array-dimensions instr))))
         (beam-east beam instr energized)))
    ))

(defun update-beams (instr energized beams)
  (loop for beam in beams
        nconcing (update-beam beam instr energized)))

(defun create-energized (instr)
  (let ((engr (make-array (array-dimensions instr) :initial-element nil)))
    ;;(setf (aref engr 0 0) t)
    engr))

(defun copy-array (source target)
  (loop for i from 0 below (array-total-size source)
        do (setf (row-major-aref target i) (row-major-aref source i))))

(defun count-energized (energized)
  (loop for i from 0 below (array-total-size energized)
        when (row-major-aref energized i)
        sum 1
        ))
(defun shoot-beam (beam instr)
  (let ((engr (create-energized instr))
        (engr-prev (create-energized instr))
        (beams (list beam)))
    (count-energized 
      (loop
        do (setf beams (update-beams instr engr beams)) 
        when (equalp engr engr-prev)
        do (return engr)
        do (copy-array engr engr-prev)
        ))))

;; Part 1

(defun day16-part1 (file)
  (let ((instr (list-to-2d-array (read-file file #'read-map))))
    (shoot-beam (make-beam :dir 'E :row  0 :col -1) instr)))

;; 46
(defun day16-test1 ()
  (day16-part1 "inputs/day16-test1"))


;; 7798
(defun day16-real1()
    (day16-part1 "inputs/day16"))


;; Part 2

(defun scan-horizontal (instr col-idx dir)
  (let ((rows (car (array-dimensions instr))))
    (loop for r from 0 below rows
          maximizing (shoot-beam (make-beam :dir dir :row r :col col-idx) instr))))

(defun scan-vertical (instr row-idx dir)
  (let ((cols (cadr (array-dimensions instr))))
    (loop for c from 0 below cols
          maximizing (shoot-beam (make-beam :dir dir :row row-idx :col c) instr))))

(defun day16-part2 (file)
  (let ((instr (list-to-2d-array (read-file file #'read-map))))
    (destructuring-bind (rows cols) (array-dimensions instr)
      (let ((east (sb-thread:make-thread #'scan-horizontal :arguments (list instr -1 'E)))
            (west (sb-thread:make-thread #'scan-horizontal :arguments (list instr cols 'W)))
            (south (sb-thread:make-thread #'scan-vertical :arguments (list instr -1 'S)))
            (north (sb-thread:make-thread #'scan-vertical :arguments (list instr rows 'N))))
        (max 
          (sb-thread:join-thread east)
          (sb-thread:join-thread west)
          (sb-thread:join-thread south)
          (sb-thread:join-thread north))
        ))))

;; 51
(defun day16-test2 ()
  (day16-part2 "inputs/day16-test1"))

;; 8026
(defun day16-real2()
    (day16-part2 "inputs/day16"))



