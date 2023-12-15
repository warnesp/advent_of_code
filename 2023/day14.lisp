;; Advent of Code Day 14

(load  "utilities.lisp")

(defun convert-rock (r)
  (case r
    (#\# 'F)
    (#\. 'E)
    (#\O 'O)))

(defun read-platform (line)
  (mapcar #'convert-rock (coerce line 'list)))

(defun lines-to-platform (lines)
  (let ((rows (length lines)) (cols (length (car lines))))
    (make-array (list rows cols) :initial-contents lines)))

(defun shift-up (platform r c)
  (loop for r-cur from r downto 0
        for r-next from (1- r) downto 0
        if (eq (aref platform r-next c) 'E)
        do (progn 
             (setf (aref platform r-cur c) 'E)
             (setf (aref platform r-next c) 'O))
        else do (return)))

(defun shift-down (platform r c)
  (let ((rows (car (array-dimensions platform))))
    (loop for r-cur from r 
          for r-next from (1+ r) below rows
          if (eq (aref platform r-next c) 'E)
          do (progn 
               (setf (aref platform r-cur c) 'E)
               (setf (aref platform r-next c) 'O))
          else do (return))))


(defun shift-left (platform r c)
  (loop for c-cur from c downto 0
        for c-next from (1- c) downto 0
        if (eq (aref platform r c-next) 'E)
        do (progn 
             (setf (aref platform r c-cur) 'E)
             (setf (aref platform r c-next) 'O))
        else do (return)))

(defun shift-right (platform r c)
  (let ((cols (cadr (array-dimensions platform))))
    (loop for c-cur from c 
          for c-next from (1+ c) below cols 
          if (eq (aref platform r c-next) 'E)
          do (progn 
               (setf (aref platform r c-cur) 'E)
               (setf (aref platform r c-next) 'O))
          else do (return))))


(defun tilt-north (platform)
  (destructuring-bind (rows columns) (array-dimensions platform)
    (loop for r from 1 below rows
          do (loop for c from 0 below columns
                    when (eq (aref platform r c) 'O)
                    do (shift-up platform r c))))
  platform)


(defun tilt-south (platform)
  (destructuring-bind (rows columns) (array-dimensions platform)
    (loop for r from (- rows 2) downto 0
          do (loop for c from (1- columns) downto 0
                    when (eq (aref platform r c) 'O)
                    do (shift-down platform r c))))
  platform)


(defun tilt-west (platform)
  (destructuring-bind (rows columns) (array-dimensions platform)
    (loop for r from 0 below rows
          do (loop for c from 1 below columns
                    when (eq (aref platform r c) 'O)
                    do (shift-left platform r c))))
  platform)


(defun tilt-east (platform)
  (destructuring-bind (rows columns) (array-dimensions platform)
    (loop for r from 0 below rows
          do (loop for c from (- columns 2) downto 0
                    when (eq (aref platform r c) 'O)
                    do (shift-right platform r c))))
  platform)

(defun spin-cycle (platform)
  (tilt-east (tilt-south (tilt-west (tilt-north platform)))))


(defun repeat-spin-cycle (platform times)
  (let ((cached (make-hash-table :test 'equal)))
    (dotimes (n times)
      (let* ((s (prin1-to-string platform)))
        (cond
          ((gethash s cached) 
           (return n))  
          (t
           (setf (gethash s cached) n)
           (spin-cycle platform)))
        ))))


(defun sum-weights (platform)
  (destructuring-bind (rows columns) (array-dimensions platform)
    (loop for r from 0 below rows
          for weight from rows downto 0
          sum (loop for c from 0 below columns
                    when (eq (aref platform r c) 'O)
                    sum weight))))

;; Part 1

;; 136
(defun day14-test1 ()
  (sum-weights (tilt-north (lines-to-platform (read-file "inputs/day14-test1" #'read-platform)))))

;; 109654
(defun day14-real1 ()
  (sum-weights (tilt-north (lines-to-platform (read-file "inputs/day14" #'read-platform)))))

;; Part 2
(defvar *itrs* 1000000000)

(defun day14-part2 (file)
  (let ((platform (lines-to-platform (read-file file #'read-platform))))
    (let* ((initial (repeat-spin-cycle platform *itrs*))
           (loop-size (repeat-spin-cycle platform *itrs*))
           (tail (- initial loop-size)))

      (repeat-spin-cycle platform (mod (- *itrs* tail) loop-size))

      )
    (sum-weights platform)
    ))


;; 64
(defun day14-test2 ()
  (day14-part2 "inputs/day14-test1"))

;; 94977 to high
;; 94924 to high
;; 94876
(defun day14-real2 ()
  (day14-part2 "inputs/day14"))

