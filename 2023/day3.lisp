;; 2023 Advent of Code Day 3

(load  "utilities.lisp")

(defun get-symbol (c)
  (when (not (or (eq c #\.) (digit-char-p c)))
    c))

(defun read-schematic (line)
  (let ((new-num 0))
    (loop for x from 0 below (length line)
          for potential-num = (parse-integer (subseq line x) :junk-allowed t)
          when (not (digit-char-p (char line x)))
            do (setf new-num 0)
          when (and (eq new-num 0) potential-num (digit-char-p (char line x)))
            do (setf new-num potential-num)
          collect (cons (get-symbol (char line x)) new-num) )))
(defun sum-same-line-parts (line)
  (+
   (loop for prev in line
         for cur in (cdr line) 
         when (car cur)
           sum (cdr prev))
   (loop for cur in line 
        for next in (cdr line) 
        when (car cur)
          sum  (cdr next)
        )))
(defun sum-other (left center right)
  (cond ((= left center right)  left)
        ((and (= left center) (zerop right)) left)
        ((and (zerop left) (= center right)) right)
        (t (+ left center right))))

;; TODO add in corner cases where symbol is at start or end of line
(defun sum-other-line-parts (line other)
  (fresh-line)
  (princ "===>")
  (princ (caar (last line)))
  (princ "===>")
  (princ (cdar (last other 2)))
  (fresh-line)
  (+ (loop for prev in other
           for cur in (cdr other) 
           for next in (cddr other) 
           for pos in (cdr line) 
           when (car pos)
             do (progn (princ (cdr prev))  (princ " ") (princ (cdr cur)) (princ " ") (princ (cdr next)) (princ " = ") (princ (sum-other (cdr prev) (cdr cur) (cdr next))) (fresh-line)) 
           when (car pos)
             sum (sum-other (cdr prev) (cdr cur) (cdr next))
           )
     (if (caar line) (sum-other 0 (cdar other) (cdadr other) ) 0)
     (if (caar (last line)) (sum-other  (cdar (last other 2)) (cdar (last other)) 0) 0)
     ))


(defun sum-parts (lines)
  (+ (loop for line in lines
           sum (sum-same-line-parts line))
     (loop for prev in lines
           for cur in (cdr lines)
           sum (sum-other-line-parts cur prev))
     (loop for cur in lines
           for next in (cdr lines)
           sum (sum-other-line-parts cur next))
     )
  )

;; 4361
(defun day3-test1 ()
   (sum-parts (read-file "inputs/day3-test1" #'read-schematic)))
;; 413
(defun day3-test2 ()
  (sum-parts (read-file "inputs/day3-test2" #'read-schematic)))
;; 530751 too low
;; 533767 too low
;; 533775
(defun day3-real1 ()
  (sum-parts (read-file "inputs/day3" #'read-schematic)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;

(defun sum-gears (lines)
  )

;;467835
(defun day3-test3 ()
  (sum-gears (read-file "inputs/day3-test1" #'read-schematic)))
;;6756
(defun day3-test4 ()
  (sum-gears (read-file "inputs/day3-test2" #'read-schematic)))
(defun day3-real2 ()
  (sum-gears (read-file "inputs/day3" #'read-schematic)))



