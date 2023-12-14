;; Advent of Code Day 13

(load  "utilities.lisp")

(defun char-to-num (c)
  (case c
      (#\# 1)
    (#\. 0)))

(defun read-map (line)
  (if line
     (mapcar #'char-to-num (coerce line 'list)) 
      nil))

(defun build-map (lines)
  (let ((maps (loop
    with sublist
    for l in (append lines '(nil))
    if l 
    do (push l sublist)
    else 
    collect (nreverse sublist) and do (setf sublist nil))))
    (loop for m in maps
          collect (make-array (list (length m) (length (car m)))
                              :initial-contents m))
    ))

(defun map-to-verticals (m)
  (destructuring-bind (r c) (array-dimensions m)
    (make-array c :initial-contents
                (loop for x from 0 below c
                      collect (loop for y from 0 below r
                                    sum (* (expt 2 y) (aref m y x))))
                )))

(defun map-to-horizontals (m)
  (destructuring-bind (r c) (array-dimensions m)
    (make-array r :initial-contents 
                (loop for y from 0 below r
                      collect (loop for x from 0 below c 
                                    sum (* (expt 2 x) (aref m y x))))
                )))

(defun find-reflection-p (line p dist)
  (loop named searcher 
    for left from (1- p) downto 0
    for right from p
    for cnt from 1 upto dist
    when (not (= (aref line left) (aref line right)))
    do (return-from searcher nil)
    finally (return-from searcher t)
    ))

(defun find-reflection (line)
  (loop with len = (array-total-size line)
        for p from 1 below len
        for dist = (min p (- len p))
        when (find-reflection-p line p dist)
        return p
        finally (return 0)))

(defun col-reflections (maps conv-f)
  (loop for m in maps
        collect (find-reflection (funcall conv-f m))))

(defun sum-vertical-reflections (maps)
  (loop for m in maps
        sum (find-reflection (map-to-verticals m)))
  )

(defun sum-horizontal-reflections (maps)
  (loop for m in maps
        sum (find-reflection (map-to-horizontals m))))

(defun print-reflections (maps)
  (fresh-line)
  (princ (map-to-horizontals (car maps)))
  
  (fresh-line)
  (princ (col-reflections maps #'map-to-verticals))
  (fresh-line)
  (princ (col-reflections maps #'map-to-horizontals)))

(defun sum-reflections (maps)
  (+ (sum-vertical-reflections maps)
     (* 100 (sum-horizontal-reflections maps)))
  )

;; Part 1

;; total: 405
(defun day13-test1 ()
  (sum-reflections (build-map (read-file "inputs/day13-test1" #'read-map))))

(defun day13-test2 ()
  (sum-reflections (build-map (read-file "inputs/day13-test2" #'read-map))))

;; total: 20941 - too low
;; 34821
(defun day13-real1 ()
  (sum-reflections (build-map (read-file "inputs/day13" #'read-map))))

;; Part 2

;; change one position, find new reflection
;; xor numbers, count bits (logcount (logxor )
;; if total bits <= 1, good to go
;; also, skip old mirror

;; 400

