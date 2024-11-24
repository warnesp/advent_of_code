;; Advent of Code 2023 Day 11

(load  "utilities.lisp")

(defun read-star-image (line)
  (coerce line 'list))

(defun expand-map (star-map)
  (labels 
    ((expand-row (row)
                 (if (= (length row) (count #\. row)) 
                     (list row (copy-list row)) 
                     (list row) ))
     )
    (loop for r in 
          (transpose 
            (loop for row in star-map
                  nconcing (expand-row row)))
          nconcing (expand-row r))
     ))

(defun find-start-locations (star-map)
  (loop for r in star-map
        for i from 0 
        nconcing (loop for c in r
                       for j from 0
                       when (equal c #\#)
                       collect (list i j)
                       )))

(defun manhat-dist-sum (lst)
  (let ((fst (car lst)))
    (apply #'+ (mapcar (lambda (other) (manhat-dist fst other)) (cdr lst)))
    ))

(defun manhat-dist-map (star-map)
  (apply #'+ (maplist #'manhat-dist-sum (find-start-locations star-map)))  
  )

;; Part 1

;; 374
(defun day11-test1 ()
  (let ((star-map (expand-map (read-file "inputs/day11-test1" #'read-star-image))))
    (manhat-dist-map star-map)))

;; 9565386
(defun day11-real1 ()
  (let ((star-map (expand-map (read-file "inputs/day11" #'read-star-image))))
    (manhat-dist-map star-map)))


;; Part 2

(defun find-empty-rows (star-map)
  (loop for r in star-map
        for i from 0
        when (= (length r) (count #\. r))
        collect i))

(defun count-empty-crosses (pos1 pos2 empties)
  (loop for empty in empties
        when (or 
               (and (< pos2 empty)
                  (< empty pos1))
               (and (< pos1 empty)
                  (< empty pos2))) 
        sum 1
        ))

(defun mod-manhat-dist (pt1 pt2 expansion empty-rows empty-cols)
  (let ((crosses (+ (count-empty-crosses (car pt1) (car pt2) empty-rows)
           (count-empty-crosses (cadr pt1) (cadr pt2) empty-cols)
          )))
    (+ (manhat-dist pt1 pt2) (* (1- expansion) crosses))
    )
  )

(defun manhat-dist-sum2 (lst expansion empty-rows empty-cols)
  (let ((pt1 (car lst)))
    (loop for pt2 in (cdr lst)
          sum (mod-manhat-dist pt1 pt2 expansion empty-rows empty-cols))
    ))

(defun manhat-dist-map2 (star-map expansion)
  (let ((empty-rows (find-empty-rows star-map))
        (empty-cols (find-empty-rows (transpose star-map)))
        (star-locs (find-start-locations star-map)))
    
    (apply #'+ 
           (maplist 
             (lambda (lst) (manhat-dist-sum2 lst expansion empty-rows empty-cols))
             star-locs)) 

    ))

;; 1030 = 10 times
;; 8410 = 100 times
(defun day11-test2 (expansion)
  (let ((star-map (read-file "inputs/day11-test1" #'read-star-image)))
    (manhat-dist-map2 star-map expansion)))

;; 857986849428
(defun day11-real2 (expansion)
  (let ((star-map (read-file "inputs/day11" #'read-star-image)))
    (manhat-dist-map2 star-map expansion)))

