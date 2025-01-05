;; Advent of Code 2024 day 8

(in-package :advt2024-d8)

(defvar *part1* "inputs/day08-part1")
(defvar *part1-test* "inputs/day08-part1-test")
(defvar *part2-test* "inputs/day08-part2-test")

(defun p1-process-line (line)
   (to-symbols line 'advt2024-d8))
  
(defun count-results (results)
  (loop for i from 0 below (array-total-size results)
        count (row-major-aref results i)))

(defun place-annode (pos results)
  (let ((x (first pos)) (y (second pos)))
    (when (in-map results x y) 
      (setf (aref results y x) t))))

(defun create-annodes-p1 (x1 y1 x2 y2)
  (let ((delta-x (- x2 x1)) (delta-y (- y2 y1)))
    (list (list (- x1 delta-x) (- y1 delta-y)) (list (+ x2 delta-x) (+ y2 delta-y)))))

(defun place-annodes (positions results create-annodes)
  (when positions
     (loop with a = (car positions)
           with x1 = (first a)
           with y1 = (second a)
           for b in (cdr positions)
           for ans = (funcall create-annodes x1 y1 (first b) (second b))
           do (dolist (a ans) (place-annode a results)))
     (place-annodes (cdr positions) results create-annodes)))

(defun place-all-annodes (xmits map &optional (create-annodes #'create-annodes-p1))
  (let ((results (make-array (array-dimensions map) :element-type 'boolean :initial-element nil)))
    (loop for k being the hash-key of xmits
          do (place-annodes (gethash k xmits) results create-annodes))
    results))

(defun find-transmitters (map)
  "look throught the map and record where the transmitters are in a hash map"
  (let ((h (make-hash-table)))
    (destructuring-bind (rows cols) (array-dimensions map)
      (loop for j from 0 below rows
            do (loop for i from 0 below cols
                     for v = (aref map j i)
                     unless (eql v '|.|)
                       do (push (list i j) (gethash v h))
                     )))
    h))

(defun run-p1 (file) 
  (let* ((map (list-to-2d-array (read-file file #'p1-process-line))))
    (count-results (place-all-annodes (find-transmitters map) map))
    ))

(defun create-annodes-2 (x1 y1 x2 y2 map)
  (destructuring-bind (rows cols) (array-dimensions map)
    (let* ((m (/ (- y2 y1) (- x2 x1) ))
           (b (- y2 (* m x2))))
      (loop for x from 0 below cols
            for y = (+ b (* x m))
            for r = (nth-value 1 (floor y))
            when (and (= r 0) (>= y 0) (< y rows))
              collect (list x y)))))

(defun run-p2 (file) 
  (let* ((map (list-to-2d-array (read-file file #'p1-process-line))))
    (count-results (place-all-annodes (find-transmitters map) map
                                      (lambda (x1 y1 x2 y2)
                                        (create-annodes-2 x1 y1 x2 y2 map))))))

(defun run-p1-real ()
  "289"
  (run-p1 *part1*))

(defun run-p1-test ()
  "14"
  (run-p1 *part1-test*))

(defun run-p2-real ()
  "1030"
  (run-p2 *part1*))

(defun run-p2-test ()
  "34"
  (run-p2 *part1-test*))

(defun run-p2-test2 ()
  "9"
  (run-p2 *part2-test*))
