;; Advent of Code 2024 day 6

(in-package :advt2024-d6)

(defvar *part1* "inputs/day06-part1")
(defvar *part1-test* "inputs/day06-part1-test")

(defstruct move x y direction)

(defstruct guard direction x y (moves (make-hash-table :test 'equalp)))

(defun convert-direction (g)
  (case g
    (^ 'UP)
    (> 'RIGHT)
    (< 'LEFT)
    (v 'DOWN)))


(defun find-guard (map)
  (destructuring-bind (rows cols) (array-dimensions map)
    (loop for j from 0 below rows
          do (loop for i from 0 below cols
                   for v = (aref  map j i)
                   when (not (or (eql '|.| v) (eql '|#| v)))
                     do (return-from find-guard (make-guard :direction (convert-direction v) :x i :y j ))))))

(defun turn-guard (guard)
  (setf (guard-direction guard)
        (case (guard-direction guard)
          (UP    'RIGHT)
          (DOWN  'LEFT)
          (LEFT  'UP)
          (RIGHT 'DOWN))))

(defun on-map (map x y)
  (destructuring-bind (rows cols) (array-dimensions map)
    (and (>= x 0) (>= y 0)
         (< y rows) (< x cols))))

(defun mark-guard (map guard)
  (setf (aref map (guard-y guard) (guard-x guard)) 'X))

(defun next-pos (guard)
  (case (guard-direction guard)
    (UP    (decf (guard-y guard)))
    (DOWN  (incf (guard-y guard)))
    (LEFT  (decf (guard-x guard)) )
    (RIGHT (incf (guard-x guard)))))

(defun prev-pos (guard)
  "moves the guard backwards and turns"
  (case (guard-direction guard)
    (UP    (incf (guard-y guard)))
    (DOWN  (decf (guard-y guard)))
    (LEFT  (incf (guard-x guard)) )
    (RIGHT (decf (guard-x guard))))
  guard)

(defun move-guard (map guard)
  (next-pos guard)
  (let ((x (guard-x guard))
        (y (guard-y guard)))
    (if (on-map map x y)
                                        ; check if we are on top of a barrier, and if so go back and turn instead
        (if (eql '|#| (aref map y x))
            (turn-guard (prev-pos guard))
            t)
                                        ; ran off the map
        (setf (guard-direction guard) nil))))

(defun solve (map guard)
  (mark-guard map guard)
  (loop while (guard-direction guard)
        do (mark-guard map guard)
        do (move-guard map guard)))

(defun run-p1 (file) 
  (let* ((map (list-to-2d-array (read-file file (lambda (s) (to-symbols s 'advt2024-d6)))))
         (guard (find-guard map)))
    ;; solve the puzzel
    (solve map guard)
    
    ;; count the spaces
    (destructuring-bind (rows cols) (array-dimensions map)
      (loop for y from 0 below rows
            sum (loop for x from 0 below cols
                      count (eq (aref map y x) 'X))))))

(defun save-move (guard move)
  (setf (gethash move (guard-moves guard)) t))

(defun reset-moves (guard)
  (setf (guard-moves guard) nil))

(defun guard-to-move (guard)
  (make-move :x (guard-x guard) :y (guard-y guard) :direction (guard-direction guard)))

(defun is-loop (x y map original-guard)
  ;; can only set new blocks in spaces where the guard originaly goes, ignoring initial position
  (unless (or (eql 'X (aref map y x)) (and (= x (guard-x original-guard)) (= y (guard-y original-guard)))) (return-from is-loop nil))
  (let ((guard (copy-guard original-guard)))
    ;; save the initial guard position
    (save-move guard (guard-to-move guard))
    ;; set the "new" block
    (setf (aref map y x) '|#|)
    ;; loop and check for guard loops
    (let ((result
            (loop
              while (move-guard map guard)
              for move = (guard-to-move guard) 
              ;; if we have seen the move before, then it is a loop
              if (gethash move (guard-moves guard))
                return t
              else
                do (save-move guard move))))

      ;; reset initial position
      (setf (aref map y x) 'X)
      ;; clear saved positions
      (clrhash (guard-moves guard))
      result)))


(defun run-p2 (file) 
  (let* ((map (list-to-2d-array (read-file file (lambda (s) (to-symbols s 'advt2024-d6)))))
         (guard (find-guard map))
         (guard2 (copy-guard guard)))
    (solve map guard2)
    (destructuring-bind (rows cols) (array-dimensions map)
      (loop for y from 0 below rows
            sum (loop for x from 0 below cols
                      count (is-loop x y map guard))))))

(defun run-p1-real ()
  "5080"
  (run-p1 *part1*))

(defun run-p1-test ()
  "41"
  (run-p1 *part1-test*))

(defun run-p2-real ()
  "1919"
  (run-p2 *part1*))

(defun run-p2-test ()
  "6"
  (run-p2 *part1-test*))
