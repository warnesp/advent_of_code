;; Advent of Code 2024 day 6

(in-package :advt2024-d6)

(defvar *part1* "inputs/day06-part1")
(defvar *part1-test* "inputs/day06-part1-test")

(defstruct move x y direction)

(defstruct guard direction x y (moves (make-hash-table :test 'equalp)))

(defun convert-direction (g)
  (case g
    (^ 'up)
    (> 'right)
    (< 'left)
    (v 'down)
    ))


(defun find-guard (map)
  (destructuring-bind (rows cols) (array-dimensions map)
    (loop for j from 0 below rows
          do (loop for i from 0 below cols
                   for v = (aref  map j i)
                   when (not (or (eql '|.| v) (eql '|#| v)))
                     do (return-from find-guard (make-guard :direction (convert-direction v) :x i :y j )))
  )))

(defun turn-guard (guard)
  (case (guard-direction guard)
      (UP (setf (guard-direction guard) 'RIGHT))
      (DOWN (setf (guard-direction guard) 'LEFT))
      (LEFT (setf (guard-direction guard) 'UP))
      (RIGHT (setf (guard-direction guard) 'DOWN))))

(defun on-map (map x y)
  (destructuring-bind (rows cols) (array-dimensions map)
    (and (>= x 0) (>= y 0)
         (< y rows) (< x cols))))

(defun mark-guard (map guard)
  (setf (aref map (guard-y guard) (guard-x guard)) 'X))

(defun next-pos (guard)
  (case (guard-direction guard)
    (UP (list (guard-x guard) (1- (guard-y guard))))
    (DOWN (list (guard-x guard) (1+ (guard-y guard))))
    (LEFT (list (1- (guard-x guard)) (guard-y guard)))
    (RIGHT (list (1+ (guard-x guard)) (guard-y guard)))
    ))

(defun move-guard (map guard)
  (destructuring-bind (x y) (next-pos guard)
    (if (on-map map x y)
        (if (eql '|#| (aref map y x))
            (turn-guard guard)
            (progn (setf (guard-x guard) x)
                   (setf (guard-y guard) y))) 
        (setf (guard-direction guard) nil)
    )))

(defun run-p1 (file) 
  (let* ((map (list-to-2d-array (read-file file #'to-symbols)))
         (guard (find-guard map)))
    (mark-guard map guard)
    (loop while (guard-direction guard)
          do (mark-guard map guard)
          do (move-guard map guard)
          )
    
  (destructuring-bind (rows cols) (array-dimensions map)
    (loop for y from 0 below rows sum (loop for x from 0 below cols count (eql (aref map y x) 'X))))))

(defun save-move (guard move)
  (setf (gethash move (guard-moves guard)) t))

(defun reset-moves (guard)
  (setf (guard-moves guard) nil))

(defun is-loop (x y map original-guard)
  ;; can only set new blocks in blank spaces
  (unless (eql '|.| (aref map y x)) (return-from is-loop nil))
  (let ((guard (copy-guard original-guard)))
    ;; save the initial guard position
    (save-move guard (make-move :x (guard-x guard) :y (guard-y guard) :direction (guard-direction guard)))
    ;; set the "new" block
    (setf (aref map y x) '|#|)
    ;; loop and check for guard loops
    (let ((result
            (loop
              while (move-guard map guard)
              for move = (make-move :x (guard-x guard) :y (guard-y guard) :direction (guard-direction guard))
              when (gethash move (guard-moves guard))
                return t
              do (save-move guard move)
              finally
                 (return nil))))

      ;; reset initial position
      (setf (aref map y x) '|.|)
      (clrhash (guard-moves guard))
      result)))


(defun run-p2 (file) 
  (let* ((map (list-to-2d-array (read-file file #'to-symbols)))
         (guard (find-guard map)))
    
    (destructuring-bind (rows cols) (array-dimensions map)
      (loop for y from 0 below rows
            sum (loop for x from 0 below cols
                      count (is-loop x y map guard)))
      )))

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
