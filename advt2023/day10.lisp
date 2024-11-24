;; Advent of Code 2023 day 10


(load  "utilities.lisp")

;; rules 

;;    | is a vertical pipe connecting north and south.
;;    - is a horizontal pipe connecting east and west.
;;    L is a 90-degree bend connecting north and east.
;;    J is a 90-degree bend connecting north and west.
;;    7 is a 90-degree bend connecting south and west.
;;    F is a 90-degree bend connecting south and east.
;;    . is ground; there is no pipe in this tile.
;;    S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.

(defparameter *maze-row* -1)
(defparameter *maze-start* nil)

(defstruct pipe pos in out orig (is-main nil))

(defun char-to-tile (c row col)
  (let ((pos (list row col)))
    (when (eq #\S c)
      (setf *maze-start* pos))

    (case c
      (#\| (make-pipe :pos pos :in (list (1- row) col) :out (list (1+ row) col) :orig c))
      (#\- (make-pipe :pos pos :in (list row (1- col)) :out (list row (1+ col)) :orig c))
      (#\L (make-pipe :pos pos :in (list (1- row) col) :out (list row (1+ col)) :orig c))
      (#\J (make-pipe :pos pos :in (list (1- row) col) :out (list row (1- col)) :orig c))
      (#\7 (make-pipe :pos pos :in (list row (1- col)) :out (list (1+ row) col) :orig c))
      (#\F (make-pipe :pos pos :in (list row (1+ col)) :out (list (1+ row) col) :orig c))
      (#\S 'S )
      (t nil)
      )
    ))

(defun read-maze-line (line)
  (incf *maze-row*)
  (loop for c in (coerce line 'list)
        for x from 0
        collect (char-to-tile c *maze-row* x)))

(defun reset-maze ()
  (setf *maze-row* -1)
  (setf *maze-start* nil))

(defun maze-to-array (maze)
  (let ((result (make-array (list (length maze) (length (car maze))))))
    (loop for row in maze
          for r from 0
          do (loop for value in row
                   for c from 0
                   do (setf (aref result r c) value)))

    result))

(defun pipe-to-pos (pipe pos)
  (and pipe 
       (or (equal (pipe-in pipe) pos)
           (equal (pipe-out pipe) pos))
       ))

;; returns t if pipes connect to one another
(defun pipes-connect (p1 p2)
  (or 
    (pipe-to-pos p1 (pipe-pos p2))
    (pipe-to-pos p2 (pipe-pos p1))))

;; TODO return both connections
;; find retuns a tile that connects to the start
(defun find-maze-start (maze start-location)
  (let* ((row (car start-location))
         (col (cadr start-location))
         (left (1- col))
         (right (1+ col))
         (up (1- row))
         (down (1+ row)))
    (cond 
      ;; check left
      ((and (>= left 0) 
            (aref maze row left)
            (pipe-to-pos (aref maze row left) start-location)) 
       (aref maze row left))
      ((and (>= up 0) 
            (aref maze up col)
            (pipe-to-pos (aref maze up col) start-location)) 
       (aref maze up col))
      ((and (< right (cadr (array-dimensions maze))) 
            (aref maze row right)
            (pipe-to-pos (aref maze row right) start-location)) 
       (aref maze row right))
      ((and (< down (car (array-dimensions maze))) 
            (aref maze down col)
            (pipe-to-pos (aref maze down col) start-location)) 
       (aref maze down col))
      )
    )
  )

(defun find-maze-start (maze start-location)
  (let* ((row (car start-location))
         (col (cadr start-location))
         (left (1- col))
         (right (1+ col))
         (up (1- row))
         (down (1+ row)))
    (apply #'values 
           (remove nil 
                   (list 
                     (when (and (>= left 0) 
                                (aref maze row left)
                                (pipe-to-pos (aref maze row left) start-location)) 
                       (aref maze row left))

                     (when (and (>= up 0) 
                                (aref maze up col)
                                (pipe-to-pos (aref maze up col) start-location)) 
                       (aref maze up col))
                     (when (and (< right (cadr (array-dimensions maze))) 
                                (aref maze row right)
                                (pipe-to-pos (aref maze row right) start-location)) 
                       (aref maze row right))
                     (when (and (< down (car (array-dimensions maze))) 
                                (aref maze down col)
                                (pipe-to-pos (aref maze down col) start-location)) 
                       (aref maze down col))
                     ))

           )
    )
  )

(defun get-next (prev-pos cur maze)
  (if (equal prev-pos (pipe-in cur))
      (aref maze (car (pipe-out cur)) (cadr (pipe-out cur)))
      (aref maze (car (pipe-in cur)) (cadr (pipe-in cur)))
    ))

(defun count-maze-steps (maze prev-pos cur cnt)
  (setf (pipe-is-main cur) t)
  (let ((next (get-next prev-pos cur maze)))
    (if (equal next 'S)
        (1+ cnt)
        (count-maze-steps maze (pipe-pos cur) next (1+ cnt)))))

;; Part 1

;; 4
(defun day10-test1 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10-test1" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    (ceiling (/ (count-maze-steps maze *maze-start* next 1) 2))
    ))

;; 8
(defun day10-test2 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10-test2" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    (ceiling (/ (count-maze-steps maze *maze-start* next 1) 2))
    )
  )

;; 7102
(defun day10-real1 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    (ceiling (/ (count-maze-steps maze *maze-start* next 1) 2))
    )
  )

;; Part 2

(defun clean-maze (maze)
  (loop for i from 0 below (array-total-size maze)
        for p = (row-major-aref maze i)
        when (and p (not (eq 'S p)) (not (pipe-is-main p)))
        do (setf (row-major-aref maze i) nil)))


;; TODO replace S with correct piece
(defun fix-s (maze s-loc)
  (let* ((row (car s-loc)) 
         (col (cadr s-loc))
         (max-row (car (array-dimensions maze)))
         (max-col (cadr (array-dimensions maze)))
         (is-up (and (> row 0) (pipe-to-pos (aref maze (1- row) col) s-loc)))
         (is-down (and (< row (1- max-row)) (pipe-to-pos (aref maze (1+ row) col) s-loc)))
         (is-left (and (> col 0) (pipe-to-pos (aref maze row (1- col)) s-loc)))
         (is-right (and (< col (1- max-col)) (pipe-to-pos (aref maze row (1+ col)) s-loc)))
         )
    (cond
      ((and is-left is-right) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list (1- row) col) :out (list (1+ row) col) :orig #\-)))
      ((and is-up is-down) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list row (1- col)) :out (list row (1+ col)) :orig #\|)))
      ((and is-up is-left) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list row (1- col)) :out (list (1- row) col) :orig #\J)))
      ((and is-up is-right) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list row (1- col)) :out (list (1+ row) col) :orig #\L)))
      ((and is-down is-right) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list row (1+ col)) :out (list (1+ row) col) :orig #\F)))
      ((and is-down is-left) 
       (setf (aref maze row col)
             (make-pipe :pos s-loc :in (list row (1+ col)) :out (list (1- row) col) :orig #\7)))
       
      )
    )
  )


;; todo don't count going in/out for horizontal movement
(defun count-interior-lr (maze)
  (destructuring-bind (rows columns) (array-dimensions maze)
    (loop for i from 0 below rows
          for inside = nil
          sum (loop for j from 0 below columns
                    for cur = (aref maze i j)

                    when (and cur (or (eq 'S cur) (not (= (car (pipe-in cur)) (car (pipe-out cur))))) )
                    do (setf inside (not inside))

                    when (and inside (not cur))
                    do (setf (aref maze i j) 'I)
                    when cur
                    do (setf (aref maze i j) 'P)

                    when (and inside (not cur))
                    sum 1
                    ))
    ))

(defun find-interior (maze)
  (destructuring-bind (rows columns) (array-dimensions maze)
    (loop for x from (1- rows) downto 0
          for inside = nil
          do (loop for row from x below rows
                   for col from 0 below columns
                   for p = (aref maze row col)
                   when (and p (find (pipe-orig p) '(#\| #\- #\F #\J)))
                   do (setf inside (not inside))
                   when (and inside (not p))
                   do (setf (aref maze row col) #\I)
                   when (and (not inside) (not p) )
                   do (setf (aref maze row col) #\O)

                   when p
                   do (setf (aref maze row col) (pipe-orig p))

                   ))
    (loop for y from 1 below columns
          for inside = nil
          do (loop for row from 0 below rows
                   for col from y below columns
                   for p = (aref maze row col)

                   when (and p (find (pipe-orig p) '(#\| #\- #\F #\J)))
                   do (setf inside (not inside))
                   when (and inside (not p))
                   do (setf (aref maze row col) #\I)
                   when (and (not inside) (not p) )
                   do (setf (aref maze row col) #\O)

                   when p
                   do (setf (aref maze row col) (pipe-orig p))
                   ))
    )
  maze)

(defun count-interior (maze)
  (loop for x from 0 below (array-total-size maze)
        when (eq (row-major-aref maze x) #\I)
        sum 1)
  )

;; 4
(defun day10-test3 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10-test3" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    ;; mark what is in maze
    (count-maze-steps maze *maze-start* next 1)
    (clean-maze maze)
    (fix-s maze *maze-start*)
    (find-interior maze)
    (count-interior maze)
    )
  )

;; 8
(defun day10-test4 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10-test4" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    ;; mark what is in maze
    (count-maze-steps maze *maze-start* next 1)
    (clean-maze maze)
    (fix-s maze *maze-start*)
    (find-interior maze)
    (count-interior maze)
    )
  )

;; 10 
(defun day10-test5 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10-test5" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    ;; mark what is in maze
    (count-maze-steps maze *maze-start* next 1)
    (clean-maze maze)
    (fix-s maze *maze-start*)
    (find-interior maze)
    (count-interior maze)
    )
  )

;; 363
(defun day10-real2 ()
  (reset-maze)
  (let* ((maze (maze-to-array (read-file "inputs/day10" #'read-maze-line)))
         (next (find-maze-start maze *maze-start*))
         )
    ;; mark what is in maze
    (count-maze-steps maze *maze-start* next 1)
    (clean-maze maze)
    (fix-s maze *maze-start*)
    (find-interior maze)
    (count-interior maze)
    )
  )
