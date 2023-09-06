;; day 8 tree top house

(defparameter *forest* nil)
(defparameter *from-left* nil)
(defparameter *from-right* nil)
(defparameter *from-top* nil)
(defparameter *from-bottom* nil)

(defparameter *view-from-left* nil)
(defparameter *view-from-right* nil)
(defparameter *view-from-top* nil)
(defparameter *view-from-bottom* nil)

(defun make-forest-arr (forest)
  (make-array (list (length (car forest)) (length forest))))

(defun tree-at (x y)
  (aref *forest* x y))

(defun char-to-int (c)
  (- (char-int c) 48))

(defun visiblep (tree n1 n2 n3 n4)
  (> tree (min n1 n2 n3 n4)))

(defun visible (x y)
  (visiblep  (tree-at x y)
             (aref *from-right* (1+ x) y)
             (aref *from-top* x (1- y))
             (aref *from-left* (1- x) y)
             (aref *from-bottom* x (1+ y))
             ))

(defun fill-from-left-row (input output row)
  (loop for i from 0 below (car (array-dimensions input))
        for x from 0 below (car (array-dimensions output))
        do (cond
             ((= 0 x) (setf (aref output 0 row) (aref input 0 row)))
             ((> (aref input i row ) (aref output (1- x) row)) (setf (aref output x row) (aref input i row)))
             (t  (setf (aref output x row) (aref output (1- x) row)))
             )))
(defun fill-from-right-row (input output row)
  (loop for i from (1- (car (array-dimensions input))) downto 0
        for x from (1- (car (array-dimensions output))) downto 0
        do (cond
             ((= (1- (car (array-dimensions output))) x) (setf (aref output x row) (aref input x row)))
             ((> (aref input i row) (aref output (1+ x) row)) (setf (aref output x row) (aref input i row)))
             (t  (setf (aref output x row) (aref output (1+ x) row)))
             )))
(defun fill-from-top-column (input output column)
  (loop for i from 0 below (cadr (array-dimensions input))
        for x from 0 below (cadr (array-dimensions output))
        do (cond
             ((= 0 x) (setf (aref output column 0) (aref input column 0)))
             ((> (aref input column i) (aref output column (1- x))) (setf (aref output column x) (aref input column i)))
             (t  (setf (aref output column x) (aref output column (1- x))))
             )))

(defun fill-from-bottom-column (input output column)
  (loop for i from (1- (cadr (array-dimensions input))) downto 0
        for x from (1- (cadr (array-dimensions output))) downto 0
        do (cond
             ((= (1- (cadr (array-dimensions output))) x) (setf (aref output column x) (aref input column x)))
             ((> (aref input column i) (aref output column (1+ x))) (setf (aref output column x) (aref input column i)))
             (t  (setf (aref output column x) (aref output column (1+ x))))
             )))

(defun fill-from-top ()
  (loop for column from 0 below (cadr (array-dimensions *forest*))
        do (fill-from-top-column *forest* *from-top* column)))

(defun fill-from-bottom ()
  (loop for column from 0 below (cadr (array-dimensions *forest*))
        do (fill-from-bottom-column *forest* *from-bottom* column)))

(defun fill-from-left ()
  (loop for row from 0 below (car (array-dimensions *forest*))
        do (fill-from-left-row *forest* *from-left* row)))

(defun fill-from-right ()
  (loop for row from 0 below (car (array-dimensions *forest*))
        do (fill-from-right-row *forest* *from-right* row)))

(defun fill-helpers ()
  (loop for row from 0 below (car (array-dimensions *forest*))
        do (fill-from-left-row *forest* *from-left* row)
           (fill-from-right-row *forest* *from-right* row)
        )
  (loop for column from 0 below (cadr (array-dimensions *forest*))
        do (fill-from-bottom-column *forest* *from-bottom* column)
           (fill-from-top-column *forest* *from-top* column)
        ))
(defun backtrace-left (input x row)
  (loop for i from (1- x) downto 0 sum 1 until (<= (aref input x row) (aref input i row)) )
  )
(defun fill-view-from-left-row (input output row)
  (loop for i from 0 below (car (array-dimensions input))
        for x from 0 below (car (array-dimensions output))
        do (cond
             ((= 0 x) (setf (aref output 0 row) 0))
             ((> (aref input i row ) (aref input (1- i) row)) (setf (aref output x row) (backtrace-left input i row)))
             (t  (setf (aref output x row) 1))
             )))
(defun backtrace-right (input x row)
   (loop for i from (1+ x) below (car (array-dimensions input)) sum 1 until (<= (aref input x row) (aref input i row)))
      )
(defun fill-view-from-right-row (input output row)
  (loop for i from (1- (car (array-dimensions input))) downto 0
        for x from (1- (car (array-dimensions output))) downto 0
        do (cond
             ((= (1- (car (array-dimensions output))) x) (setf (aref output x row) 0))
             ((> (aref input i row) (aref input (1+ i) row)) (setf (aref output x row) (backtrace-right input i row)))
             (t  (setf (aref output x row) 1))
             )))

(defun backtrace-top (input column y)
  (loop for i from (1- y) downto 0 sum 1 until (<= (aref input column y) (aref input column i)) )
      )
(defun fill-view-from-top-column (input output column)
  (loop for i from 0 below (cadr (array-dimensions input))
        for x from 0 below (cadr (array-dimensions output))
        do (cond
             ((= 0 x) (setf (aref output column 0) 0))
             ((> (aref input column i) (aref input column (1- i))) (setf (aref output column x) (backtrace-top input column i)))
             (t  (setf (aref output column x) 1))
             )))
(defun backtrace-bottom (input column y)
  (loop for i from (1+ y) below (cadr (array-dimensions input)) sum 1 until (<= (aref input column y) (aref input column i)))
      )
(defun fill-view-from-bottom-column (input output column)
  (loop for i from (1- (cadr (array-dimensions input))) downto 0
        for x from (1- (cadr (array-dimensions output))) downto 0
        do (cond
             ((= (1- (cadr (array-dimensions output))) x) (setf (aref output column x) 0))
             ((> (aref input column i) (aref input column (1+ i))) (setf (aref output column x) (backtrace-bottom input column i)))
             (t  (setf (aref output column x) 1))
             )))

(defun fill-view-helpers ()
  (loop for row from 0 below (car (array-dimensions *forest*))
        do (fill-view-from-left-row *forest* *view-from-left* row)
           (fill-view-from-right-row *forest* *view-from-right* row)
        )
  (loop for column from 0 below (cadr (array-dimensions *forest*))
        do (fill-view-from-bottom-column *forest* *view-from-bottom* column)
           (fill-view-from-top-column *forest* *view-from-top* column)
        ))

(defun read-treetop-file ()
  (let ((forest nil))
    (with-open-file (in "input-day08" :if-does-not-exist nil) 
      (when in
        (loop for line = (read-line in nil)
              while line do
                (push (mapcar #'char-to-int (coerce line 'list)) forest)
              )))
    (setq *forest* (make-forest-arr forest))
    (loop for y from (1- (length forest)) downto 0
          for row in forest do
      (loop for x from 0 below (length row)
            for cell in row do
            (setf (aref *forest* x y) cell)))
    (setq *from-left* (make-forest-arr forest))
    (setq *from-right* (make-forest-arr forest))
    (setq *from-top* (make-forest-arr forest))
    (setq *from-bottom* (make-forest-arr forest))
    (setq *view-from-left* (make-forest-arr forest))
    (setq *view-from-right* (make-forest-arr forest))
    (setq *view-from-top* (make-forest-arr forest))
    (setq *view-from-bottom* (make-forest-arr forest))
    nil
    ))

(defun to-visible ()
  (let ((edge-trees (+  (* 2 (apply #'+ (array-dimensions *forest*))) -4)))
  (+ (loop for x from 1 below (1- (car (array-dimensions *forest*)))
           sum
           (loop for y from 1 below (1- (cadr (array-dimensions *forest*)))
                 when (visible x y)
                   sum 1
                 )
           )
     edge-trees))
  )

(defun to-visible-grid ()
  (loop for x from 1 below (1- (car (array-dimensions *forest*)))
          collect 
           (loop for y from 1 below (1- (cadr (array-dimensions *forest*)))
                 collect (visible x y)
                 )
           )
  )

(defun find-vis (x y)
  (*
   (aref *view-from-top* x y)
   (aref *view-from-left* x y)
   (aref *view-from-bottom* x y)
   (aref *view-from-right* x y)
   ))

(defun to-max-vis ()
  (loop for x from 1 below (1- (car (array-dimensions *forest*)))
        maximize
        (loop for y from 1 below (1- (cadr (array-dimensions *forest*)))
              maximize (find-vis x y))))
