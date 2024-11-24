;; Advent of Code day 14 regolith reservior

(defparameter *sand* '(500 0))

(defparameter *cave* nil)

(defun parse-pair (txt)
  (mapcar #'parse-integer (uiop:split-string txt :separator ",")))

(defun process-line (line)
  (remove nil (mapcar #'parse-pair (uiop:split-string line :separator "->"))))

(defun read-reservior-file ()
  (with-open-file (in "input-day14-test" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (process-line line )))))


(defun find-x (lines fn)
  (labels ((find-line-x (line)
             (apply fn (mapcar #'car line))))
    (apply fn (mapcar #'find-line-x lines ))))

(defun find-y (lines fn)
  (labels ((find-line-y (line)
             (apply fn (mapcar #'cadr line))))
  (apply fn (mapcar #'find-line-y lines ))))


(defun adjust-lines (min-x lines)
  (loop for line in lines
        collect
        (loop for point in line
              collect
              (list (- (car point) min-x)  (cadr point) )))
  )

(defun draw-line (board line)
  (when (cdr line)
    (let ((x1 (car (car line))) (x2 (car (cadr line)))
          (y1 (cadr (car line))) (y2 (cadr (cadr line))))
      (loop for x
            from (min x1 x2)
              to (max x1 x2)
            do (setf (aref board x (cadr (car line))) 'r))
      (loop for y
            from (min y1 y2)
              to (max y1 y2)
            do (setf (aref board (car (car line)) y) 'r))
      (draw-line board (cdr line))
      )))

(defun print-spot (s)
  (princ (cond ((eq s 'r) #\#)
               ((eq s 's) #\o)
               ((eq s '+) #\+)
               (t #\.))))

(defun print-board (board)
  (loop for y from 0 below (cadr (array-dimensions board))
        do (fresh-line)
           (loop for x
                 from 0 below (car (array-dimensions board))
                 do (print-spot (aref board x y))
                 )

        ))
(defun make-game-board (lines)
  (let* ((min-x (find-x lines #'min))
         (max-x (find-x lines #'max))
         (max-y (find-y lines #'max))
         (board (make-array (list (+ (- max-x min-x) 2)  (1+ max-y)) :initial-element nil)))
    (loop for line in (adjust-lines min-x lines) do (draw-line board line))
    (setf (aref board (- (car *sand*) min-x) (cadr *sand*)) '+)
    board
    ;;(adjust-lines min-x min-y lines)
    )
  )

(defun move-sand (board sand)
  (cond
    ;; check for bottom of board
    ((>= (1+ (cadr sand)) (cadr (array-dimensions board))) nil)
    ;; space below
    ((eq nil (aref board (car sand) (1+ (cadr sand))))
     (incf (cadr sand))
     (move-sand board sand))
    ;; down and left
    ((eq nil (aref board (1- (car sand)) (1+ (cadr sand))))
     (incf (cadr sand))
     (decf (car sand))
     (move-sand board sand))
    ;; down and right
    ((eq nil (aref board (1+ (car sand)) (1+ (cadr sand))))
     (incf (cadr sand))
     (incf (car sand))
     (move-sand board sand))
    ;; no where to go, stop
    (t sand)
    ))

(defun find-sand-start (board)
  (let ((found-x nil))
    (loop
      for y from 0 below (cadr (array-dimensions board))
      when (loop
             for x from 0 below (car (array-dimensions board))
             when (eq (aref board x y) '+)
               do (setq found-x x)
                  (return x)
             )
        return (list found-x y))))

(defun simulate-sand (board)
  (let ((running t) (sand-start (find-sand-start board)))
    (loop
      while running
      do
         (let ((cur-sand (copy-list sand-start)))

           (setf running nil)
           ))))

