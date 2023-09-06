;; advent of code day 12 Hill Climbing Alorithm

(defparameter *to-check* nil)
(defparameter *map* (make-array '(1 1)))
(defparameter *map-dist* (make-array '(1 1)))

(defparameter *start-pos* '(0 0))
(defparameter *lowest-pos* '((0 0)))
(defparameter *end-pos* '(0 0))

(defun process-line (line row)
  (let ((map-line (coerce line 'list)))
    (loop for cell in map-line
          for column from 0
          collect
          (cond ((eq cell #\S) (setf *start-pos* (list column row)) (push *start-pos* *lowest-pos*) 97)
                ((eq cell #\E) (setf *end-pos* (list column row)) 122)
                ((eq cell #\a) (push (list column row) *lowest-pos*) 97)
                (t (char-int cell))
                )
          )))

(defmacro get-height (row column)
  (list 'aref '*map* column row))

(defmacro get-moves (row column)
  (list 'aref '*map-dist* column row))

(defun  map-to-array (map)
  (let ((width (length (car map))) (height (length map)))
    (make-array (list height width) :initial-contents map)))

(defun read-hill-file ()
  (with-open-file (in "input-day12" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            for row from 0
            while line collect
                       (process-line line row)))))
(defun reset-map-dist ()
  (setf *map-dist* (make-array (array-dimensions *map*) :initial-element 999999)))
(defun load-map ()
  (setf *lowest-pos* nil)
  (setf *map* (map-to-array (read-hill-file)))
  )

(defun in-boundsp (cell)
  (let ( (nx (car cell)) (ny (cadr cell)))
    (cond
      ((< nx 0) nil)
      ((< ny 0) nil)
      ((>= nx (cadr (array-dimensions *map*))) nil)
      ((>= ny (car (array-dimensions *map*))) nil)
      (t t)
      )))

(defun is-valid-move (prev next)
  (let ((px (car prev)) (py (cadr prev)) (nx (car next)) (ny (cadr next)))
    (cond
      ((not (in-boundsp next)) nil)
      ((>= (1+ (get-height px py)) (get-height nx ny)) t)
      (t nil)
      )))

(defun gen-moves (prev)
  (let ((px (car prev)) (py (cadr prev)))
    (remove-if-not #'in-boundsp 
                   (list
                    (list (1+ px) py);; right
                    (list (1- px) py);; left
                    (list px (1+ py));; down 
                    (list px (1- py));; up
                    ))))

(defun update-moves (prev next)
  (let ((px (car prev)) (py (cadr prev)) (nx (car next)) (ny (cadr next)))
    (let ((new-moves (1+ (get-moves px py))))
      (when (< new-moves (get-moves nx ny))
        (setf (get-moves nx ny) new-moves)
        )
      )))

(defun check-and-add (prev move)
  (when (and move (is-valid-move prev move))
    (when (update-moves prev move)
      (push move *to-check*))
    ))

(defun find-map-path ()
  (load-map)
  (reset-map-dist)
  (setf (get-moves (car *start-pos*) (cadr *start-pos*)) 0)
  (setf *to-check* nil)
  (push *start-pos* *to-check*)
  (loop while *to-check* do
    (let* ((prev (pop *to-check*)) (moves (gen-moves prev))
           (m1 (nth 0 moves)) (m2 (nth 1 moves)) (m3 (nth 2 moves))
           (m4 (nth 3 moves)))
      (check-and-add prev m1)
      (check-and-add prev m2)
      (check-and-add prev m3)
      (check-and-add prev m4)
      ))
  (get-moves (car *end-pos*) (cadr *end-pos*)))

(defun find-map-path ()
  (load-map)
  (reset-map-dist)
  (setf (get-moves (car *start-pos*) (cadr *start-pos*)) 0)
  (setf *to-check* nil)
  (push *start-pos* *to-check*)
  (loop while *to-check* do
    (let* ((prev (pop *to-check*)) (moves (gen-moves prev))
           (m1 (nth 0 moves)) (m2 (nth 1 moves)) (m3 (nth 2 moves))
           (m4 (nth 3 moves)))
      (check-and-add prev m1)
      (check-and-add prev m2)
      (check-and-add prev m3)
      (check-and-add prev m4)
      ))
  (get-moves (car *end-pos*) (cadr *end-pos*)))
