(in-package :aoc-utils)


(defun list-to-2d-array (lst)
  "converts the given list into a 2d array"
  (let ((rows (length lst))
        (cols (length (car lst))))
    (make-array (list rows cols) :initial-contents lst)))


(defun read-file (file-name process-line)
  "processes the given file line by line with the function process-line"
  (with-open-file (in file-name :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (funcall process-line line)))))


(defun transpose (lst)
  "transposes the given list"
  (apply #'mapcar #'list lst))

(defun manhat-dist (p1 p2)
  "calculate the manhaten distance between the points p1 and p2"
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cadr p1) (cadr p2)))))

(defun manhat-dist-2 (x1 y1 p2)
  "calculate the manhaten distance between the points x1, y1 and p2"
  (+ (abs (- x1 (car p2)))
     (abs (- y1 (cadr p2)))))