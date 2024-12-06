(in-package :aoc-utils)

(defun to-symbols (str)
  "converts a string to a list of symbols"
  (loop for x across str collect (intern (string x))))

(defun list-to-2d-array (lst)
  "converts the given list into a 2d array"
  (let ((rows (length lst))
        (cols (length (car lst))))
    (make-array (list rows cols) :initial-contents lst)))


(defun read-file (file-name process-line)
  "processes the given file line by line with the function process-line"
  (with-open-file (in (concatenate 'string (sb-posix:getcwd) "/" file-name) :if-does-not-exist nil) 
    (if in
      (loop for line = (read-line in nil)
            while line
            collect (funcall process-line line))
      "not found")))


(defun transpose (lst)
  "transposes the given list"
  (apply #'mapcar #'list lst))

(defun flatten (lst)
  "flattens a list of list"
  (apply #'concatenate 'list lst))

(defun manhat-dist (p1 p2)
  "calculate the manhaten distance between the points p1 and p2"
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cadr p1) (cadr p2)))))

(defun manhat-dist-2 (x1 y1 p2)
  "calculate the manhaten distance between the points x1, y1 and p2"
  (+ (abs (- x1 (car p2)))
     (abs (- y1 (cadr p2)))))
