
(ql:quickload "cl-ppcre")
(ql:quickload "str")


(defun read-file (file-name process-line)
  (with-open-file (in file-name :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (funcall process-line line)))))



(defun manhat-dist (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cadr p1) (cadr p2)))))

(defun manhat-dist-2 (x1 y1 p2)
  (+ (abs (- x1 (car p2)))
     (abs (- y1 (cadr p2)))))
