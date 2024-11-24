;; Advent of Code day 15 - beacon exclusion zone

(defparameter *beacons* nil)
(defparameter *sensors* nil)

(defstruct sensor pos manhat-dist beacon)

(defun manhat-dist (p1 p2)
  (+ (abs (- (car p1) (car p2)))
     (abs (- (cadr p1) (cadr p2)))))

(defun manhat-dist-2 (x1 y1 p2)
  (+ (abs (- x1 (car p2)))
     (abs (- y1 (cadr p2)))))

(defun parse-pos (str)
  (let ((parts (remove "" (uiop:split-string str :separator "xy= ,") :test 'string=)))
    (list (parse-integer (car parts)) (parse-integer (cadr parts)))
    ))

(defun process-line (line)
  (let* ((parts (uiop:split-string line :separator ":"))
         (sensor (parse-pos (subseq (car parts) (length "Sensor at "))))
         (beacon (parse-pos (subseq (cadr parts) (length " closest beacon is at "))))
         )
    (make-sensor :pos sensor :manhat-dist (manhat-dist sensor beacon) :beacon beacon) 
    
    ))
(defun read-beacon-file ()
  (with-open-file (in "input-day15" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (process-line line )))))

;; ..####B######################..
;; 26 hash marks; B does not count
;;find min-x, max-x
;; prune sensors where manhat-dist > manhat-dist to same x y = 2,000,000
;; count every point between (min-x max-x) y=fixed-y manhat-dist <= sensor manhat-dist
;; subtract number of beacons on line

(defun find-x (sensors fun)
  (apply fun
         (append (mapcar (lambda (s) (+ (sensor-manhat-dist s) (car (sensor-pos s)))) sensors)
                 (mapcar (lambda (s) (- (sensor-manhat-dist s) (car (sensor-pos s)))) sensors)
                 (mapcar #'car (mapcar #'sensor-beacon sensors)))))

(defun beacons-on-y (sensors y)
  (length (remove-if-not (lambda (b) (= y (cadr b)))
                         (remove-duplicates 
                          (mapcar #'sensor-beacon sensors)
                          :test #'equal))))

(defun get-not-possible (sensors fixed-y)
  (let* ((min-x (find-x sensors #'min))
         (max-x (find-x sensors #'max))
         (pruned-sensors (remove-if (lambda (b)
                                      (< (sensor-manhat-dist b)
                                         (manhat-dist (sensor-pos b) (list (car (sensor-pos b)) fixed-y))))
                                    sensors))
         )
    (fresh-line)
    (princ "[")
    (princ min-x )
    (princ ",")
    (princ max-x )
    (princ "]")
    (apply #'append 
           (loop for s in pruned-sensors
                 collect (loop for x
                               from min-x to max-x
                               when (<= (manhat-dist (list x fixed-y) (sensor-pos s))
                                        (sensor-manhat-dist s))
                                 collect  x)
                 ))
    ))
(defun count-not-possible (y)
  (let ((sensors (read-beacon-file)))
  (- (length (remove-duplicates (get-not-possible sensors y) :test #'equal))
    (beacons-on-y sensors y))))

(defun point-in-rangep (s x y)
  (<= (manhat-dist-2 x y (sensor-pos s)) (sensor-manhat-dist s)))
(defparameter *itrs* 0)
(defun find-first-empty (sensors max-p)
  (setf *itrs* 0)
  (loop for y from 0 to max-p do
    (let ((res (loop for x from 0 to max-p
                     unless (loop for s in sensors
                                  when (point-in-rangep s x y)
                                    do (incf *itrs*)
                                       (incf x (+ (max 0 (- (car (sensor-pos s)) x))
                                                  (- (sensor-manhat-dist s) (manhat-dist-2 x y (sensor-pos s)))))
                                       (return t))
                return (list x y) 
                )))
      (when res (return res))
      ))
  )

(defun calc-search-freq (pos)
  (+ (* 4000000 (car pos)) (cadr pos)))
