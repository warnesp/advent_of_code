;; 2023 Advent of Code Day 5

(load  "utilities.lisp")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(defstruct almanac-entry dest-start source-start range)
(defstruct almanac-map name entries)
(defstruct almanac seeds maps)

(defparameter *almanac* nil)

(defun read-seeds (line)
  (let ((seeds (str:split #\Space (subseq line 6) :omit-nulls t)))
    (loop for seed in seeds
          collect (parse-integer seed :junk-allowed t))))

(defun start-new-map (line)
  (push (make-almanac-map :name line) (almanac-maps *almanac*)))

(defun add-to-map (line)
  (let ((entry-values (mapcar (lambda (str) (parse-integer str :junk-allowed t))
                              (str:split #\Space line)) ))
    (push (make-almanac-entry :dest-start (car entry-values)
                              :source-start (cadr entry-values)
                              :range (caddr entry-values))
          (almanac-map-entries (car (almanac-maps *almanac*)))))
  )

(defun read-almanac (line)
  (cond ((= (length line) 0) nil)
        ((str:starts-with-p "seeds:" line) (setf (almanac-seeds *almanac*) (read-seeds line)) )
        ((str:ends-with-p " map:" line) (start-new-map line))
        (t (add-to-map line))))

(defun in-range (entry value)
  (and (<= (almanac-entry-source-start entry) value) (< value (+ (almanac-entry-source-start entry) (almanac-entry-range entry)))))

(defun apply-entry (entry value)
  (+ (almanac-entry-dest-start entry) (- value (almanac-entry-source-start entry))))

(defun almanac-map-convert (map value)
  (let ((v (loop for entry in (almanac-map-entries map)
        when (in-range entry value)
          return (apply-entry entry value))))
    (if v v value)))

(defun almanac-convert (almanac value)
  (labels ((ac-rec (maps currValue)
             (if maps
                 (ac-rec (cdr maps) (almanac-map-convert (car maps) currValue))
                 currValue)
             ))
    (ac-rec (almanac-maps almanac) value)))

(defun combine-maps (map1 map2)
  (let ((new-name (concatenate 'string (almanac-map-name map1) (almanac-map-name map2)))
        (es1 (almanac-map-entries map1))
        (es2 (almanac-map-entries map2))
        )
    (loop for e1 in es1
          for e2 in es2
          )
    (make-almanac-map :name new-name
                      :entries entries)))

(defun reset-almanac () 
  (setf *almanac* (make-almanac)))

(defun find-lowest (almanac)
  (loop for v in (almanac-seeds almanac)
        minimizing (almanac-convert almanac v)))

(defun compare-entry-source (e1 e2)
  (< (almanac-entry-source-start e1) (almanac-entry-source-start e2)))

(defun sort-entries (map)
  (setf (almanac-map-entries map)  (sort (almanac-map-entries map) #'compare-entry-source)))

(defun fill-entry-gaps (map)
  (let ((first-entry-start (almanac-entry-source-start (car (almanac-map-entries map)))))
    (when (< 0 first-entry-start)
      (push (make-almanac-entry :dest-start 0 :source-start 0 :range first-entry-start)
            (almanac-map-entries map))
      )
    )
  (setf (almanac-map-entries map)
        (append (almanac-map-entries map)
                (loop for prev in (almanac-map-entries map)
                      for next in (cdr (almanac-map-entries map))
                      for end-prev = (+ (almanac-entry-source-start prev) (almanac-entry-range prev))
                      for gap = (- (almanac-entry-source-start next) end-prev)
                      when (> gap 0)
                        collect (make-almanac-entry :dest-start end-prev :source-start end-prev :range gap)
                      ))))

(defun find-top-range (maps)
  (loop for m in maps
        maximizing
        (loop for e in (almanac-map-entries m)
              maximizing (+ (almanac-entry-source-start e) (almanac-entry-range e)) 
              maximizing (+ (almanac-entry-dest-start e) (almanac-entry-range e)) )
        ))

(defun add-top (map top-range)
  (let ((last-entry (car (last (almanac-map-entries map)))))
    (when (> top-range (+ (almanac-entry-source-start last-entry) (almanac-entry-range last-entry))))
    ))

(defun fix-map (map)
  (sort-entries map)
  (fill-entry-gaps map)
  (sort-entries map)
  )


;; 35
(defun day5-test1 ()
  (reset-almanac)
  (read-file "inputs/day5-test1" #'read-almanac)
  (setf (almanac-maps *almanac*) (reverse (almanac-maps *almanac*)))
  (dolist (m (almanac-maps *almanac*)) (fix-map m))
  (find-lowest *almanac*))

;; 993500720
(defun day5-real1 ()
  (reset-almanac)
  (read-file "inputs/day5" #'read-almanac)
  (setf (almanac-maps *almanac*) (reverse (almanac-maps *almanac*)))
  (dolist (m (almanac-maps *almanac*)) (fix-map m))
  (find-lowest *almanac*))

;; Part 2


(defun find-lowest-alt (almanac)
  (loop for start in (almanac-seeds almanac) by #'cddr
        for range in (cdr (almanac-seeds almanac)) by #'cddr
        for end = (+ start range)
        do (princ "*")
        minimizing
        (loop for x from start to end
              minimizing (almanac-convert almanac x))))


;; 46
(defun day5-test2 ()
  (reset-almanac)
  (read-file "inputs/day5-test1" #'read-almanac)
  (setf (almanac-maps *almanac*) (reverse (almanac-maps *almanac*)))
  (dolist (m (almanac-maps *almanac*)) (fix-map m))
  (find-lowest-alt *almanac*)
  )

;; 4917124
(defun day5-real2 ()
  (reset-almanac)
  (read-file "inputs/day5" #'read-almanac)
  (setf (almanac-maps *almanac*) (reverse (almanac-maps *almanac*)))
  (dolist (m (almanac-maps *almanac*)) (fix-map m))
  (find-lowest-alt *almanac*)
  )

