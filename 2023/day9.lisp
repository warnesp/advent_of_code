;; 2023 Advent of Code Day 9

(load  "utilities.lisp")


(defun expand-seq (seq)
  (if (= (length (car seq)) (count 0  (car seq) ))
      seq
      (expand-seq (cons (loop for i in (car seq)
                              for j in (cdar seq)
                              collect (- j i)) seq))))


(defun predict-next (seq)
  (labels ((pn-rec (seq accum)
                   (if seq
                       (pn-rec (cdr seq) (cons (+ (car (last (car seq)))  (car accum)) accum))
                       accum)))
          (car (pn-rec seq '(0)))
          ))

(defun read-report (line)
  (list (mapcar #'parse-integer (str:split #\Space line))))


;;;;;;;;;;;;
;; Part 1 ;;
;;;;;;;;;;;;

;; 114
(defun day9-test1 ()
  (apply #'+ (mapcar #'predict-next (mapcar #'expand-seq (read-file "inputs/day9-test1" #'read-report))))
  )


;; 1987402313
(defun day9-real1 ()
  (apply #'+ (mapcar #'predict-next (mapcar #'expand-seq (read-file "inputs/day9" #'read-report))))
  )


;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;

(defun read-report-r (line)
  (list (reverse (mapcar #'parse-integer (str:split #\Space line)))))


;; 
(defun day9-test2 ()
  (apply #'+ (mapcar #'predict-next (mapcar #'expand-seq (read-file "inputs/day9-test1" #'read-report-r))))
  )


;; 1987402313
(defun day9-real2 ()
  (apply #'+ (mapcar #'predict-next (mapcar #'expand-seq (read-file "inputs/day9" #'read-report-r))))
  )


;; 
(defun day9-test2 ()
  (apply #'lcm (to-steps3 (data-to-network (remove nil (read-file "inputs/day9-test3" #'read-network)))))
  )


;; 
(defun day9-real2 ()
  (apply #'lcm (to-steps2 (data-to-network (remove nil (read-file "inputs/day9" #'read-network)))))
  )

