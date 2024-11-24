;; Advent of Code Day 15

(load  "utilities.lisp")

(defun read-init-seq (line)
  (str:split "," line))

(defun calc-hash (stp)
 (reduce (lambda (p n) (rem (* 17 (+ p (char-code n))) 256)) stp :initial-value 0))

;; Part 1

(defun day15-part1 (file)
  (apply #'+ (mapcar #'calc-hash (car (read-file file #'read-init-seq)) ))
  )

;; 1320
(defun day15-test1 ()
  (day15-part1 "inputs/day15-test1"))

;; 522547
(defun day15-real1 ()
  (day15-part1 "inputs/day15"))

;; Part 2

(defstruct init-val name box operation)

(defun parse-init-seq (seq)
  (let* ((p-r (position #\- seq))
        (p-e (position #\= seq))
        (pos (or p-r p-e))
        (name (subseq seq 0 pos)))
    (make-init-val :name name
                   :box (calc-hash name)
                   :operation (parse-integer (subseq seq (1+ pos)) :junk-allowed t))
    ))

(defun crunch-seqs (seqs)
  (reverse
    (let ((found nil))
      (loop for s in (reverse seqs)
            for p = (find (init-val-name s) found :test #'string=)
            unless (or p (init-val-operation s)) 
            do (push (init-val-name s) found)
            unless (or p (not (init-val-operation s)))
            collect s)
      )))

(defun add-to-bucket (seq buckets)
  (let ((items (assoc (init-val-box seq) buckets)))
    (unless (loop named foo for s in (cdr items)
                  when (string= (init-val-name s) (init-val-name seq))
                  do (progn 
                       (setf  (init-val-operation s) (init-val-operation seq))
                       (return-from foo t))
                  )
      (push seq (cdr items)))
    ))

(defun bucketize-seqs (seqs)
  (let* ((buckets (remove-duplicates (mapcar #'init-val-box seqs)))
        (alst (reduce (lambda (p n) (acons n nil p)) buckets :initial-value nil)))
    (loop for s in seqs do (add-to-bucket s alst))
    alst
    ))

;; box is (Number . (list of items))
(defun sum-box (box)
  (loop for seq in (reverse (cdr box)) 
        for i from 1 
        sum (* (1+ (car box)) i (init-val-operation seq))))

(defun sum-boxes (boxes)
  (apply #'+ (mapcar #'sum-box boxes)))

(defun day15-part2 (file)
  (let ((seqs (mapcar #'parse-init-seq (car (read-file file #'read-init-seq)))))
    (sum-boxes (bucketize-seqs (crunch-seqs seqs)))))


;; 145
(defun day15-test2 ()
  (day15-part2 "inputs/day15-test1"))
;; 229271
(defun day15-real2 ()
  (day15-part2 "inputs/day15"))

