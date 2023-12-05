;; 2023 Advent of Code Day 4

(load  "utilities.lisp")
(ql:quickload "cl-ppcre")

(defun create-card (num winning have)
  (list num winning have))

(defun parse-nums (nums)
  (loop for x from 0 below (length nums) by 3
        collect (parse-integer (subseq nums x) :junk-allowed t)))

(defun read-scratchcard (line)
  (let* ((s1 (cl-ppcre:split ":" line))
         (s2 (cl-ppcre:split #\| (cadr s1))))
  (create-card (parse-integer (subseq (car s1) 5) :junk-allowed t)
               (parse-nums (subseq (car s2) 1))
               (parse-nums (subseq (cadr s2) 1)))
    ))
(defun count-winners (card)
  (let ((winners (sort (cadr card) #'<))
        (have (sort (caddr card) #'<)))
    (loop for w in winners
          when (find w have)
            sum 1)
    ))

(defun calc-points (card)
  (let ((winners (count-winners card)))
    (if (= 0 winners) 0
        (expt 2 (- winners 1))
    )))

(defun sum-points (cards)
  (apply #'+ (mapcar #'calc-points cards)))
;; 13
(defun day4-test1 ()
  (sum-points (read-file "inputs/day4-test1" #'read-scratchcard)))

;;27059
(defun day4-real1 ()
  (sum-points (read-file "inputs/day4" #'read-scratchcard)))


;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;

(defun update-dupes (dupes winners mult)
  (let ((new-dupes (append
                    (loop for d in dupes
                          repeat winners
                          collect (+ d mult))
                    (subseq dupes (min (length dupes) winners))
                    (loop repeat (- winners (length dupes))
                          collect mult))))
    (if new-dupes new-dupes '(0))))

(defun count-cards (cards dupes total)
  (if cards
      (let* ((winners (count-winners (car cards)))
             (mult (1+ (car dupes)))
             (new-dupes (update-dupes (cdr dupes) winners mult))
             )
        (count-cards (cdr cards) new-dupes (+ total mult))
        
        )
      total))
;; 30
(defun day4-test2 ()
   (count-cards (read-file "inputs/day4-test1" #'read-scratchcard) '(0) 0))
;; 5744979
(defun day4-real2 ()
  (count-cards (read-file "inputs/day4" #'read-scratchcard) '(0) 0))
