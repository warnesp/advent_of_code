;; 2023 Advent of Code Day 7

(load  "utilities.lisp")
(ql:quickload "cl-ppcre")
(ql:quickload "str")

(defstruct hand cards bid)

(defun read-hands (line)
  line)
(defun count-occurs (cards value)
  (loop for v in cards
        when (eql v value)
          sum 1))

(defun is-5-of-kind (cards)
  (= 5 (count-occurs cards (car cards))))

(defun is-4-of-kind (cards)
  (or (= 4 (count-occurs cards (car cards))) (= 4 (count-occurs cards (cadr cards)))))

(defun is-full-house (cards)
  nil)

(defun is-3-of-kind (cards)
  (and (not (is-full-house cards))
       (or (= 3 (count-occurs cards (car cards)))
      (= 3 (count-occurs cards (cadr cards)))
      (= 3 (count-occurs cards (caddr cards))))))
  

(defun is-two-pair (cards)
  (let ((unique (remove-duplicates cards)))
    (and (= 3 (length unique))
         (or (and (= 2 (count-occurs cards (car unique)))
                  (= 2 (count-occurs cards (cadr unique))))
             (and (= 2 (count-occurs cards (car unique)))
                  (= 2 (count-occurs cards (caddr unique))))
             (and (= 2 (count-occurs cards (cadr unique)))
                  (= 2 (count-occurs cards (caddr unique))))
             )
       )))
(defun is-two-of-kind (cards)
  (= 4 (length (remove-duplicates cards))))

(defun rank-hand (hand)
  1)



(defun compare-hands (h1 h2)
  t)

(defun sum-bids (hands)
  (loop for h in hands
        for x from 1
        sum (* h (hand-bid h))))

;; 6440
(defun day7-test1 ()
  (read-file "inputs/day7-test1" #'read-hands)
  )

;; 
(defun day7-real1 ()
  (read-file "inputs/day7" #'read-hands)
  )

;; Part 2


