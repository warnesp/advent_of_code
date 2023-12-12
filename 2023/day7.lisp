;; 2023 Advent of Code Day 7

(load  "utilities.lisp")

(defstruct hand cards bid)

(defun letter-to-value (c)
  (case c
    (#\2 1)
    (#\3 2)
    (#\4 3)
    (#\5 4)
    (#\6 5)
    (#\7 6)
    (#\8 7)
    (#\9 8)
    (#\T 9)
    (#\J 10)
    (#\Q 11)
    (#\K 12)
    (#\A 13)
    ))

(defun read-hands (line)
  (let ((parts (str:split #\Space line)))
    (make-hand :cards (mapcar #'letter-to-value (coerce (car parts) 'list))
               :bid (parse-integer (cadr parts) :junk-allowed t))
    ))

(defun count-occurs (cards value)
  (loop for v in cards
        when (eql v value)
          sum 1))

(defun is-5-of-kind (cards)
  (= 5 (count-occurs cards (car cards))))

(defun is-4-of-kind (cards)
  (or (= 4 (count-occurs cards (car cards))) (= 4 (count-occurs cards (cadr cards)))))

(defun is-full-house (cards)
  (let ((uniq (remove-duplicates cards)))
    (and
     (=  2 (length uniq))
     (or
      (and (= 3 (count-occurs cards (car uniq)))
           (= 2 (count-occurs cards (cadr uniq))))
      (and (= 2 (count-occurs cards (car uniq)))
           (= 3 (count-occurs cards (cadr uniq))))
      ))))

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
(defun is-2-of-kind (cards)
  (= 4 (length (remove-duplicates cards))))


(defun rank-hand-base (hand)
  (loop for c in (hand-cards hand)
        for x from 4 downto 0 
        sum (* (expt 14 x) c)))

(defun rank-hand (hand)
  (+ (rank-hand-base hand)
   (cond
    ((is-5-of-kind (hand-cards hand)) (* 6 (expt 14 6)))
    ((is-4-of-kind (hand-cards hand)) (* 5 (expt 14 6)))
    ((is-full-house (hand-cards hand)) (* 4 (expt 14 6)))
    ((is-3-of-kind (hand-cards hand)) (* 3 (expt 14 6)))
    ((is-two-pair (hand-cards hand)) (* 2 (expt 14 6)))
    ((is-2-of-kind (hand-cards hand)) (* 1 (expt 14 6)))
    (t 0)
    )))



(defun compare-hands (h1 h2)
  (< (rank-hand h1) (rank-hand h2)))

(defun sum-bids (hands)
  (loop for h in hands
        for x from 1
        sum (* x (hand-bid h))))

;; 6440
(defun day7-test1 ()
  (sum-bids (sort (read-file "inputs/day7-test1" #'read-hands) #'compare-hands))
  )
;; 6592
(defun day7-test2 ()
  (sum-bids (sort (read-file "inputs/day7-test2" #'read-hands) #'compare-hands))
  )

;; 252857847 too high
;; 252656917
(defun day7-real1 ()
  (sum-bids (sort (read-file "inputs/day7" #'read-hands) #'compare-hands))
  )

;; Part 2
(defun letter-to-value2 (c)
  (case c
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    (#\T 10)
    (#\J 1)
    (#\Q 11)
    (#\K 12)
    (#\A 13)
    ))


(defun is-5-of-kind2 (cards)
  (or
   (= 5 (count-occurs cards (car cards)))
   (let ((no-jack (remove 1 cards)))
     (= (length no-jack) (count-occurs no-jack (car no-jack)))
     )
   ))

(defun is-4-of-kind2 (cards)
  (or
   (= 4 (count-occurs cards (car cards)))
   (= 4 (count-occurs cards (cadr cards)))
   (let ((no-jack (remove 1 cards)))
     (or
      (= (1- (length no-jack)) (count-occurs no-jack (car no-jack)))
      (= (1- (length no-jack)) (count-occurs no-jack (cadr no-jack))))
     )
   ))
(defun is-full-house2 (cards)
  (let ((uniq (remove-duplicates cards)))
    (or
     (and (= 2 (length uniq))
          (or
           (and
            (= 2 (count (car uniq) cards))
            (= 3 (count (cadr uniq) cards)))
           (and
           (= 3 (count (car uniq) cards))
           (= 2 (count (cadr uniq) cards))))
          (= 0 (count 1 cards)))
     (and (= 3 (length uniq))
          (and
           (= 2 (count (car (remove 1 uniq)) cards))
           (= 2 (count (cadr (remove 1 uniq)) cards)))
          (= 1 (count 1 cards)))
     
     )))

(defun is-3-of-kind2 (cards)
  (let ((uniq (remove-duplicates cards)))
    (or
     (and (= 3 (length uniq))
          (or
           (= 3 (count (car uniq) cards))
           (= 3 (count (cadr uniq) cards))
           (= 3 (count (caddr uniq) cards))
              ))
     (and (= 4 (length uniq))
          (= 1 (count 1 cards)))
     (= 2 (count 1 cards))
     )))

(defun is-two-pair2  (cards)
  (let ((uniq (remove-duplicates cards)))
    (or (and (= 0 (count 1 cards))
             (= 3 (length uniq)))
        (and (= 1 (count 1 cards))
             (= 5 (length uniq)))
        )
    ))

(defun is-2-of-kind2 (cards)
  (or
   (= 4 (length (remove-duplicates cards)))
   (= 1 (count 1 cards))
   ))
(defun rank-hand2 (hand)
  (+ (rank-hand-base hand)
     (cond
       ((is-5-of-kind2 (hand-cards hand)) (* 6 (expt 14 6)))
       ((is-4-of-kind2 (hand-cards hand)) (* 5 (expt 14 6)))
       ((is-full-house2 (hand-cards hand)) (* 4 (expt 14 6)))
       ((is-3-of-kind2 (hand-cards hand)) (* 3 (expt 14 6)))
       ((is-two-pair (hand-cards hand)) (* 2 (expt 14 6)))
       ((is-2-of-kind2 (hand-cards hand)) (* 1 (expt 14 6)))
       (t 0)
       )))

(defun read-hands2 (line)
  (let ((parts (str:split #\Space line)))
    (make-hand :cards (mapcar #'letter-to-value2 (coerce (car parts) 'list))
               :bid (parse-integer (cadr parts) :junk-allowed t))
    ))

(defun compare-hands2 (h1 h2)
  (< (rank-hand2 h1) (rank-hand2 h2)))

;;5905 
(defun day7-test3 ()
  (sum-bids (sort (read-file "inputs/day7-test1" #'read-hands2) #'compare-hands2))
  )
;; 6839
(defun day7-test4 ()
  (sum-bids (sort (read-file "inputs/day7-test2" #'read-hands2) #'compare-hands2))
  )

;; 253499763
(defun day7-real2 ()
  (sum-bids (sort (read-file "inputs/day7" #'read-hands2) #'compare-hands2))
  )
