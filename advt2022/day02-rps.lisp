
;; day 2
(defun rps-judge (elf us)
  (cond
    ((and (equal elf "A") (equal us "X")) 3)
    ((and (equal elf "A") (equal us "Y")) 6)
    ((and (equal elf "A") (equal us "Z")) 0)
    ((and (equal elf "B") (equal us "X")) 0)
    ((and (equal elf "B") (equal us "Y")) 3)
    ((and (equal elf "B") (equal us "Z")) 6)
    ((and (equal elf "C") (equal us "X")) 6)
    ((and (equal elf "C") (equal us "Y")) 0)
    ((and (equal elf "C") (equal us "Z")) 3)
    ))

(defstruct game-move)
(defstruct (rock (:include game-move)) (score 1))
(defstruct (paper (:include game-move)) (score 2))
(defstruct (sissors (:include game-move)) (score 3))

(defmethod beats (m))
(defmethod beats ((m rock)) (make-sissors))
(defmethod beats ((m paper)) (make-rock))
(defmethod beats ((m sissors)) (make-paper))
(defmethod loses (m))
(defmethod loses ((m rock)) (make-paper))
(defmethod loses ((m paper)) (make-sissors))
(defmethod loses ((m sissors)) (make-rock))

                                        ; x    y    z
                                        ;lose draw win
(defun move-factory (m)
  (cond
    ((or (equal m #\A) (equal m #\X))
     (make-rock))
    ((or (equal m #\B) (equal m #\Y))
     (make-paper))
    ((or (equal m #\C) (equal m #\Z))
     (make-sissors))
    ))

(defun result-factory (m r)
  (cond
    ((equal r #\X) (beats m))
    ((equal r #\Y) m)
    ((equal r #\Z) (loses m))
    ))

(defun moves-factory (line)
  (let ((m (move-factory (char line 0))))
    (list m (result-factory m (char line 2)))
    ))

(defmethod get-score (m))
(defmethod compare (them us))

(defmethod get-score ((m rock))
  (rock-score m))
(defmethod get-score ((m paper))
  (paper-score m))
(defmethod get-score ((m sissors))
  (sissors-score m))

(defmethod compare ((them rock) (us rock)) 3)
(defmethod compare ((them rock) (us paper)) 6)
(defmethod compare ((them rock) (us sissors)) 0)
(defmethod compare ((them paper) (us rock)) 0)
(defmethod compare ((them paper) (us paper)) 3)
(defmethod compare ((them paper) (us sissors)) 6)
(defmethod compare ((them sissors) (us rock)) 6)
(defmethod compare ((them sissors) (us paper)) 0)
(defmethod compare ((them sissors) (us sissors)) 3)



(defparameter *rps* '())

(defun reset-rps ()
  (setq *rps* '()))

(defun read-rps-file ()
  (let ((in (open "input-day02" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do
              (push (moves-factory line) *rps*)
            )
      (close in)
      )))

(defun sum-results ()
  (reduce #'+ (mapcar (lambda (x) (apply #'compare x)) *rps*)))
(defun sum-moves ()
  (reduce #'+ (mapcar (lambda (x) (get-score (cadr x))) *rps*)))
(defun rps-total ()
  (reset-rps)
  (read-rps-file)
  (+ (sum-results) (sum-moves)))
