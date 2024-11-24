;; day 4
(defun to-range (str)
  (mapcar #'parse-integer (uiop:split-string str :separator "-")))

(defun to-ranges (str)
  (mapcar #'to-range (uiop:split-string str :separator ",")))

(defun check-overlap (ranges)
  (cond
    ((= (caar ranges) (caadr ranges)) t)
    ((< (caar ranges) (caadr ranges)) (>= (cadar ranges) (cadadr ranges)))
    (t (<= (cadar ranges) (cadadr ranges)))
    ))

(defun any-check-overlap (ranges)
  (cond
    ((= (caar ranges) (caadr ranges)) t)
    ((< (caar ranges) (caadr ranges)) (>= (cadar ranges) (caadr ranges)))
    (t (<= (caar ranges) (cadadr ranges)))
    ))
(defun read-camp-cleanup-file ()
  (let ((in (open "input-day04" :if-does-not-exist nil)) )
    (when in
      (let ((total
              (loop for line = (read-line in nil)
                    while line sum (if (check-overlap (to-ranges line)) 1 0))))
        (close in)
        total)
      )
    ))

(defun read-camp-cleanup-file-any ()
  (let ((in (open "input-day04" :if-does-not-exist nil)) )
    (when in
      (let ((total
              (loop for line = (read-line in nil)
                    while line sum (if (any-check-overlap (to-ranges line)) 1 0))))
        (close in)
        total)
      )
    ))
