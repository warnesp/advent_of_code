
;; day 3

(defun get-priority (c)
  (let ((cc (char-code c)))
    (cond
      ((>= cc 97) (1+ (- cc 97)))
      (t (+ (- cc 65) 27)
         ))))

(defun process-rucksack (r)
  (let ((lst (coerce r 'list)))
    (intersection (subseq lst 0 (/ (length lst ) 2))
                 (nthcdr (/ (length lst) 2) lst))
    ))

(defun read-rucksack-file ()
  (let ((in (open "input-day03" :if-does-not-exist nil)) )
    (when in
      (let ((total
              (loop for line = (read-line in nil)
                    while line sum 
                               (get-priority (car (process-rucksack line)))
                    )))
        (close in)
        total)
      )
    ))

(defun find-badge (r1 r2 r3)
  (car (intersection (coerce r1 'list) (intersection (coerce r2 'list) (coerce r3 'list)))))



(defun read-rucksack-file-for-badges ()
  (let ((in (open "input-day03" :if-does-not-exist nil)) (group '()))
    (when in
      (let ((total
              (loop for line = (read-line in nil)
                    while line sum 
                               (if (equal (length group) 2)
                                   (let ((result (get-priority (find-badge (car group) (cadr group) line))))
                                     (setf group '())
                                     result)

                                   (progn (push line group)
                                          0)
                                   ))))
        (close in)
        total)
      )
    ))
