;; day 6 tuning trouble

(defun process-packet-rec (line a b c d depth)
  (cond ((equal nil line) depth)
        ((= 4 (length (remove-duplicates (list a b c d)))) depth)
        (t (process-packet-rec (cdr line) b c d (car line) (1+ depth)))
        ))

(defun process-packet (line)
  (if (< (length line) 4)
      -1
      (process-packet-rec (cddddr line) (car line) (cadr line) (caddr line) (cadddr line) 4))
  )

(defun process-message-rec (line buffer depth)
  (cond ((equal nil line) depth)
        ((= 14 (length (remove-duplicates buffer))) depth)
        (t (process-message-rec (cdr line) (cons (car line) (subseq buffer 0 13)) (1+ depth)))
        ))

(defun process-message (line)
  (if (< (length line) 14)
      -1
      (process-message-rec (nthcdr 14 line) (subseq line 0 14) 14))
  )

(defun read-tuning-file ()
  (with-open-file (in "input-day06" :if-does-not-exist nil) 
    (when in
      (let ((line (coerce (read-line in nil) 'list)))
        (list
         (length line)
         (process-packet line)
         (process-message line)
         ))
      
      )))

