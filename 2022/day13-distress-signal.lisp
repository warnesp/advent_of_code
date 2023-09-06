;; advent of code day 13 distress signal

(defstruct signal-pair first second)

(defun parse-signal (line)
  (read-from-string (substitute #\( #\[ (substitute #\) #\] (substitute #\   #\, line)))))

(defun process-line (line1 line2 )
  (make-signal-pair :first (parse-signal line1)
                    :second (parse-signal line2)))

(defun read-distress-signal-file ()
  (with-open-file (in "input-day13" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            when (> (length line) 0)
              collect (process-line line (read-line in) )))))

(defun read-distress-signal-file-packets ()
  (with-open-file (in "input-day13" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            when (> (length line) 0)
              collect (parse-signal line )))))

(defun compare-signals (s1 s2)
  (cond
    ((and (listp s1) (listp s2))
     (let ((res (loop for is1 in s1
           for is2 in s2
           do
              ;; if both numbers, check
              (when (and (numberp is1) (numberp is2))
                (cond
                  ((< is1 is2) (return 'pass))
                  ((> is1 is2) (return 'fail))))
              (when (and (numberp is1) (listp is2))
                (case (compare-signals (list is1) is2)
                  (pass (return 'pass))
                  (fail (return 'fail))))
              (when (and (listp is1) (numberp is2))
                (case (compare-signals is1 (list is2))
                  (pass (return 'pass))
                  (fail (return 'fail))))
              (when (and (listp is1) (list is2))
                (case (compare-signals is1 is2)
                  (pass (return 'pass))
                  (fail (return 'fail))))
              
           )))
       (cond
         ((eq res 'pass) 'pass)
         ((eq res 'fail) 'fail)
         ((< (length s1) (length s2)) 'pass)
         ((> (length s1) (length s2)) 'fail)))
     )
    ((numberp s1) (compare-signals (list s1) s2))
    (t (compare-signals s1 (list s2)))
    ))

(defparameter *special-a* '((2)))
(defparameter *special-b* '((6)))

(defun process-signals (signal-pairs)
  (loop for pair in signal-pairs
        for i from 1
        when (eq 'pass (compare-signals (signal-pair-first pair) (signal-pair-second pair)))
          collect i
        ))

(defun packet< (p1 p2)
  (eq 'pass (compare-signals p1 p2)))

(defun sort-packets (packets)
  (sort packets #'packet<))

(defun add-special (packets)
  (cons *special-a* (cons *special-b* packets)))

(defun get-ordered-packets ()
  (sort-packets (add-special (read-distress-signal-file-packets))))

(defun get-special-loc (packets)
  (*
   (1+ (position *special-a* packets :test #'equal))
   (1+ (position *special-b* packets :test #'equal))
   ))
