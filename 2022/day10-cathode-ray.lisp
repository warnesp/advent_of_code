;; advent of code day 10 cathode ray tube

;; addx #
;; noop

(defparameter *X* 1)
(defparameter *clock* 1)

(defparameter *pc* nil)

(defparameter *cycle-sum* 0)

(defun draw-pixel (clock x)
  (if (<= (abs (- clock x)) 1)
      (princ "#")
      (princ "."))
  )

(defun reset-comp ()
  (setf *clock* 0)
  (setf *X* 1)
  (setf *cycle-sum* 0))


(defun inc-clock ()
  (draw-pixel (mod *clock* 40) *X*)
  (incf *clock*)
  (when (= (mod *clock* 40) 0) (fresh-line))
  (when (= 0 (mod (- *clock* 20) 40))
    (incf *cycle-sum* (* *X* *clock*)))
  )

(defun add (v)
  (inc-clock)
  (incf *X* v))

(defun noop ()
  (inc-clock))

(defun process-line (line)
  (let* ((part (uiop:split-string line))
         (instruction (read-from-string (car part))))
    (cons instruction (when (cdr part) (list (parse-integer (cadr part)))))
    ))

(defun read-cathode-file ()
  (with-open-file (in "input-day10" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line collect
                       (process-line line)))))

(defun run-cathode-prog ()
  (reset-comp)
  (let ((pc (read-cathode-file)))
    (loop for instruction in pc
          do
             (case (car instruction)
               (noop (noop))
               (addx (noop) (add (cadr instruction)))
               )
          )
    ))
