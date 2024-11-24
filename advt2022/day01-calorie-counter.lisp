;; advent of code day 1
;; elf calorie counter


(defparameter *elfs* '(0))

(defun reset-elfs ()
  (setq *elfs* (list 0)))

(defun read-elf-file (f)
  (let ((in (open f :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do
            (cond
              ((equal line "") (push 0 *elfs*))
              (t (incf (car *elfs*) (parse-integer line)))
              )
      )
      (close in)
      )))

(defun read-in-elfs ()
  (reset-elfs)
  (read-elf-file "input-day01")
  )

(defun find-high-elf ()
  (loop for e in *elfs*
        maximizing e))

(defun find-high-three ()
  (setq *elfs* (sort *elfs* #'>))
  (+ (car *elfs*) (cadr *elfs*) (caddr *elfs*)))

