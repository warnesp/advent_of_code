;; Advent of Code 2024 day 9

(in-package :advt2024-d9)

(defvar *part1* "inputs/day09-part1")
(defvar *part1-test* "inputs/day09-part1-test")
(defvar *part2-test* "inputs/day09-part2-test")


(defun p1-process-line (line)
  (map 'vector (lambda (c) (- (char-code c) 48)) line))



(defun run-p1 (file) 
  (let* ((disk (car (read-file file #'p1-process-line)) )
         (begin 0)
         (last-count 0)
         (end (1- (length disk))))
    (loop
      until (= begin end)
      for counter from 0
      for begin-block-id = (/ begin 2)
      when (= counter (+ last-count (aref disk begin)))
        do (progn 
             (setf last-count counter)
             (incf begin))
      if (evenp begin)
        sum (* counter begin-block-id)
      else
        sum (loop
              with empty = (aref disk begin)
              for end-block-id from end downto 0)
      )
    disk))

(defun p2-process-line (line)
  )

(defun run-p2 (file) 
  )

(defun run-p1-real ()
  (run-p1 *part1*))

(defun run-p1-test ()
  (run-p1 *part1-test*))

(defun run-p2-real ()
  (run-p2 *part1*))

(defun run-p2-test ()
  (run-p2 *part2-test*))
