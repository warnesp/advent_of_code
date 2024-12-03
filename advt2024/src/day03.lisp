;; Advent of Code 2024 day 3

(in-package :advt2024-d3)

(defvar *part1* "inputs/day03-part1")
(defvar *part1-test* "inputs/day03-part1-test")
(defvar *part2-test* "inputs/day03-part2-test")

(defun p1-mult (str)
  "pulls out numbers and multiplies them, assumes already filtered by size"
  (let ((vals (ppcre:all-matches-as-strings "\\d+" str)))
    (apply #'* (or (mapcar #'parse-integer vals) '(0)))))

(defun p1-process-line (line)
  "look for mul, do the mul, and sum"
  (let ((ptrn "mul\\(\\d?\\d?\\d,\\d?\\d?\\d\\)"))
    (apply #'+ (mapcar #'p1-mult (ppcre:all-matches-as-strings ptrn line)))))

(defun run-p1 (file) 
  (let ((data (read-file file #'p1-process-line)))
    (apply #'+ data)))

(defun p2-process-line (line)
  "looks for mul, do, and don't"
  (let ((ptrn "(mul\\(\\d?\\d?\\d,\\d?\\d?\\d\\))|(do\\(\\))|(don't\\(\\))"))
    (ppcre:all-matches-as-strings ptrn line)))

(defun p2-filter (data)
  "expects list containing the string tokens (mul, do, don't) from the file"
  (let ((process t))
    (loop for x in data
          when (string= "don't()" x)
            do (setf process nil)
          when (string= "do()" x)
            do (setf process t)
          when process
            sum (p1-mult x))))

(defun run-p2 (file) 
  (let ((data (read-file file #'p2-process-line)))
    ;; treat the input as one line to make processing the do's and don't's easier
    (p2-filter (flatten data))))

(defun run-p1-real ()
  "166630675"
  (run-p1 *part1*))

(defun run-p1-test ()
  "161"
  (run-p1 *part1-test*))

(defun run-p2-real ()
  "93465710"
  (run-p2 *part1*))

(defun run-p2-test ()
  "48"
  (run-p2 *part2-test*))
