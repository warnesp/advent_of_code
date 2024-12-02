;; Advent of Code 2024 day 1

(in-package :advt2024-d1)

(defvar *part1* "inputs/day01-part1")
(defvar *part1-test* "inputs/day01-part1-test")
(defvar *part2-test* "inputs/day01-part2-test")


(defun p1-process-line (line)
  (let ((parts (str:words line)))
        (list (parse-integer (car parts)) (parse-integer (cadr parts)))))

(defun run-p1 (file)
  (destructuring-bind (l1 l2)  (transpose (read-file file #'p1-process-line))
    (let ((l1s (sort l1 #'<)) (l2s (sort l2 #'<)))
          (loop for x in l1s
                for y in l2s
                sum (abs (- y x))
          ))))

(defun run-p1-test ()
  (eq 11 (run-p1 *part1-test*)))

(defun run-p1-real ()
  "2367773"
  (run-p1 *part1*))

(defun list-to-counts (lst)
  "loops over a list counting the occurances of each item, runs in n time"
  (let ((results (make-hash-table)))
    (loop for x in lst
          do (setf (gethash x results)
                   ; increment the count, or initially set it if missing
                   (1+ (or (gethash x results) 0))))
             
    results))

(defun p2-alt-process-line (line h1 h2)
  "processes the line right into hash tables, seems to have a minor improvement"
  (let* ((parts (str:words line))
         (c1 (parse-integer (car parts)))
         (c2 (parse-integer (cadr parts))))
    (setf (gethash c1 h1) (1+ (or (gethash c1 h1) 0))) 
    (setf (gethash c2 h2) (1+ (or (gethash c2 h2) 0)))))



(defun run-p2 (file)
  "very basic approach, n^2 time"
  (destructuring-bind (l1 l2)  (transpose (read-file file #'p1-process-line))
    (let ((l1s (sort l1 #'<)) (l2s (sort l2 #'<)))
      (loop for x in l1s
            sum (* x (count x l2s))))))

(defun run-p2-alt (file)
  "better approach, runs in n time"
  (destructuring-bind (l1 l2)  (transpose (read-file file #'p1-process-line))
    (let ((l1-hash (list-to-counts l1)) (l2-hash (list-to-counts l2)))
      (loop for x being the hash-keys of l1-hash
            sum (* x (gethash x l1-hash) (or (gethash x l2-hash) 0))))))

(defun run-p2-alt2 (file)
  "read directly into hash tables to save some cons"
  (let ((h1 (make-hash-table)) (h2 (make-hash-table)))
    (read-file file (lambda (line) (p2-alt-process-line line h1 h2)))
    (loop for x being the hash-keys of h1
            sum (* x (gethash x h1) (or (gethash x h2) 0)))))

(defun run-p2-real ()
  "21271939"
  (run-p2-alt *part1*))

(defun run-p2-test ()
  (eq 31 (run-p2-alt *part2-test*)))
