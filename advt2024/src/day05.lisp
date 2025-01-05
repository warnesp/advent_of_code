;; Advent of Code 2024 day 5

(in-package :advt2024-d5)

(defvar *part1-rules* "inputs/day05-part1-rules")
(defvar *part1-pages* "inputs/day05-part1-pages")
(defvar *part1-test-rules* "inputs/day05-part1-test-rules")
(defvar *part1-test-pages* "inputs/day05-part1-test-pages")
(defvar *part2-test* "inputs/day05-part2-test")


(defun p1-process-rules (line)
  (mapcar #'parse-integer (uiop:split-string line :separator "|")))

(defun p1-process-pages (line)
  (mapcar #'parse-integer (uiop:split-string line :separator ",")))

(defun middle (pages)
  (nth (floor (length pages) 2) pages))

(defun check-rule-p (rule pages)
  (let ((p1 (position (car rule) pages))
        (p2 (position (cadr rule) pages)))
    (or (not p1) (not p2) (< p1 p2))))

(defun ordered-p (pages rules)
  (loop for r in rules
        unless (check-rule-p r pages)
          return nil
        finally
           (return t)))

(defun run-p1 (rules-file pages-file) 
  (let ((rules (read-file rules-file #'p1-process-rules))
        (pages (read-file pages-file #'p1-process-pages)))
    (loop for p in pages
          when (ordered-p p rules)
            sum (middle p)
          )))

(defun fix-pages (rules pages)
  (sort pages (lambda (p1 p2) (ordered-p (list p1 p2) rules)) ))

(defun run-p2 (rules-file pages-file) 
  (let ((rules (read-file rules-file #'p1-process-rules))
        (pages (read-file pages-file #'p1-process-pages)))
    (loop for p in pages
          unless (ordered-p p rules)
            sum (middle (fix-pages rules p))
          )))

(defun run-p1-real ()
  "4609"
  (run-p1 *part1-rules* *part1-pages*))

(defun run-p1-test ()
  "143"
  (run-p1 *part1-test-rules* *part1-test-pages*))

(defun run-p2-real ()
  "5723"
  (run-p2 *part1-rules* *part1-pages*))

(defun run-p2-test ()
  "123"
  (run-p2 *part1-test-rules* *part1-test-pages*))
