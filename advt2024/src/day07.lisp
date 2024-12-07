;; Advent of Code 2024 day 7

(in-package :advt2024-d7)

(defvar *part1* "inputs/day07-part1")
(defvar *part1-test* "inputs/day07-part1-test")

(defstruct calibration result inputs)

(defun p1-process-line (line)
  (let ((parts (str:words line)))
    (make-calibration :result (parse-integer (car parts) :junk-allowed t)
                      :inputs (mapcar #'parse-integer (cdr parts)))))

(defun apply-opperators (c opps)
  (let ((accum (car (calibration-inputs c))))
  (loop for o in opps
        for v in (cdr (calibration-inputs c))
        until (> accum (calibration-result c))
        if (eql o 'ADD)
          do (setf accum (+ accum v))
        else if (eql o 'MUL)
          do (setf accum (* accum v))
        else
          do (setf accum (+ v (* accum (expt 10 (1+ (floor (log v 10)))))))
        finally (return accum)
        )))

(defun generate-operators (item-count)
  (labels ((g-rec (c results)
             (if (< c 1)
                 results
                 (g-rec (1- c) (loop for r in results
                                     collect (cons 'ADD r)
                                     collect (cons 'MUL r))))))
    (g-rec (1- item-count) '((ADD) (MUL)))))

(defun generate-ops-hash (c gen-ops)
  (let ((h (make-hash-table)))
    (dotimes (x c)
      (setf (gethash (+ 2 x) h) (funcall gen-ops (+ 1 x))))
    h))

(defun validate-calibration (c ops-h)
  (let ((r (calibration-result c))
        (ops (gethash (length (calibration-inputs c)) ops-h)))
    (loop for o in ops
          for v = (apply-opperators c o)
          when (= v r)
            return t)))

(defun run-p1 (file) 
  (let ((calibrations  (read-file file #'p1-process-line))
        (ops (generate-ops-hash 13 #'generate-operators)))
    (loop for c in calibrations
          when (validate-calibration c ops)
            sum (calibration-result c))))

(defun generate-operators-p2 (item-count)
  (labels ((g-rec (c results)
             (if (< c 1)
                 results
                 (g-rec (1- c) (loop for r in results
                                     collect (cons 'ADD r)
                                     collect (cons 'MUL r)
                                     collect (cons 'CAT r))))))
    (g-rec (1- item-count) '((ADD) (MUL) (CAT)))))

(defun run-p2 (file) 
  (let ((calibrations  (read-file file #'p1-process-line))
        (ops (generate-ops-hash 13 #'generate-operators-p2)))
    (loop for c in calibrations
          when (validate-calibration c ops)
            sum (calibration-result c))))

(defun run-p1-real ()
  "1985268524462"
  (run-p1 *part1*))

(defun run-p1-test ()
  "3749"
  (run-p1 *part1-test*))

(defun run-p2-real ()
  "150077710195188"
  (run-p2 *part1*))

(defun run-p2-test ()
  "11387"
  (run-p2 *part1-test*))
