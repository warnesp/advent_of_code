;; Advent of Code 2024 day 4

(in-package :advt2024-d4)

(defvar *part1* "inputs/day04-part1")
(defvar *part1-test* "inputs/day04-part1-test")


(defun p1-process-line (line)
  (to-symbols line 'advt2024-d4))

(defun found-word-h (word data i j)
  "checks for a word existing from the point horizontally to the right"
  (loop for j2 from j 
        for w in word
        when (not (eql w (aref data i j2)))
          return nil
        finally (return t)))

(defun found-word-v (word data i j)
  "checks for a word existing from the point vertically down"
  (loop for i2 from i 
        for w in word
        when (not (eql w (aref data i2 j)))
          return nil
        finally (return t)))

(defun found-word-d-l (word data i j)
  "checks for a word existsing from the point diagonally to the left and down"
  (destructuring-bind (n m) (array-dimensions data)
    (declare (ignorable n))
    
    (and (>= (- i (length word)) -1)
         (>= m (+ j  (length word)))
         (loop for i2 from i downto 0
               for j2 from j
               for w in word
               when (not (eql w (aref data i2 j2)))
                 return nil
               finally  (return t)))))

(defun found-word-d-r (word data i j)
  "checks for a word existing from the point diagonally to the right and down"
  (destructuring-bind (n m) (array-dimensions data)
    (and (>= n (+ i (length word)))
         (>= m (+ j  (length word)))
         (loop for i2 from i
               for j2 from j
               for w in word
               when (not (eql w (aref data i2 j2)))
                 return nil
               finally  (return t)))
    ))

(defun count-word-h (data word)
  "Counts horizontal matches of the word"
  (let ((word-r (reverse word))
        (word-l (length word)))
    (destructuring-bind (n m) (array-dimensions data)
      (loop for i from 0 below n 
            sum (loop for j from 0 upto (- m word-l)
                      count (found-word-h word data i j)
                      count (found-word-h word-r data i j))))))

(defun count-word-v (data word)
  "Counts vertical matches of the word"
  (let ((word-r (reverse word))
        (word-l (length word)))
    (destructuring-bind (n m) (array-dimensions data)
      (loop for j from 0 below m 
            sum (loop for i from 0 upto (- n word-l)
                      count (found-word-v word data i j)
                      count (found-word-v word-r data i j))))))

(defun count-word-d (data word)
  "Counts diagonal matches of the word"
  (let ((word-r (reverse word)))
    (destructuring-bind (n m) (array-dimensions data)
      (loop for i from 0 below n
            sum (loop for j from 0 below m
                      count (found-word-d-l word data i j)
                      count (found-word-d-l word-r data i j)
                      count (found-word-d-r word data i j)
                      count (found-word-d-r word-r data i j)
                      )))))


(defun run-p1 (file)
  "cares about the word xmas in any direction"
  (let ((word '(X M A S))
        (data (list-to-2d-array (read-file file #'p1-process-line))))
    (+
     (count-word-v data word)
     (count-word-h data word)
     (count-word-d data word))))


(defun run-p2 (file) 
  "cares about an x of mas crossed with mas"
  (let ((word '(M A S))
        (word-r '(S A M))
        (data (list-to-2d-array (read-file file #'p1-process-line))))
    (destructuring-bind (n m) (array-dimensions data)
      (loop for i from 0 below (- n 2)
            sum (loop for j from 0 below (- m 2)
                      count (and (found-word-d-r word data i j)
                                 (found-word-d-l word data (+ i 2) j))
                      count (and (found-word-d-r word-r data i j)
                                 (found-word-d-l word data (+ i 2) j))
                      count (and (found-word-d-r word data i j)
                                 (found-word-d-l word-r data (+ i 2) j))
                      count (and (found-word-d-r word-r data i j)
                                 (found-word-d-l word-r data (+ i 2) j))
                        )))))

(defun run-p1-real ()
  (run-p1 *part1*))

(defun run-p1-test ()
  "18"
  (run-p1 *part1-test*))

(defun run-p2-real ()
  (run-p2 *part1*))

(defun run-p2-test ()
  "9"
  (run-p2 *part1-test*))
