;; advent of code day 11 monkey in the middle

(defstruct monkey id items operation test m-pass m-fail (inspections 0))
(defun parse-monkey-num (line)
  (parse-integer (subseq (remove #\: line) (length "Monkey "))))
(defun parse-starting-items (line)
  (let ((parts (uiop:split-string line :separator ":")))
    (mapcar #'parse-integer  (uiop:split-string (cadr parts) :separator ","))))
(defun parse-operation (line)
  (let* ((parts (uiop:split-string line :separator "="))
         (operands (uiop:split-string (string-trim " " (cadr parts)))))
    (lambda (old)
      (declare (special old))
      (eval (list (read-from-string (nth 1 operands))
                  (read-from-string (nth 0 operands))
                  (read-from-string (nth 2 operands)))))
    ))
(defun parse-test (line)
  (parse-integer (car (last (uiop:split-string line))))
  )
(defun parse-test-pass (line)
  (parse-integer (car (last (uiop:split-string line))))
  )
(defun parse-test-fail (line)
  (parse-integer (car (last (uiop:split-string line))))
  )
(defun monkey-factory (lines)
  (let* ((id (parse-monkey-num (nth 0 lines)))
         (items (parse-starting-items (nth 1 lines)))
         (oper (parse-operation (nth 2 lines)))
         (test (parse-test (nth 3 lines)))
         (m-pass (parse-test-pass (nth 4 lines)))
         (m-fail (parse-test-pass (nth 5 lines))))
    (make-monkey :id id :items items :operation oper :test test :m-pass m-pass :m-fail m-fail)
    ))


(defun read-monkey-file ()
  (with-open-file (in "input-day11" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line unless (equal "" line) collect
                                              (monkey-factory (list line
                                                                    (read-line in)
                                                                    (read-line in)
                                                                    (read-line in)
                                                                    (read-line in)
                                                                    (read-line in)
                                                                    ))))))
(defun monkeys-to-alist (monkeys)
  (loop for m in monkeys
        collect (cons (monkey-id m) m)
        ))

(defun manage-worry (worry)
  (floor (/ worry 3)))

(defun manage-worry-2 (worry)
  (mod worry 9699690))

(defun process-monkey (m amky mw)
  ;; monkey inspected this many items
  (incf (monkey-inspections m) (length (monkey-items m)))
  (loop for i in (monkey-items m) do
    (let* ((worry (funcall mw  (funcall (monkey-operation m) i)))
           (target (if (= 0 (mod worry (monkey-test m)))
                       (monkey-m-pass m)
                       (monkey-m-fail m)))
           (tmky (cdr (assoc target amky))))
      (setf (monkey-items tmky) (append (monkey-items tmky) (list worry)))
      ))
  (setf (monkey-items m) nil))

(defun do-monkey-game (rounds mw)
  (let*
      ((monkeys (read-monkey-file))
       (amky (monkeys-to-alist monkeys)))
    (dotimes (r rounds)
      (loop for m in monkeys
            do (process-monkey m amky mw))
      )
    monkeys))

(defun count-mky-business ()
  (let ((mkys (do-monkey-game 20 #'manage-worry)))
    (apply #'* (subseq (sort (mapcar #'monkey-inspections mkys) #'>) 0 2))
    ))
(defun count-mky-business-2 ()
  (let ((mkys (do-monkey-game 10000 #'manage-worry-2)))
    (apply #'* (subseq (sort (mapcar #'monkey-inspections mkys) #'>) 0 2))
    ))
