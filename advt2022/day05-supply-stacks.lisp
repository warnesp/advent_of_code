;; day 5

;;[C]         [S] [H]                
;;[F] [B]     [C] [S]     [W]        
;;[B] [W]     [W] [M] [S] [B]        
;;[L] [H] [G] [L] [P] [F] [Q]        
;;[D] [P] [J] [F] [T] [G] [M] [T]    
;;[P] [G] [B] [N] [L] [W] [P] [W] [R]
;;[Z] [V] [W] [J] [J] [C] [T] [S] [C]
;;[S] [N] [F] [G] [W] [B] [H] [F] [N]
;; 1   2   3   4   5   6   7   8   9
;; 1   5   9   13 17  21   25  29 33  (1+ (* 4 (1- n)))
;; blank line
;; instr

(defparameter *stacks* nil)
(defparameter *moves* nil)

;; zero based
(defun stack-string-idx (i)
  (1+ (* 4 i)))

;; read in inital config
(defun process-stacks (accum)
  ;; process header
  (let* ((stack-nums (mapcar #'parse-integer (remove-if #'uiop:emptyp (uiop:split-string (pop accum)))))
         (stack-count (reduce #'max stack-nums))
         (result (make-array stack-count :element-type 'list)))
    (loop for i from 0 below (length result) do (setf (aref result i) nil))
    ;; loop and process rows
    (loop for line in accum do
      (loop for slot from 0 below (length result) do
           (let ((box (char line (stack-string-idx slot))))
             (unless (equal #\  box)
               (push box (aref result slot)))
             ))
          )
    result))

;; read in moves
;; move 15 from 8 to 9
;; (15 8 9)
(defun line-to-moves (line)
  (let ((subs (uiop:split-string line :separator " ")))
    (list (parse-integer (cadr subs)) (parse-integer (nth 3 subs)) (parse-integer (nth 5 subs))
          )))

;; process moves
(defun process-move (move stacks)
  (dotimes (number (car move))
    (let ((crate (pop (aref stacks (1- (cadr move))))))
      (when crate
        (push crate (aref stacks (1- (caddr move))))))))

(defun process-move-2 (move stacks)
  (let ((number (car move)))
    (setf (aref stacks (1- (caddr move)))
          (append (subseq (aref stacks (1- (cadr move))) 0 number) (aref stacks (1- (caddr move)))))
    (setf (aref stacks (1- (cadr move))) (nthcdr number (aref stacks (1- (cadr move)))))
    ))

(defun process-moves-strat (moves stacks proc)
  (loop for move in (nreverse moves) do
    (fresh-line)
    (princ (car move))
    (funcall proc move stacks)))

(defun process-moves (moves stacks)
  (process-moves-strat moves stacks #'process-move))

(defun process-moves-2 (moves stacks)
  (process-moves-strat moves stacks #'process-move-2))

;; output result

(defun read-supply-stacks-file ()
  (setq *stacks* nil)
  (setq *moves* nil)
  (let ((in (open "input-day05" :if-does-not-exist nil)) (stack-accum nil))
    (when in
      (loop for line = (read-line in nil)
            while line do
              (cond
                ((equal line "") (setf *stacks* (process-stacks stack-accum)))
                ((eq *stacks* nil) (push line stack-accum))
                (t (push (line-to-moves line) *moves*)))
            ))
    (close in)
    ))

(defun print-results (stacks)
  (loop for i from 0 below (length stacks) do
    (fresh-line)
    (princ i)
    (princ ": ")
    (if (equal nil (aref stacks i))
        (princ "empty")
        (princ (car (aref stacks i))))
        ))
