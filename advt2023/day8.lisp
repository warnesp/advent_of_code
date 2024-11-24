;; 2023 Advent of Code Day 8

(load  "utilities.lisp")

(defstruct node start left right is-end is-start)

(defstruct network path nodes starts ends)

(defun nodes-to-hash (nodes)
  (let ((tbl (make-hash-table)))
    (loop for node in nodes
          do (setf (gethash (node-start node) tbl) node))
    tbl))

(defun data-to-network (lines)
  (make-network :path (car lines)
                :nodes (nodes-to-hash (cdr lines))
                :starts (mapcar #'node-start (remove-if-not #'node-is-start (cdr lines)))
                :ends (mapcar #'node-start (remove-if-not #'node-is-end (cdr lines)))))

(defun parse-path (line)
  (mapcar  (lambda (c) (if (eq c #\L) 'L 'R)) (coerce line 'list)))

(defun parse-node (line)
  (let* ((parts (str:split-omit-nulls #\= line))
         (is-start (str:ends-with? "A" (str:trim (car parts))))
         (is-end (str:ends-with? "Z" (str:trim (car parts))))
         (paths (read-from-string (str:trim (str:replace-all "," #\Space (cadr parts))))))
    (make-node :start (intern (str:trim (car parts)))
               :left (car paths)
               :right (cadr paths)
               :is-start is-start
               :is-end is-end)
    ))

(defun read-network (line)
  (cond
    ((find #\= line) (parse-node line))
    ((= 0 (length line)) nil)
    (t (parse-path line))
    ))

(defun get-next (node instr)
  (if (eq 'L instr)
      (node-left node)
      (node-right node)))

(defun to-steps (network)
  (labels ((pts-rec (network instr current cnt)
             (if (eq 'ZZZ current)
                 cnt
                 (pts-rec network (or (cdr instr) (network-path network)) (get-next (gethash current (network-nodes network)) (car instr)) (1+ cnt))
                 )))
    (pts-rec network (network-path network) 'AAA 0)
    ))

;;;;;;;;;;;;
;; Part 1 ;;
;;;;;;;;;;;;

;; 2
(defun day8-test1 ()
  (to-steps (data-to-network (remove nil (read-file "inputs/day8-test1" #'read-network)))) 
  )

;; 6
(defun day8-test2 ()
  (to-steps (data-to-network (remove nil (read-file "inputs/day8-test2" #'read-network)))) 
  )


;; 11567
(defun day8-real1 ()
  (to-steps (data-to-network (remove nil (read-file "inputs/day8" #'read-network)))) 
  )


;;;;;;;;;;;;
;; Part 2 ;;
;;;;;;;;;;;;

(defun is-at-end (ends currents)
  (not (member nil (loop for c in currents
        collect (member c ends)))))

(defun follow-paths (network currents instr)
  (loop for current in currents
        collect (get-next (gethash current (network-nodes network)) instr))
  
  )

(defun to-steps2 (network)
  (labels ((pts-rec (instr current cnt)
             (if (node-is-end current)
                 cnt
                 (pts-rec (or (cdr instr) (network-path network))
                          (gethash (get-next current (car instr)) (network-nodes network))
                          (1+ cnt))
                 )))
    (loop for start in (network-starts network)
          collect (pts-rec (network-path network) (gethash start (network-nodes network)) 0))
    
    ))


;; 6
(defun day8-test3 ()
  (apply #'lcm (to-steps2 (data-to-network (remove nil (read-file "inputs/day8-test3" #'read-network)))))
  )


;; 9858474970153
(defun day8-real2 ()
  (apply #'lcm (to-steps2 (data-to-network (remove nil (read-file "inputs/day8" #'read-network)))))
  )

