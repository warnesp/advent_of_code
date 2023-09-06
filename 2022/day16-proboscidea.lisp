;; advent of code day 16 Proboscidea Volcanium


(defparameter *time* 30)
(defparameter *pos* 'aa)

(defparameter *nodes* nil)

(defstruct node name rate (enabled nil) tunnels (cost 1))

;;Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
;;Valve II has flow rate=20; tunnel lead to valve JJ

(defun restart-game ()
  (setf *time* 30)
  (setf *pos* 'aa)
  (setf *nodes* nil)
  )

(defun process-line (line)
  (let ((name (read-from-string (subseq line (length "Valve ") (+ 2 (length "Valve ")))))
        (rate (parse-integer (cadr (uiop:split-string line :separator "=;"))))
        (tunnels (mapcar #'read-from-string (remove "" (nthcdr 9 (uiop:split-string line :separator " ,")) :test #'string=)))
        )
    (cons name (make-node :name name :rate rate :tunnels tunnels))
    ))

(defun read-pv-file ()
  (with-open-file (in "input-day16" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (process-line line)))))

(defun should-enable (node)
  (not (or (node-enabled node) (= 0 (node-rate node))))
  )

(defun useless-node (n)
  (and (= 0 (node-cost n)) (= 1 (length (node-tunnels n)))))

(defun remove-node (n nodes)
  (loop for o in nodes
        do (setf (node-tunnels (cdr o)) (delete (node-name n) (node-tunnels (cdr o)))))
  (remove (node-name n) nodes :key #'car))

(defun replace-nodes (old new nodes)
  (loop for n in (mapcar #'cdr nodes)
        when (member (node-name old) (node-tunnels n))
          do (push (node-name new) (node-tunnels n)))
  (remove-node old nodes)
  )

(defun find-empty-edge (nodes)
  (loop for n in (mapcar #'cdr nodes)
        when (and (= 1 (length (node-tunnels n)))
                  (= 0 (node-rate n)))
          return n))
;; removes a node if it has no rate and links to only one other node
(defun prune-empty-edges (nodes)
  (let ((empty (find-empty-edge nodes)))
    (cond 
      (empty (prune-empty-edges (remove-node empty nodes)))
      (t nodes))))

;; test if a node is useless (no flow and no branching)
(defun node-is-useless (n)
  (and (= 0 (node-rate n)) (= 2 (length (node-tunnels n)))))

(defun find-useless-adj (nodes n)
  (cond
    ((not n) nil)
    ((= 0 (length (node-tunnels n))) nil)
    ((node-is-useless (cdr (assoc (car (node-tunnels n)) nodes)))
     (cdr (assoc (car (node-tunnels n)) nodes)))
    ((= 1 (length (node-tunnels n))) nil)
    ((node-is-useless (cdr (assoc (cadr (node-tunnels n)) nodes)))
     (cdr (assoc (cadr (node-tunnels n)) nodes)))
    (t (princ "fall through") nil)
    ))

(defun find-empty-pair-rec (nodes to-search)
  (let ((n1 (car to-search)) (n2 (find-useless-adj nodes (car to-search))))
    (cond
      ;; no nodes in list
      ((not n1) nil)
      ;; to few nodes in list
      ((< (length to-search) 2) nil)
      ((and n2 (node-is-useless n1))
       ;; two useless nodes that are adj
       (list n1 n2))
      ;; otherwise, keep looking
      (t (find-empty-pair-rec nodes (cdr to-search)))
      ))
  )

(defun find-empty-pair (nodes)
  (find-empty-pair-rec nodes
                       (remove-if (lambda (n) (> 0 (node-rate n)))
                                  (mapcar #'cdr nodes))))

(defun collapse-empty-rec (nodes results)
  (cond (nodes
         (let ((current (cadar nodes)))
           
           )
         )
        (t results))
  )

