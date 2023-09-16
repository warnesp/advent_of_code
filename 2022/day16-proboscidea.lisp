;; advent of code day 16 Proboscidea Volcanium


(defparameter *time* 30)
(defparameter *pos* 'aa)


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
  (with-open-file (in "inputs/input-day16" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (process-line line)))))
(defun read-pv-file-test ()
  (with-open-file (in "inputs/input-day16-test" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line
            collect (process-line line)))))
(defparameter *nodes* (read-pv-file))
(defparameter *nodes-test* (read-pv-file-test))
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

(defun find-empty-pair (nodes)
  (labels ((find-empty-pair-rec (nodes to-search)
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
                 ))))
    (find-empty-pair-rec nodes
                         (remove-if (lambda (n) (> 0 (node-rate n)))
                                    (mapcar #'cdr nodes)))))

;; cur is a node, returns a node or nil
(defun nearest-neighbor (nodes cur)
  (loop for neighbor
          in (node-tunnels cur)
        when (assoc neighbor nodes)
          return (cdr (assoc neighbor nodes))))


;; source is a symbol
(defun dijk-search (nodes source target)
  (let ((dist (make-hash-table :size (* 2 (length nodes))))
        (prev (make-hash-table :size (* 2 (length nodes)))))
    ;; initialize to node with very large size (infinity placeholder)
    ;; initialize prev to nil
    (loop for n in nodes
          do (setf (gethash (car n) dist) 40000000)
             (setf (gethash (car n) prev) nil))
    (setf (gethash source dist) 0)

    (labels ((next-smallest (dist visited)
               (let ((result nil) (last-min 40000000))
                 (maphash (lambda (key value)
                            (when (and (< value last-min)
                                       (not (member key visited)))
                              (setf last-min value)
                              (setf result key))
                            ) dist)
                 result)))
      (let ((visited nil))
        (loop while (/= (length nodes) (length visited) )
              for next = (next-smallest dist visited)
              when (eq next target)
                return nil
              do (push next visited)

                 (loop for v in (set-difference (node-tunnels (cdr (assoc next nodes))) visited)
                       do (let ((alt (+ 1 (gethash next dist))))
                            (when (< alt (gethash v dist))
                              (setf (gethash v dist) alt)
                              (setf (gethash v prev) next)
                              ))))
        ))
    (gethash target dist)))

(defun get-node2node-with-value-pairs (nodes)
  (loop for n-rest on nodes
        for node = (cdar n-rest)
        for n = (caar n-rest)
        when (and (> (node-rate node) 0) (> (length n-rest) 1))
          nconcing (loop for n2 in n-rest
                        when (and (not (eq n (car n2))) (> (node-rate (cdr n2)) 0))
                          collect (list n (car n2)))
        ))

;; get cost for each pair
(defun pair-cost (nodes)
  (loop for np in (get-node2node-with-value-pairs nodes) 
        for dv = (dijk-search nodes (car np) (cadr np))
        nconcing (list (list (car np) (cadr np) dv) (list (cadr np) (car np) dv))))

(defun get-rate (node nodes)
  (node-rate (cdr (assoc node nodes))))

(defun initial-travel-cost (start nodes)
  (loop for n in nodes
        for node = (car n)
        when (> (node-rate (cdr n)) 0)
          collect (list start node (dijk-search nodes start node))
          ))

(defun all-travel-cost (start nodes)
  (append (initial-travel-cost start nodes) (pair-cost nodes)))

;; backpack solving algorithm
(defun find-max-value (nodes)
  (labels ((find-max-value-rec (current-node time-left visited nodes travel-costs)
             (cond
               ((< time-left 0) 0)
                   (t (+
                       ;; the value of turning on the current node
                       (*   time-left (get-rate current-node nodes))
                       (loop for path in travel-costs
                         for ntime-left = (- time-left 1 (caddr path))
                         for next-visit = (if (> ntime-left 0) (cons current-node visited) visited)
                         for next-node = (if (> ntime-left 0) (cadr path) current-node)
                             when (and (eq current-node (car path)) (not (member (cadr path) visited)))
                               maximize (find-max-value-rec next-node ntime-left next-visit nodes travel-costs)
                             )
                       )))))
    (find-max-value-rec 'aa 30 nil nodes (all-travel-cost 'aa nodes))
    ))

;; 1651 - test answer

;; 1653 correct

                ;;(fresh-line)
                ;;(princ (length visited))
                ;;(loop for x from 0 to (length visited) do (princ " "))
                ;;(princ "El turn ")
                ;;(princ el-node)
                ;;(princ " ")
                ;;(princ el-time-left)
;; 26 mins, need to have 2 agents
(defun find-max-value2 (nodes)
  (labels ((valid-destination (node visited)
             (not (member node visited)))
           (fmv-rec (us-node el-node us-time-left el-time-left visited nodes travel-costs)
             (cond
               ;; both out of time, stop
               ((and (<= us-time-left 0) (<= el-time-left 0)) 0)
               ((>= us-time-left el-time-left)
                ;; we are next to move
                (+ (* us-time-left (get-rate us-node nodes))
                   ;; todo just pass the turn and not add to visited when time-left is <= 
                   (loop for path in travel-costs
                         for time-left = (- us-time-left 1 (caddr path))
                         for next-node = (cadr path) 
                         when (and (eq us-node (car path)) (valid-destination next-node visited))
                           maximize
                           (if (<= time-left 0)
                               (fmv-rec us-node el-node -1 el-time-left visited nodes travel-costs)
                               (fmv-rec next-node el-node time-left el-time-left (cons next-node visited) nodes travel-costs)
                               )

                         )))
               (t
                ;; elephant-node is next to move
                (+ (* el-time-left (get-rate el-node nodes))
                   (loop for path in travel-costs
                         for time-left = (- el-time-left 1 (caddr path))
                         for next-node = (cadr path) 
                         when (and (eq el-node (car path)) (valid-destination next-node visited))
                           maximize
                           (if (<= time-left 0)
                               (fmv-rec us-node el-node us-time-left -1 visited nodes travel-costs)
                               (fmv-rec us-node next-node us-time-left time-left (cons next-node visited) nodes travel-costs)
                               )

                         )))
               )))
    (fmv-rec 'aa 'aa 26 26 nil nodes (all-travel-cost 'aa nodes))
    ))

(defun find-max-value3 (nodes)
  (let ((map (make-hash-table :test #'equal)))
    (labels
        ;; doesn't take into acount where we are, so distances may be wrong and thus overestimate
        ((make-key (t1 t2 n1 n2 visited)
           (if (> t1 t2)
               (list t1 t2 n1 n2 (stable-sort (copy-list visited) #'string-lessp))
               (list t2 t1 n2 n1 (stable-sort (copy-list visited) #'string-lessp))))
         (fmv-rec (us-nodes el-nodes us-time-left el-time-left visited nodes travel-costs)
           (let ((key (make-key us-time-left el-time-left (car us-nodes) (car el-nodes) visited)))
             (cond
              ;; both out of time, stop
              ((and (<= us-time-left 0) (<= el-time-left 0)) 0)
              ((gethash key map) (gethash key map))
              (t 
               (let* ((travel-costs-n (remove-if
                                      (lambda (n) (member (cadr n) visited))
                                      travel-costs))
                      (result (loop for path in travel-costs-n
                             for time-left-u = (- us-time-left 1 (caddr path))
                             for time-left-e = (- el-time-left 1 (caddr path))
                             for next-node = (cadr path) 
                             for next-rate = (get-rate next-node nodes)
                             when (and (eq (car us-nodes) (car path)) (> time-left-u 0))
                               maximize
                               (+ (* time-left-u next-rate)
                                  (fmv-rec (cons next-node us-nodes) el-nodes time-left-u el-time-left
                                           (cons next-node visited) nodes travel-costs-n)
                                  ) into mu
                             when (and (eq (car el-nodes) (car path)) (> time-left-e 0))
                               maximize
                               (+ (* time-left-e next-rate)
                                  (fmv-rec us-nodes (cons next-node el-nodes) us-time-left time-left-e
                                           (cons next-node visited) nodes travel-costs-n)
                                  ) into me
                             finally (return (max mu me))
                             )))
                 (setf (gethash key map) result)
                 result
                 ))))
           
           ))
      "
      (let ((base-travel (all-travel-cost 'aa nodes)) (start '(aa)))
        (loop for p in base-travel
              for time-left = (- 26 1 (caddr p))
              when (eq 'aa (car p)) 
                maximize
                (+ (* time-left (get-rate (cadr p) nodes))
                   (fmv-rec (cons (cadr p) start) start time-left
                            26 (list (cadr p)) nodes (remove-if
                                                      (lambda (n) (eq (cadr n) (cadr p)))
                                                      base-travel)))
                
              )
        )"
      (fmv-rec '(aa) '(aa) 26 26 (list 'aa) nodes (all-travel-cost 'aa nodes))
      ;;(loop for key being the hash-keys of map do (fresh-line) (princ key) (princ " ") (princ (gethash key map)) maximize (gethash key map))
      
      )))
