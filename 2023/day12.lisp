;; Advent of Code 2023 Day 12

(load  "utilities.lisp")


;; operational   .
;; damaged       #
;; unknown       ?
;; line = spring states followed by damaged groups
;; ..##......###..    2,3

(defstruct springs report groups)

(defun read-condition-record (line)
  (let* ((parts (str:split-omit-nulls " " line))
         (report (str:split-omit-nulls "." (car parts)))
         (groups (str:split-omit-nulls "," (cadr parts))))
    (make-springs :report (car parts) :groups (mapcar #'parse-integer groups))
    ))

;; one single spring report
(defun count-arrangements (report pos groups )
  (let ((group (car groups))
        (report-len (length report)))
    (if groups
        (loop for x from pos below report-len
              for c = (char report x)
              for end = (+ x group)
              when (and (not (equal c #\.)) 
                        (>= (- report-len x) group)
                        ;; todo does the group fit in the current spot
                        (= group
                           (loop for y from x below end
                                 when (or (equal (char report y) #\#)
                                          (equal (char report y) #\?))
                                 sum 1))

                        ;; either at the end 
                        ;; or less than the end and not followed by a #
                        (or (= end report-len)
                            (and (< end report-len)
                                 (not (equal (char report end) #\#))))
                        )  
              sum (count-arrangements report (+ x group 1) (cdr groups) )
              ;; stop when we find a #, no more matches for this group
              until (equal c #\#)
              )

        ;; TODO make sure there are not spots left, ie out of groups, but # remain
        (if (and
              (< pos report-len)
              (find #\# (subseq report pos))) 
            0 
            1)
        )))
(defun count-arrangements-dp (report pos groups tbl)
  (let ((group (car groups))
        (report-len (length report)))
    
    (let* ((pr (gethash (list pos groups) tbl)))
      (cond 
              (pr (progn (princ pr) (princ " ") pr))
              (groups (loop for x from pos below report-len
                      for c = (char report x)
                      for end = (+ x group)
                      for key = (list (1+ x) (cdr groups))
                      when (and (not (equal c #\.)) 
                                (>= (- report-len x) group)
                                ;; todo does the group fit in the current spot
                                (= group
                                   (loop for y from x below end
                                         when (or (equal (char report y) #\#)
                                                  (equal (char report y) #\?))
                                         sum 1))

                                ;; either at the end 
                                ;; or less than the end and not followed by a #
                                (or (= end report-len)
                                    (and (< end report-len)
                                         (not (equal (char report end) #\#))))
                                )  
                      sum (if (gethash key tbl)
                              (gethash key tbl)
                              (let ((r2 (count-arrangements report (+ x group 1) (cdr groups) )))
                                  (setf (gethash key tbl) r2)
                                  r2
                                  )
                              ) 
                      
                      ;; stop when we find a #, no more matches for this group
                      until (equal c #\#)
                      ))
              ((and
                      (< pos report-len)
                      (find #\# (subseq report pos))) 0)
              (t 1))
      )
    ))
(defun sum-arrangements (springs-list)
  (let ((ln (length springs-list)))
    (loop for sp in springs-list
          for x from 1
          do (progn
               (fresh-line)
               (princ x)
               (princ " of ")
               (princ ln)
               )
          sum (count-arrangements-dp (springs-report sp) 0 (springs-groups sp) 
                                     (make-hash-table :test #'equal)))
    )
  )

(defun col-arrangements (springs-list)
  (loop for sp in springs-list
        collect (count-arrangements (springs-report sp) 0 (springs-groups sp))))
;; Part 1

;; 1, 4, 1, 1, 4, 10
;; total: 21
(defun day12-test1 ()
  (sum-arrangements (read-file "inputs/day12-test1" #'read-condition-record)))

;; 1, 1, 1, 4, 1, 1, 4, 10
;; total: 23
(defun day12-test2 ()
  (sum-arrangements (read-file "inputs/day12-test2" #'read-condition-record)))

;; 8099 - to high
;; 7653
(defun day12-real1 ()
  (sum-arrangements (read-file "inputs/day12" #'read-condition-record)))

;; Part 2



(defun unfold-groups (groups)
  (loop for x from 1 to 5
        nconcing (copy-list groups)))

(defun unfold-report (report)
  (apply #'concatenate 'string 
         (cons report 
               (loop for x from 1 to 4
                     collect (concatenate 'string "?" report) )))
  )

(defun unfold-record (record)
  (let ((new-groups (unfold-groups (springs-groups record)))
        (new-report (unfold-report (springs-report record))))
    (make-springs :report new-report :groups new-groups)
    ))

;; 525152
(defun day12-test3 ()
  (let ((records (read-file "inputs/day12-test1" #'read-condition-record)))
    (sum-arrangements (mapcar #'unfold-record records))))

(defun day12-real2 ()
  (let ((records (read-file "inputs/day12" #'read-condition-record)))
    (sum-arrangements (mapcar #'unfold-record records))))



