;; day 7 no space left of device

(defstruct file-system-object (name "NA"))
(defstruct (dir (:include file-system-object)) (children) (parent))
(defstruct (file (:include file-system-object)) (size 0))

(defparameter *current-dir* nil) 
(defparameter *file-tree* (make-dir :name "/"))
(defparameter *current-command* nil)

(defmethod total-size (f))
(defmethod total-size ((f dir))
  (loop for c in (dir-children f)
        sum (total-size c)
        ))
(defmethod total-size ((f file))
  (file-size f))

(defun dir-contains (parent name)
  (find name (dir-children parent)
        :test (lambda (a b) (equal a (file-system-object-name b)))))

(defun process-dir (parent name)
  (unless (dir-contains parent name)
    (push (make-dir :name name :parent parent) (dir-children parent))))

(defun process-file (parent name size)
  (unless (dir-contains parent name)
    (push (make-file :name name :size (parse-integer size)) (dir-children parent))))

(defun process-ls (dir line)
  (let ((tokens (uiop:split-string line)))
    (if (equal "dir" (car tokens))
        (process-dir dir (cadr tokens))
        (process-file dir (cadr tokens) (car tokens))
        )))

(defun subdirs (dir)
  (remove-if (lambda (d) (not (dir-p d))) (dir-children dir)))

(defun do-cd (input)
  (setq *current-command* 'cd)
  (cond ((equal "/" input)
         (setq *current-dir* *file-tree*))
        ((equal ".." input)
         (setq *current-dir* (dir-parent *current-dir*))
         )
        (t
         (process-dir *current-dir* input)
         (setq *current-dir* (dir-contains *current-dir* input))
         )))


(defun do-ls ()
  (setq *current-command* 'ls))

(defun process-command (cmd)
  (let ((tokens (uiop:split-string cmd)))
    (when (equal "$" (car tokens))
      (cond
        ((equal "cd" (cadr tokens)) (do-cd (caddr tokens)))
        ((equal "ls" (cadr tokens)) (do-ls))
        (t (princ "unknown command")
           (setq *current-command* nil))
        ))))

(defun process-cmd-output (line)
  (cond
    ((eq 'ls *current-command*)
     (process-ls *current-dir* line))
    (t (princ "unknown output"))
    ))


(defun read-no-space-file ()
  (setq *current-dir* nil)
  (setq *current-command* nil)
  (setq *file-tree* (make-dir :name "/"))
  (with-open-file (in "input-day07" :if-does-not-exist nil) 
    (when in
      (loop for line = (read-line in nil)
            while line do
              (if (equal #\$ (char line 0))
                  (process-command line)
                  (process-cmd-output line)
                  ))
      )))


(defun find-small-dirs-rec (dir-tree size results)
  (cond
    ((equal nil dir-tree) results)
    (t
     (loop for d in (subdirs dir-tree) sum
           (find-small-dirs-rec d size
                                ))

     (mapcar
        (lambda (d)
          (find-small-dirs-rec d size
                               (if (<= (total-size dir-tree) size)
                                   (+ (total-size dir-tree) results)
                                   results
                                   )
                               ))
        (subdirs dir-tree)))
  ))

(defun find-small-dirs (dir-tree)
  (let ((this-size (total-size dir-tree)))
    (+ (if (< this-size 100000) this-size 0)
       (reduce #'+ (mapcar #'find-small-dirs (subdirs dir-tree)) :initial-value 0))
    ))

(defun find-needed-space (dir-tree)
  (- 30000000 (- 70000000 (total-size dir-tree))))

(defparameter *needed-space* 9192532)

(defun find-dirs-sizes (dir-tree smallest)
  (let ((this-size (total-size dir-tree)))
    (cond
      ((< this-size *needed-space*) smallest)
      ((= this-size *needed-space*) this-size)
      (t (apply #'min (cons this-size (mapcar (lambda (d) (find-dirs-sizes d smallest)) (subdirs dir-tree)))))
      )))
