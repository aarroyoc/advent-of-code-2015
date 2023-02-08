(defun get-first-line (filename)
  (let ((file (open filename)))
    (read-line file)))

(defparameter *data* (coerce (get-first-line "input") 'list))

(defun char-to-int (char)
  (cond
    ((eq char #\() 1)
    ((eq char #\)) -1)
    (t 0)))

(defun part-one ()
  (reduce #'+ (mapcar #'char-to-int *data*)))

(defun count-until-basement (line &optional (current-floor 0) (count 0))
  (cond
    ((eq current-floor -1) count)
    (t (count-until-basement (rest line) (+ current-floor (char-to-int (first line))) (+ count 1)))))

(defun part-two ()
  (count-until-basement *data*))
