(defun read-file (filename)
  (let ((stream (make-string-output-stream)))
    (progn
      (sb-ext:run-program "/usr/bin/awk" (list "-f" "pre.awk" filename) :output stream)
      (read-from-string (get-output-stream-string stream)))))

(defparameter *data* (read-file "input"))

(defun turn-on (grid x y)
  (setf (aref grid x y) 1))

(defun turn-off (grid x y)
  (setf (aref grid x y) 0))

(defun toggle (grid x y)
  (if (= 0 (aref grid x y))
      (setf (aref grid x y) 1)
      (setf (aref grid x y) 0)))


(defun count-lights (grid)
  (loop for x from 0 to 999
	sum (loop for y from 0 to 999
		  sum (aref grid x y))))

(defun visit-lights (turn-on turn-off toggle)
  (let ((grid (make-array '(1000 1000))))
    (progn
      (loop for operation in *data*
            do (let ((type (nth 0 operation))
	             (x0 (nth 1 operation))
	             (y0 (nth 2 operation))
	             (x1 (nth 3 operation))
	             (y1 (nth 4 operation)))
	         (loop for x from x0 to x1
		       do (loop for y from y0 to y1
				do (cond
			             ((eq type :turn-on) (funcall turn-on grid x y))
		                     ((eq type :turn-off) (funcall turn-off grid x y))
		                     ((eq type :toggle) (funcall toggle grid x y)))))))
      grid)))

(defun part-one ()
  (count-lights (visit-lights #'turn-on #'turn-off #'toggle)))


(defun turn-on-2 (grid x y)
  (setf (aref grid x y) (+ 1 (aref grid x y))))

(defun turn-off-2 (grid x y)
  (setf (aref grid x y) (if (= 0 (aref grid x y))
			    0
			    (- (aref grid x y) 1))))

(defun toggle-2 (grid x y)
  (setf (aref grid x y) (+ 2 (aref grid x y))))    

(defun part-two ()
  (count-lights (visit-lights #'turn-on-2 #'turn-off-2 #'toggle-2)))

