(in-package stw.util)

(defun xor (&rest rest)
  "Either or neither, but not more than one.
Returns two values: either -> the value of the 
first true value and T; neither -> nil and t; 
otherwise nil and nil."
  (let ((hit))
    (map-tree-depth-first
     #'(lambda (arg)
	 (cond ((and hit arg)
		(return-from xor (values nil nil)))
	       (arg
		(setf hit arg))))
     rest)
    (values hit t)))
