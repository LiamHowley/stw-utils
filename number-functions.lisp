(in-package stw.util)


(declaim (ftype (function (string &optional (or single-float double-float)) (or single-float double-float)) get-float)
	 (inline get-float))

(defun get-float (string &optional (prototype 0s0))
  (let ((num (read-from-string string)))
    (the float 
	 (etypecase num
	   (float (float num prototype))
	   (fixnum (float num prototype))))))
