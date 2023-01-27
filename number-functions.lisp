(in-package stw.util)


(declaim (ftype (function (string &optional float) float) float%)
	 (inline float%))

(defun get-float (string &optional prototype)
  (declare (optimize (speed 3)(safety 0)))
  (let ((num (read-from-string value)))
    (the float 
	 (etypecase num
	   (float num)
	   (number (aif prototype
			(float num self)
			(float num)))))))
