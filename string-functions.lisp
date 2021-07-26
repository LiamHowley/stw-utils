(in-package stw.util)


(defmethod ensure-string (value)
  (format nil "~a" value))

(defmethod ensure-string ((value string))
  value)

(defmethod ensure-string ((value real))
  (write-to-string value))

(defmethod ensure-string ((value character))
  (string value))

(defmethod ensure-string ((value cons))
  (concat-string value))

(defmethod ensure-string ((value array))
  (concat-string value))


(defmethod concat-string ((list list) &optional insert-space)
  "Returns a simple-array from a list."
  (let ((s (make-string-output-stream))
	(length (length list)))
    (loop for string in list
       for count from 1
       for str = (ensure-string string)
       for remaining = (- length count)
       when str
       do (write-string str s)
       when (and insert-space (> remaining 0))
       do (write-string " " s))
    (get-output-stream-string s)))


(defmethod concat-string ((array array) &optional insert-space)
  "Ensures array is a string."
  (if (stringp array)
      array
      (let ((s (make-string-output-stream))
	    (length (length array)))
	(loop for string across array
	   for count from 1
	   for str = (ensure-string string)
	   for remaining = (- length count)
	   when str
	   do (write-string str s)
	   when (> remaining 0)
	   do (write-string " " s))
	(get-output-stream-string s))))
