(in-package ctx.util)



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


(defmethod concat-string ((list list))
  "returns a simple-array from a list of strings,"
  (let ((s (make-string-output-stream))
	(length (length list)))
    (loop for string in list
       for count from 1
       for str = (ensure-string string)
       for remaining = (- length count)
       when str
       do (write-string str s)
       when (> remaining 0)
       do (write-string " " s))
    (get-output-stream-string s)))


(defmethod concat-string ((array array))
  "returns a simple-array from an array of strings,"
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
    (get-output-stream-string s)))



(declaim (inline indent-string)
	 (ftype (function (fixnum stream) string) indent-string))

(defun indent-string (num stream)
  "Indent string by number of chars. Takes two args: num and stream."
  (declare (optimize (speed 3) (safety 0)))
  (fresh-line stream)
  (write-string (make-string num :initial-element #\space)
		stream))


(declaim (ftype (function (string) boolean) empty-string/newline-p)
	 (inline empty-string/newline-p))

(defun empty-string/newline-p (string)
  "Test if string is empty"
  (declare (optimize (speed 3) (safety 0)))
  (loop for char across (the string string)
     always (whitespacep char)))


(declaim (ftype (function (character) boolean) newlinep whitespacep)
	 (inline newlinep whitespacep))

(defun newlinep (char)
  (declare (optimize (speed 3) (safety 0)))
  (case (the character char)
    ((#\newline #\linefeed #\return)
     t)
    (t nil)))

(defun whitespacep (char)
  (declare (optimize (speed 3) (safety 0)))
  (or (char= (the character char) #\space)
      (char= (the character char) #\tab)
      (newlinep (the character char))))
