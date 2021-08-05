(in-package stw.util)


(defmethod ensure-string (value)
  (format nil "~a" value))

(defmethod ensure-string ((value string))
  value)

(defmethod ensure-string ((value number))
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
	   when (and insert-space (> remaining 0))
	   do (write-string " " s))
	(get-output-stream-string s))))


(defgeneric match-token (token)
  (:documentation "match-token accepts a token as argument and returns
a closure that accepts both a sequence and index, (type fixnum), as arguments 
and returns list of coordinates associated with matching tokens.

 Token can be string, character, number etc. If a function is passed 
it must accept a seq and index (type fixnum), as arguments, and return a list 
of matching index and (+ index (length index)), or nil. 
E.g. With sequence equaling \"abcdefgabcdefg\" and the index 7 being passed,
a token matching \"abc\" would return (7 10)."))

(defmethod match-token (token)
  (match-token (ensure-string token)))

(defmethod match-token ((token function))
  token)

(defmethod match-token ((token character))
  #'(lambda (seq index)
      (declare (fixnum index))
      (when (char= token (aref seq index))
	(list index (1+ index)))))

(defmethod match-token ((token string))
  (let ((token-length (array-total-size token)))
    (declare (fixnum token-length))
    #'(lambda (seq index)
	(declare (fixnum index))
	;;(declare (optimize (safety 0) (speed 3)))
	(when (char-equal (aref seq index) (aref token 0))
	  (let* ((end (+ index token-length))
		 (within-bounds (>= (length seq) end)))
	    (declare (fixnum end))
	    (when (and within-bounds
		       (string= (make-array token-length
					    :element-type 'character
					    :displaced-to seq
					    :displaced-index-offset index)
				token))
	      (list index end)))))))

(defun match-tokens (&rest tokens)
  (loop for token in tokens
     collect (match-token token)))

(defun consume-sequence (matchers seq)
  (let ((seq-length (array-total-size seq))
	(acc))
    (loop for index below seq-length
       do (loop for func in matchers
	     do (let ((result (funcall func seq index)))
		  (when result
		    (push result acc)))))
    (when acc
      (nreverse acc))))
       
(defun split-sequence% (fn seq &rest args)
  (let ((last))
    (map-tree-depth-first
     #'(lambda (index)
	 (declare (fixnum index))
	 (unless (and last
		      (<= index last))
	   (prog1
	       (funcall fn
			(if last
			    (subseq seq last index)
			    (unless (eql index 0)
			      (subseq seq 0 index))))
	     (setf last index))))
     (list (consume-sequence (apply #'match-tokens args) seq)
	   (array-total-size seq)))))

(defun find-all (seq &rest args)
  "Find location of all instances of character, string tokens,
or functions. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5)."
  (declare (inline consume-sequence))
  (consume-sequence (apply #'match-tokens args) seq))

(defun split-sequence (seq &rest args)
  "Split sequence with multiple character, string delimiters or 
functions. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5).

Returns ordered list of strings, including delimiting tokens."
  (declare (inline split-sequence%))
  (apply #'split-sequence% #'identity seq args))

(defun find-and-replace (seq args)
  "Find and replace multiple characters or strings.
Requires a sequence and alist of (<to-find> . <to-replace>) pairs.
Returns an amended copy of the sequence."
  (declare (inline split-sequence% concat-string))
  (concat-string
   (apply #'split-sequence%
	  #'(lambda (str)
	      (let ((to-replace (assoc-if
				 #'(lambda (key)
				     (equal (ensure-string key) str))
				 args)))
		(if to-replace
		    (ensure-string (cdr to-replace))
		    str)))
	  seq
	  (mapcar #'car args))))
