(in-package stw.util)


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


(defmethod concat-string ((list list) &optional insert-space (func #'identity))
  "Returns a simple-array from a list."
  (let ((s (make-string-output-stream))
	(length (length list)))
    (loop for string in list
       for count from 1
       for str = (ensure-string string)
       for remaining = (- length count)
       when str
       do (write-string (funcall func str) s)
       when (and insert-space (> remaining 0))
       do (write-string " " s))
    (get-output-stream-string s)))


(defmethod concat-string ((array array) &optional insert-space (func #'identity))
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
	   do (write-string (funcall func str) s)
	   when (and insert-space (> remaining 0))
	   do (write-string " " s))
	(get-output-stream-string s))))



(declaim (ftype (function (vector fixnum &optional fixnum) vector) make-displaced-array)
	 (inline make-displaced-array))

(defun make-displaced-array (array start &optional (end (length array)))
  "makes an array of element-type character displaced to ARRAY."
  (declare (optimize (speed 3) (safety 0)))
  (the vector (make-array (- (the fixnum end) (the fixnum start))
			  :element-type 'character
			  :displaced-to (the simple-array array)
			  :displaced-index-offset (the fixnum start))))

(defgeneric match-token (seq token)
  (:documentation "Match-token accepts a token and sequence as arguments and 
returns a closure that accepts both a sequence and index, (type fixnum), as 
arguments and returns list of coordinates associated with matching tokens. 
Uses a naive brute force algorithm to facilitate batch searches of multiple tokens.

Token can be string, character, number etc. If a function is passed 
it must accept an index (type fixnum), as argument, and return a list 
of matching index and (+ index (length index)), or nil. 
E.g. With sequence equaling \"abcdefgabcdefg\" and the index 7 being passed,
a token matching \"abc\" would return (7 10)."))

(defmethod match-token (seq token)
  (declare (optimize (speed 3) (safety 0)))
  (match-token seq (ensure-string token)))

(defmethod match-token (seq (token function))
  (declare (ignore seq)
	   (optimize (speed 3) (safety 0)))
  token)

(defmethod match-token ((seq string) (token character))
  #'(lambda (index)
      (declare (fixnum index)
	       (optimize (speed 3) (safety 0)))
      (when (char= token (aref seq index))
	(list index (1+ index)))))

(defmethod match-token ((seq string) (token string))
  (let ((token-length (array-total-size token))
	(seq-length (array-total-size seq)))
    (declare (fixnum token-length))
    #'(lambda (index)
	(declare (fixnum index)
		 (optimize (speed 3) (safety 0)))
	(when (char-equal (aref seq index) (aref token 0))
	  (let* ((end (+ index token-length))
		 (within-bounds (< end (1+ seq-length))))
	    (declare (fixnum end)
		     (boolean within-bounds))
	    (when (and within-bounds
		       (string= (make-displaced-array seq index end)
				token))

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
(defun match-tokens (seq &rest tokens)
  (if (eql (length tokens) 1)
      (match-token seq (car tokens))
      (loop for token in tokens
	 collect (match-token seq token))))

	
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
  (declare (inline split-sequence%))
  (let ((replaced))
    (values 
     (concat-string
      (apply #'split-sequence%
	     #'(lambda (str)
		 (let ((to-replace (assoc-if
				    #'(lambda (key)
					(equal (ensure-string key) str))
				    args)))
		   (if to-replace
		       (prog1
			   (ensure-string (cdr to-replace))
			 (pushnew to-replace replaced :test #'equal))
		       str)))
	     seq
	     (mapcar #'car args)))
     replaced)))
