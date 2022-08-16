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


(declaim (ftype (function ((or list array) &optional boolean (func function)) simple-array) concat-string))

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



(declaim (ftype (function (string fixnum &optional fixnum) string) make-displaced-array)
	 (inline make-displaced-array))

(defun make-displaced-array (array start &optional (end (length array)))
  "makes an array of element-type character displaced to ARRAY."
  (declare (optimize (speed 3) (safety 0)))
  (the string (make-array (- (the fixnum end) (the fixnum start))
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
	(seq-length (array-total-size seq))
	(initial-char (aref token 0)))
    (declare (fixnum token-length seq-length)
	     (character initial-char))
    #'(lambda (index)
	(declare (fixnum index)
		 (optimize (speed 3) (safety 0)))
	(when (char-equal (aref seq index) initial-char)
	  (let* ((end (+ index token-length))
		 (within-bounds (< end (1+ seq-length))))
	    (declare (fixnum end)
		     (boolean within-bounds))
	    (when (and within-bounds
		       (string= (make-displaced-array seq index end)
				token))
	      (values (list index end)
		      token)))))))


(defun match-tokens (seq &optional tokens (trie (make-trie)))
  "tokens and trie are both optional to allow for a precompiled
trie to be added."
  (let ((characters))
    (when tokens
      (loop for token in tokens
	 do (typecase token
	      (string
	       (insert-word token trie))
	      (character
	       (push token characters)
	       (insert-key trie token)))))
    (lambda (index)
      (let ((result)
	    (foundp (walk-branch trie)))
	(loop for i from index below (length seq)
	   initially (let ((char (aref seq i)))
		       (when (member char characters :test #'char=)
			 (setf result char)))
	   for char = (aref seq i)
	   for next = (funcall foundp char)
	   for value = (when next
			 (trie-leaf next))
	   unless next
	   do (return)
	   when value
	   do (setf result value))
	(when result
	  (values (list index
			(+ index (etypecase result
				   (string (length result))
				   (character 1))))
		  result))))))


(defun consume-sequence (matchers seq
			 &key (start 0) (end (length seq)) end-test
			   (map (constantly nil)) one-only)
  (loop 
     for index from start
     until (or (and end-test
		    (funcall end-test (aref seq index)))
	       (eql index end)
	       (and one-only acc))
     for acc = (funcall matchers index)
     when acc
     do (setf acc (funcall map acc))
     when acc
     collecting acc into results
     finally (return (cond (one-only
			    results)
			   (t
			    (values results index))))))


(defun map-exploding-string (seq args
			&key
			  (start 0) (end (length seq)) end-test
			  remove-separators one-only with-bounding-text
			  (map-word #'identity)
			  (map-delimiter #'identity))
  (let* ((length (length seq))
	 (last start)
	 (list))
    (when (and with-bounding-text (> start 0))
      (push (subseq seq 0 start) list))
    (consume-sequence (etypecase args
			(list (match-tokens seq args))
			(atom (match-token seq args)))
		      seq
		      :start start
		      :end end
		      :end-test end-test
		      :one-only one-only
		      :map #'(lambda (indices)
			       (destructuring-bind (start end)
				   indices
				 (unless (and last
					      (<= start last))
				   (push (funcall (the function map-word) (subseq seq last start)) list)
				   (unless remove-separators
				     (push (funcall (the function map-delimiter) (subseq seq start end)) list)))
				 (setf last end)
				 (values))))
    (when (or with-bounding-text (< last length))
      (push (subseq seq last length) list))
    (nreverse list)))



(defun explode-string (seq args
		       &rest params
		       &key (start 0) (end (length seq)) end-test remove-separators one-only)
  "Split sequence with multiple character, string delimiters or 
functions. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5).

Returns ordered list of strings, including delimiting tokens."
  (declare (inline map-exploding-string)
	   (ignore start end end-test remove-separators one-only))
  (apply #'map-exploding-string seq args params))


(defun find-all (seq args
		 &rest params
		 &key (start 0) (end (length seq)) end-test)
  "Find location of all instances of character, string tokens,
or functions. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5)."
  (declare (inline consume-sequence) (ignore start end end-test))
  (apply #'consume-sequence
	 (etypecase args
	   (list (match-tokens seq args))
	   (atom (match-token seq args)))
	 seq
	 :map #'identity
	 params))


(defun find-and-replace (seq args &rest params &key (start 0) (end (1- (length seq))) end-test)
  "Find and replace multiple characters or strings.
Requires a sequence and alist of (<to-find> . <to-replace>) pairs.
Returns an amended copy of the sequence."
  (declare (inline map-exploding-string) (ignore start end end-test))
  (let ((replaced))
    (values 
     (concat-string
      (apply #'map-exploding-string
	     seq
	     (mapcar #'car args)
	     :map-delimiter 
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
	     (mapcar #'car args)
	     :with-bounding-text t
	     params))
     replaced)))
