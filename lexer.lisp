(in-package :stw.util)


(defvar *document*)

(declaim (fixnum *char-index* *length*)
	 (simple-string *document*))


(defvar *char-index* 0 
  "Do not set globally. Matches the index of next character to be read in the working document.")

(defvar *length* 0 
  "Do not set globally. Matches the length of the working document.")

(defvar *decoder* nil
  "Do not set globally. Decoding function for parsing documents.")

(defvar *encoder* nil
  "Do not set globally. Encoding function for parsed documents.")


(declaim (inline stw-read-char next)
	 (ftype (function (&optional fixnum) (or null fixnum))
		next)
	 (ftype (function () (or character keyword))
		stw-read-char))

(defun stw-read-char ()
  "Reads char from *DOCUMENT*. Returns char."
  (declare (optimize (safety 0) (speed 3)))
  (if (< *char-index* *length*)
      (the character (char (the simple-string *document*) *char-index*))
      (the keyword :eof)))

(defun next (&optional (increment 1))
  "Increments *CHAR-INDEX* by a supplied increment.
If none is supplied, *CHAR-INDEX* increments by 1"
  (declare (optimize (safety 0) (speed 3)))
  (setf *char-index* (+ increment *char-index*)))


(declaim (inline stw-peek-next-char
		 stw-peek-last-char)
	 (ftype (function () (or character null))
		stw-peek-next-char
		stw-peek-last-char))

(defun stw-peek-next-char ()
  "Returns the next char in *DOCUMENT*."
  (declare (optimize (safety 0) (speed 3)))
  (when (< (1+ *char-index*) *length*)
    (char *document* (1+ *char-index*))))

(defun stw-peek-last-char ()
  "Returns the last char in *DOCUMENT*"
  (declare (optimize (safety 0) (speed 3)))
  (when (> *char-index* 0)
    (char *document* (1- *char-index*))))



(declaim (ftype (function (function) function)
		consume-until
		consume-while))

(defun consume-while (predicate)
  "Consumes while character/token challenge is met. Predicate is a 
function that accepts one character. Returns the index of the first
and last characters read."
  #'(lambda (&optional (start *char-index*))
      (declare (fixnum start)
	       (optimize (safety 0) (speed 3)))
      (loop
	for char = (stw-read-char)
	until (eq char :eof)
	while (funcall predicate char)
	do (next))
      (values start *char-index*)))


(defun consume-until (predicate)
  "Consumes until character/token challenge is met. Predicate is a 
function that accepts one character. Returns the index of the first
and last characters read."
  (lambda (&optional (start *char-index*))
    (declare (fixnum start)
	     (optimize (safety 0) (speed 3)))
    (loop
      for char = (stw-read-char)
      until (or (eq char :eof)
		(funcall predicate char))
      do (print char)
      do (next))
    (values start *char-index*)))

(defvar *consume-whitespace* (consume-while #'whitespacep)
  "Returns a closure.")

(declaim (inline match-string)
	 (ftype (function (simple-string &optional boolean trie) function)
		match-string))


(defun match-string (token &optional case-sensitive (trie (make-trie)))
  (let ((length (length token)))
    (declare (type trie trie) (fixnum length)
	     (optimize (speed 3) (safety 0)))
    (insert-word token trie token case-sensitive)
    #'(lambda (&optional char)
	(declare (ignore char)
		 (inline trie-leaf)
		 (optimize (speed 3) (safety 0)))
	(let ((result)
	      (foundp (walk-branch trie)))
	  (declare (function foundp))
	  (loop
	     for index of-type fixnum from *char-index* below *length*
	     for char = (the character (char *document* index))
	     for next = (funcall foundp char)
	     when next
	     do (setf result (trie-leaf next))
	     unless next
	     do (return))
	  (when result
	    (prog1 *char-index*
	      (next length)))))))


(declaim (inline match-character)
	 (ftype (function (&rest character) function)
		match-character))

(defun match-character (&rest chars)
  #'(lambda (char)
      (declare (character char)
	       (optimize (speed 3) (safety 0)))
      (when (member char chars :test #'char=)
	*char-index*)))


(declaim (ftype (function (function) function) read-until))

(defun read-until (predicate)
  "Creates and returns a reader function that reads until 
character/token challenge is met and can be invoked as required. 
Predicate is a function that accepts one character. Each time the
reader is invoked, it returns a subseq of *DOCUMENT*, 
the last char read and a boolean indicating EOF"
  #'(lambda (&optional (start *char-index*))
      (declare (optimize (safety 0) (speed 3))
	       (fixnum start))
      (loop 
	 (let ((char (stw-read-char)))
	   (etypecase char
	     (character
	      (let ((result (funcall (the function predicate) char)))
		(cond ((and result (eql *char-index* start))
		       ;; empty
		       (return (values nil char nil)))
		      (result
		       (return (values (subseq *document* start result)
				       char
				       nil)))
		      (t (next)))))
	     (keyword 
	      (return 
		(values (subseq *document* start *char-index*)
			(stw-peek-last-char)
			t))))))))



(declaim (ftype (function (function) (or character null)) decode)
	 (inline decode))

(defun decode (decoder)
  "Decoder is a function that returns the opening and closing
coordinates of the observed entity, and the decoded character
as a result."
  (declare (optimize (safety 0) (speed 3)))
  (multiple-value-bind (coordinates result)
      (funcall decoder *char-index*)
    (when result
      (setf *char-index* (cadr coordinates))
      result)))



(declaim (ftype (function (&optional function) function)
		read-and-decode
		read-and-encode))

(defun read-and-decode (&optional (predicate (constantly nil)) (decode-char #\&))
  "Reads and decodes *DOCUMENT*, until EOF or PREDICATE. PREDICATE is tested
against all chars, but those in encoded entities. Returns a reader function 
to be invoked as required."
  (declare (optimize (safety 0) (speed 3))
	   (inline make-displaced-array))
  (let ((reader (read-until #'(lambda (char)
				(when (or (funcall (the function predicate) char)
					  (char= char decode-char))
				  *char-index*)))))
    #'(lambda ()
	(let ((fragments)
	      (start)
	      (last))
	  (declare (reader function))
	  (loop
	    (multiple-value-bind (token char eof)
		(funcall reader)
	      (setf start *char-index*)
	      (cond ((eq char decode-char)
		     (when token
		       (push token fragments))
		     (let ((character (decode *decoder*)))
		       (if character
			   (push character fragments)
			   ;; test if char is matched by the predicate function.
			   ;; If so process and return.
			   (cond ((funcall (the function predicate) decode-char)
				  (push (make-displaced-array *document* start *char-index*) fragments)
				  (return (values (the simple-array (concat-string (nreverse fragments))) decode-char eof)))
				 (t
				  (next)
				  (push (make-displaced-array *document*  start *char-index*) fragments)))))
		     (setf last *char-index*))
		    (t
		     (cond (fragments
			    (push (the string (make-displaced-array *document* last *char-index*)) fragments)
			    (return (values (the simple-array (concat-string (nreverse fragments))) char eof)))
			   (t
			    (return (values token char eof))))))))))))



(defmacro with-encoder (string encoder &body body)
  `(let ((*document* ,string)
	 (*length* (length ,string))
	 (*char-index* 0)
	 (*encoder* ,encoder))
     ,@body))


(defun read-and-encode (&optional (encodep #'(lambda (char)
					       (if (char= char #\&)
						   (null (decode *decoder*))
						   (member char '(#\' #\" #\< #\>))))))
  "Returns a closure around the predicate encodep that reads and encodes characters within 
*DOCUMENT* when the optional ENCODEP returns true. 

ENCODEP is a predicate of one character whose function is to determine whether to encode or not.
The default setting for encodep is to assume xml special characters are to be encoded. As xml
special characters are parsed beginning with the #\& ampersand character, any #\& read is first tested
with (decoder *DECODER*) to test for an already encoded character. If it is already encoded, it is 
ignored. As this behaviour occurs within the body of the predicate ENCODEP, it is a trivial matter
to change this behaviour to require double encoding.

Within the returned closure a reader is formed from the return value of READ-UNTIL. This return value
encapsulates a predicate of either ENCODEP or the predicate return-on-token."
  (let ((test))
    #'(lambda (&optional (return-on-token (constantly nil)))
	(declare (optimize (safety 0) (speed 3)))
	(let ((fragments)
	      (reader (read-until #'(lambda (char)
				      (when (or (setf test (funcall (the function return-on-token) char))
						(funcall (the function encodep) char))
					*char-index*)))))
	  (loop
	    (multiple-value-bind (token char eof)
		(funcall reader)
	      ;; catch special chars.
	      (cond ((and fragments (or eof test))
		     (when token
		       (push token fragments))
		     (return (values (the simple-array (concat-string (nreverse fragments))) char eof)))
		    ((and char (or eof test))
		     ;; no fragments means we've run through without needing to funcall *ENCODER*
		     (return (values token char eof)))
		    (char
		     (when token
		       (push token fragments))
		     (push (the simple-array (funcall (the function *encoder*) char)) fragments)
		     (next))
		    (t
		     (return (values token char eof))))))))))
