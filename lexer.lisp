(in-package :stw.util)


(defvar *document*)

(declaim (fixnum *char-index* *line-number* *length*)
	 (simple-string *document*))


(defvar *char-index* 0 
  "Do not set globally. Matches the index of next character to be read in the working document.")

(defvar *line-number*  0
  "Do not set globally. Matches the current line of the working document.")

(defvar *length* 0 
  "Do not set globally. Matches the length of the working document.")

(defvar *decoder* nil
  "Do not set globally. Decoding function for parsing documents.")

(defvar *consume-whitespace* nil
  "Do not set globally. Function for skipping whitespace in documents.")


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



(declaim (ftype (function (function &optional function) (values fixnum fixnum))
		consume-until
		consume-while))

(defun consume-while (predicate &optional (parse (constantly nil)))
  "Consumes while character/token challenge is met. Predicate is a 
function that accepts one character. Returns the index of the first
and last characters read."
  (declare (optimize (safety 0) (speed 3)))
  (let ((start *char-index*))
    (declare (fixnum start))
    (loop
      for char = (stw-read-char)
      until (eq char :eof)
      while (funcall predicate char)
      do (funcall parse char)
      do (next))
    (values start *char-index*)))


(defun consume-until (predicate &optional (parse (constantly nil)))
  "Consumes until character/token challenge is met. Predicate is a 
function that accepts one character. Returns the index of the first
and last characters read."
  (declare (optimize (safety 0) (speed 3)))
  (let ((start *char-index*))
    (declare (fixnum start))
    (loop
      for char = (stw-read-char)
      until (or (eq char :eof)
		(funcall predicate char))
      do (funcall parse char)
      do (next))
    (values start *char-index*)))


(declaim (inline match-string)
	 (ftype (function (simple-string &optional boolean trie) function)
		match-string))


(defun match-string (token &optional case-sensitive trie)
  (let ((trie (or trie (make-trie)))
	(length (length token)))
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
  "Read until character/token challenge is met. Predicate is a 
function that accepts one character. Returns subseq of *document*, 
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
  "decoder is a function that returns the opening and closing
coordinates of the observed entity, and the decoded character
as a result."
  (declare (optimize (safety 0) (speed 3)))
  (multiple-value-bind (coordinates result)
      (funcall decoder *char-index*)
    (when result
      (setf *char-index* (cadr coordinates))
      result)))



(declaim (ftype (function (&optional function) function)
		read-and-decode))

(defun read-and-decode (&optional (predicate (constantly nil)) (decode-char #\&))
  "Reads and decodes *DOCUMENT*, until EOF or PREDICATE. PREDICATE is tested
against all chars, but those in encoded entities."
  (declare (optimize (safety 0) (speed 3))
	   (inline make-displaced-array)
	   (simple-string *document*))
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
