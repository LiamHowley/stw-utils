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


(declaim (ftype (function (function) (values fixnum fixnum))
		consume-until
		consume-while))

(defun consume-while (predicate)
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
      do (next))
    (values start *char-index*)))


(defun consume-until (predicate)
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
      do (next))
    (values start *char-index*)))


(declaim (ftype (function () (values fixnum fixnum)) consume-whitespace)
	 (inline consume-whitespace))

(defun consume-whitespace ()
  "Updates *CHAR-INDEX* while consuming 
whitespace characters. Returns the values
 starting index and *CHAR-INDEX*."
  (declare (optimize (safety 0) (speed 3)))
  (let ((start *char-index*))
    (declare (fixnum start))
    (loop
      while (whitespacep (stw-read-char))
      do (next))
    (values start *char-index*)))


(declaim (inline match-string)
	 (ftype (function (simple-string &optional boolean trie) (values function trie))
		match-string))

(defun match-string (token &optional case-sensitive (trie (make-trie)))
  "Adds the token to the trie as supplied by the &optional arg. 
Returns both a function which searches the *DOCUMENT* string at
*CHAR-INDEX* for a match within the trie, and the trie itself, 
so that other tokens may be added to the trie."
  (declare (optimize (speed 3) (safety 0))
	   (type trie trie))
  (insert-word token trie token case-sensitive)
  (values 
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
	     (next (length result))))))
   trie))


(declaim (ftype (function (list &optional boolean) (values function trie)) match-strings)
	 (inline match-strings))

(defun match-strings (tokens &optional case-sensitive)
  "Takes tokens and retrieves a search function and trie from 
the call to MATCH-STRING on the first token. Adds tokens to the 
rest of the tokens to the trie.

Returns both the search function which searches the *DOCUMENT* string at
*CHAR-INDEX* for a match within the trie, and the trie itself, 
so that other tokens may be added to the trie."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (func trie)
      (match-string (car tokens))
    (loop
      for token in (cdr tokens)
      do (insert-word token trie token case-sensitive))
    (values func trie)))



(declaim (inline match-character)
	 (ftype (function (&rest character) function)
		match-character))

(defun match-character (&rest chars)
  #'(lambda (char)
      (declare (character char)
	       (optimize (speed 3) (safety 0)))
      (when (member char chars :test #'char=)
	*char-index*)))



(declaim (ftype (function (function) (values (or string null) character boolean)) read-until))

(defun read-until (predicate)
  "Reads until character/token challenge is met and can be invoked 
as required. Predicate is a function that accepts one character. 
Returns a subseq of *DOCUMENT*,the last char read and a boolean 
indicating EOF"
  (declare (optimize (safety 0) (speed 3)))
  (let ((start *char-index*))
    (declare (fixnum start))
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
  (multiple-value-bind (character position)
      (funcall decoder *char-index*)
    (when character
      (setf *char-index* (the fixnum position))
      character)))


(declaim (ftype (function (&optional function character) (values string character boolean))
		read-and-decode))

(defun read-and-decode (&optional (predicate (constantly nil)) (decode-char #\&))
  "Reads and decodes *DOCUMENT*, until EOF or PREDICATE. PREDICATE is tested
against all chars, but those in encoded entities. Returns a decoded subseq of 
*DOCUMENT*,the last char read and a boolean indicating EOF"
  (declare (optimize (safety 0) (speed 3))
	   (inline make-displaced-array))
  (let ((fragments)
	(start)
	(last))
    (loop
      (multiple-value-bind (token char eof)
	  (read-until #'(lambda (char)
			  (when (or (funcall (the function predicate) char)
				    (char= char decode-char))
			    *char-index*)))
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
			    (push (make-displaced-array *document* start *char-index*) fragments)))))
	       (setf last *char-index*))
	      (t
	       (cond (fragments
		      (push (the string (make-displaced-array *document* last *char-index*)) fragments)
		      (return (values (the simple-array (concat-string (nreverse fragments))) char eof)))
		     (t
		      (return (values token char eof))))))))))


(defmacro with-encoder (string encoder &body body)
  `(let ((*document* ,string)
	 (*length* (length ,string))
	 (*char-index* 0)
	 (*encoder* ,encoder))
     ,@body))

(declaim (ftype (function (&optional function function) (values string character boolean))
		read-and-encode))

(defun read-and-encode
    (&optional
       (predicate (constantly nil))
       (encodep #'(lambda (char)
		    (if (char= char #\&)
			(null (decode *decoder*))
			(member char '(#\' #\" #\< #\>))))))
  "Accepts two optional args, a predicate function that accepts a character or string, 
and determines a point of exit, and ENCODEP, a predicate of one character whose function 
is to determine whether to encode or not. 

The default setting for encodep is to assume xml special characters are to be encoded. As xml
special characters are parsed beginning with the #\& ampersand character, any #\& read is first tested
with (decoder *DECODER*) to test for an already encoded character. If it is already encoded, it is 
ignored. As this behaviour occurs within the body of the predicate ENCODEP, it is a trivial matter
to change this behaviour to require double encoding.

Returns an encoded subseq of *DOCUMENT*, the last char read and a boolean indicating EOF"
  (declare (optimize (safety 0) (speed 3)))
  (let ((test)
	(fragments))
    (loop
      (multiple-value-bind (token char eof)
	  (read-until #'(lambda (char)
			  (when (or (setf test (funcall (the function predicate) char))
				    (funcall (the function encodep) char))
			    *char-index*)))
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
	       (return (values token char eof))))))))
