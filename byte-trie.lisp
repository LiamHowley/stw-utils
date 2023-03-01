(in-package util.trie)

(declaim (inline next-key%
		 walk-branch%
		 find-in-array
		 insert-byte-key
		 insert-byte
		 byte-trie-leaf
		 byte-trie-branch
		 byte-trie-word
		 remove-key%
		 remove-trie%
		 remove-from-stack%))

(defstruct byte-trie
  word
  leaf
  (branch nil :type (or null array))
  (extension nil :type (or null hash-table)))

(defmethod print-object ((object byte-trie) stream)
  (print-unreadable-object (object stream :type t :identity t)))


(defun next-key% (key trie)
  "Returns two values: the cons of the next node, and the branch, or nil"
  (let ((branch (byte-trie-branch trie)))
    (flet ((get-extended-value ()
	     (awhen (byte-trie-extension trie)
	       (values (gethash key self) self))))
      (if branch
	  (awhen (aref branch key)
	    (if (typep self 'byte-trie)
		(values self branch)
		(get-extended-value)))
	  (get-extended-value)))))


(defun walk-branch% (trie &optional (fn #'next-key%))
  "Returns a function, that accepts a key as argument and encapsulates 
and updates the current working trie."
  (let ((working-trie trie))
    #'(lambda (key)
	(awhen (funcall fn key working-trie)
	  (setf working-trie self)))))


(defmethod find-word ((word array) (trie byte-trie))
  "Finds word in trie."
  (let ((foundp (walk-branch% trie)))
    (loop
       for byte across word
       for next = (funcall foundp byte)
       unless next
       do (return)
       finally (return
		 (awhen (byte-trie-leaf next)
		   self)))))


(defun find-in-array (array trie &key (start 0) (end (1- (length string))))
  "Walk string and search in trie for matches."
  (let ((result)
	(foundp (walk-branch% trie)))
    (declare (function foundp)
	     (optimize (speed 3) (safety 0)))
    (loop
       for i of-type (unsigned-byte 8) from start to end
       for byte = (the (unsigned-byte 8) (aref array i))
       for next = (funcall foundp byte)
       when next
       do (setf result (byte-trie-leaf next))
       unless next
       do (return))
    result))


(defun insert-byte-key (trie key &optional (branch-length 256) (next-trie (make-byte-trie)))
  "Insert key into branch of next-trie."
  (cond ((< key branch-length)
	 (or (byte-trie-branch trie)
	     (setf (byte-trie-branch trie) (make-array branch-length :element-type '(or fixnum byte-trie))))
	 (setf (aref (byte-trie-branch trie) key) next-trie))
	(t
	 (aif (byte-trie-extension trie)
	      (setf (gethash key self) next-trie)
	      (let ((table (setf (byte-trie-extension trie) (make-hash-table))))
		(setf (gethash key table) next-trie))))))


(defun insert-byte (byte trie &optional (branch-length 256))
  "Insert character into branch of trie."
  (declare (special case-sensitive)
	   (inline next-key%))
  (let ((alternate
	 (unless case-sensitive
	   (if (lowercasep byte) 
	       (get-uppercase byte)
	       (get-lowercase byte)))))
    (multiple-value-bind (next branch)
	(next-key% byte trie)
      (cond ((and next branch)
	     nil)
	    (branch
	     (setf next (insert-byte-key trie byte branch-length)))
	    (t
	     (setf next (insert-byte-key trie byte branch-length))
		   (setf
		   branch (byte-trie-branch trie))))
      (unless (or case-sensitive
		  (eql byte alternate)
		  (aref branch alternate))
	(insert-byte-key trie alternate branch-length next))
      (values next branch))))


(defmethod insert-word ((word array) (trie byte-trie) &optional (value (octets-to-string word)) (case-sensitive t))
  "Insert word into trie"
  (declare (special case-sensitive))
  (let ((foundp (walk-branch% trie #'insert-byte)))
    (loop
       for byte across word
       for next = (funcall foundp byte)
       finally (setf (byte-trie-leaf next) value
		     (byte-trie-word next) word))
    trie))


(defun remove-key% (trie key)
  "Remove key from trie."
  (setf (aref (byte-trie-branch trie) key) nil
	(byte-trie-leaf trie) nil
	(byte-trie-word trie) nil))


(defun remove-trie% (trie trie%)
  "Remove trie from branch."
  (loop
    for res across (byte-trie-branch trie)
    for i from 0
    when (eq trie% res)
      do (setf (aref (byte-trie-branch trie) i) nil))
  (setf (byte-trie-leaf trie) nil
	(byte-trie-word trie) nil))
  

(defun remove-from-stack% (stack)
  "Remove trie from trie stack. Helper function 
for DELETE-WORD."
  (let ((initial-trie (car stack)))
    (if (byte-trie-branch initial-trie)
	;; Word is a prefix to another word.
	(values t
		(setf (byte-trie-word initial-trie) nil
		      (byte-trie-leaf initial-trie) nil))
	(loop
	   while stack
	   do (let* ((trie (pop stack))
		     (children (append (loop
					 for trie% across (byte-trie-branch trie)
					 when (typep trie% trie)
					   collect trie%)
				       (loop for value being each hash-values of (byte-trie-extension trie)
					     collect value))))
		(cond ((and children (< (length children) 2))
		       (setf (byte-trie-branch trie) nil
			     (byte-trie-extension trie) nil))
		      (children
		       (remove-trie% (car stack) trie)
		       (return (values t t)))))))))


(defmethod delete-word ((word array) (trie byte-trie))
  "Delete word from trie. Returns two 
booleans: found and deleted."
  (let ((trie-stack)
	(foundp (walk-branch% trie)))
    (loop
       for byte across word
       for next = (funcall foundp byte)
       unless next
       do (loop-finish)
       do (push next trie-stack)
       finally (unless (or (byte-trie-leaf next)
			   (byte-trie-word next))
		 (return-from delete-word (values nil nil))))
    (when trie-stack
      (remove-from-stack% trie-stack))))
