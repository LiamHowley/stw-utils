(in-package util.trie)

(declaim (inline next-key
		 walk-branch
		 find-in-string
		 insert-key
		 insert-character
		 delete-word
		 remove-key
		 remove-trie
		 remove-from-stack
		 trie-leaf
		 trie-branch
		 trie-word))


(defstruct trie
  (word nil :type (or null string))
  (leaf nil)
  (branch nil :type list))

(defmethod print-object ((object trie) stream)
  (print-unreadable-object (object stream :type t :identity t)))


(declaim (ftype (function (character trie) (values (or null trie) list)) next-key))

(defun next-key (key trie)
  "Returns two values: the cons of the next node, and the branch, or nil"
  (declare (optimize (speed 3)(safety 0)))
  (awhen (trie-branch trie)
    (values (cdr (assoc key self :test #'char=)) self)))


(declaim (ftype (function (trie &optional function) function) walk-branch))

(defun walk-branch (trie &optional (fn #'next-key))
  "Returns a function, that accepts a key as argument and encapsulates 
and updates the current working trie."
  (declare (optimize (speed 3)(safety 0)))
  (let ((working-trie trie))
    #'(lambda (key)
	(declare (optimize (speed 3)(safety 0))
		 (character key))
	(awhen (funcall fn key working-trie)
	  (setf working-trie self)))))


(defmethod find-word (word (trie trie))
  "Finds word in trie."
  (let ((foundp (walk-branch trie)))
    (loop
       for char across word
       for next = (funcall foundp char)
       unless next
       do (return)
       finally (return
		 (awhen (trie-leaf next)
		   self)))))


(defun find-in-string (string trie &key (start 0) (end (1- (length string))))
  "Walk string and search in trie for matches."
  (let ((result)
	(foundp (walk-branch trie)))
    (declare (function foundp)
	     (optimize (speed 3) (safety 0)))
    (loop
       for i of-type fixnum from start to end
       for char = (the character (aref string i))
       for next = (funcall foundp char)
       when next
       do (setf result (trie-leaf next))
       unless next
       do (return))
    result))


(declaim (ftype (function (trie character &optional trie) trie) insert-key))

(defun insert-key (trie key &optional (next-trie (make-trie)))
  "Insert key into branch of next-trie."
  (declare (optimize (speed 3) (safety 0)))
  (setf (trie-branch trie)
	(acons key next-trie (trie-branch trie)))
  next-trie)


(declaim (ftype (function (character trie) (values (or null trie) list)) insert-character))

(defun insert-character (char trie)
  "Insert character into branch of trie."
  (declare (optimize (speed 3) (safety 0))
	   (special case-sensitive)
	   (inline next-key))
  (let ((alternate
	 (unless case-sensitive
	   (if (char= char (char-downcase char))
	       (char-upcase char)
	       (char-downcase char)))))
    (multiple-value-bind (next branch)
	(next-key char trie)
      (cond ((and next branch)
	     nil)
	    (branch
	     (setf next (insert-key trie char)))
	    (t
	     (setf next (insert-key trie char)
		   branch (trie-branch trie))))
      (unless (or case-sensitive
		  (char= char alternate)
		  (assoc alternate branch :test #'char=))
	(insert-key trie alternate next))
      (values next branch))))


(defmethod insert-word (word trie &optional (value word) (case-sensitive t))
  "Insert word into trie"
  (declare (special case-sensitive))
  (let ((foundp (walk-branch trie #'insert-character)))
    (loop
       for char across word
       for next = (funcall foundp char)
       finally (setf (trie-leaf next) value
		     (trie-word next) word))
    trie))


(defun remove-key (trie key &optional (test #'char=))
  "Remove key from trie."
  (setf (trie-branch trie)
	(remove key (trie-branch trie) :key #'car :test test)
	(trie-leaf trie) nil
	(trie-word trie) nil))

(defun remove-trie (trie trie%)
  "Remove trie from branch."
  (setf (trie-branch trie)
	(remove trie% (trie-branch trie) :key 'cdr :test #'eq)
	(trie-leaf trie) nil
	(trie-word trie) nil))
  
(defun remove-from-stack (stack)
  "Remove trie from trie stack. Helper function 
for DELETE-WORD."
  (let ((initial-trie (car stack)))
    (if (trie-branch initial-trie)
	;; Word is a prefix to another word.
	(values t
		(setf (trie-word initial-trie) nil
		      (trie-leaf initial-trie) nil))
	(loop
	   while stack
	   do (let* ((trie (pop stack))
		     (children (trie-branch trie)))
		(cond ((and children (< (length children) 2))
		       (setf (trie-branch trie) nil))
		      (children
		       (remove-trie (car stack) trie)
		       (return (values t t)))))))))


(defmethod delete-word (word trie)
  "Delete word from trie. Returns two 
booleans: found and deleted."
  (let ((trie-stack)
	(foundp (walk-branch trie)))
    (loop
       for char across word
       for next = (funcall foundp char)
       unless next
       do (loop-finish)
       do (push next trie-stack)
       finally (unless (or (trie-leaf next)
			   (trie-word next))
		 (return-from delete-word (values nil nil))))
    (when trie-stack
      (remove-from-stack trie-stack))))



(declaim (ftype (function (trie &optional function) list) walk-trie))

(defgeneric walk-trie (trie &optional function)
  (:documentation "Depth first traversal that returns a flattened list."))

(defmethod walk-trie ((trie trie) &optional (fn #'identity))
  (declare (optimize (speed 3) (safety 0)))
    (labels ((walk (next acc)
	       (typecase next
		 (cons
		  (cond ((typep (car next) 'character)
			 (walk (cdr next) acc))
			((typep (car next) 'character)
			 acc)
			(t
			 (walk (cdr next) (walk (car next) acc)))))
		 (trie
		    (walk (trie-branch next)
			  (aif (funcall (the function fn) next)
			       (cons self acc)
			       acc)))
		 (t
		  acc))))
      (walk trie nil)))


(defun walk-leaves (trie &optional (fn #'identity))
  "Recursively walks trie and accumulates
outer leaves."
  (walk-trie
   trie
   #'(lambda (trie)
       (when (or (trie-leaf trie)
		 (trie-word trie))
	 (funcall fn trie)))))


(defun merge-tries (&rest tries)
  (let ((the-trie (car tries)))
    (loop
      for trie in (cdr tries)
      do (walk-leaves
	  trie
	  #'(lambda (trie%)
	      (print (trie-word trie%))
	      (insert-word (trie-word trie%) the-trie (trie-leaf trie%)))))
    the-trie))


(defmethod compress-edge ((trie trie))
  "Compress edge of trie, when there 
are no other keys."
  (walk-trie
   trie
   #'(lambda (next)
       (let ((edge-nodes
	       (walk-trie #'(lambda (trie)
			      (when (or (trie-leaf trie)
					(trie-word trie))
				trie))
			  next)))
	 (when (eql (length edge-nodes) 1)
	   (let ((node (car edge-nodes)))
	     (setf (trie-word next) (trie-word node)
		   (trie-leaf next) (trie-leaf node)
		   (trie-branch next) nil))))
       (values)))
  trie)
