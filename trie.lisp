(in-package stw.util)

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


(defun next-key (key trie &optional (test #'char=))
  "Returns two values: the cons of the next node, and the branch, or nil"
  (awhen (trie-branch trie)
    (values (cdr (assoc key self :test (the function test))) self)))


(defun walk-branch (trie &optional (fn #'next-key) (test #'char=))
  "Returns a function, that accepts a key as argument and encapsulates 
and updates the current working trie."
  (let ((working-trie trie))
    #'(lambda (key)
	(awhen (funcall fn key working-trie test)
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


(defun insert-key (trie key &optional (next-trie (make-trie)))
  "Insert key into branch of next-trie."
  (setf (trie-branch trie)
	(acons key next-trie (trie-branch trie)))
  next-trie)

(defun insert-character (char trie &optional (test #'char=))
  "Insert character into branch of trie."
  (declare (special case-sensitive)
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
		  (assoc alternate branch :test test))
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



(declaim (ftype (function (function trie &optional boolean) list) walk-trie))

(defgeneric walk-trie (fn trie &optional case-sensitive)
  (:documentation "Depth first traversal that returns a flattened list."))

(defmethod walk-trie (fn (trie trie) &optional case-sensitive)
  (declare (optimize (speed 3) (safety 0)))
  (let ((char))
    (labels ((walk (next acc)
	       (typecase next
		 (cons
		  ;; in trie-branch
		  (cond ((and case-sensitive (typep (car next) 'character))
			 (setf char (car next))
			 (walk (cdr next) acc))
			((and (typep (car next) 'base-char)
			      (char= (car next) (char-downcase (the base-char (car next)))))
			 (setf char (car next))
			 (walk (cdr next) acc))
			((typep (car next) 'character)
			 acc)
			(t
			 (walk (cdr next) (walk (car next) acc)))))
		 (trie
		  (let ((result (funcall fn char next)))
		    (walk (trie-branch next)
			  (aif result
			       (cons self acc)
			       acc))))
		 (t
		  acc))))
      (walk trie nil))))


(defun walk-leaves (trie)
  "Recursively walks trie and accumulates
outer leaves."
  (walk-trie #'(lambda (char trie)
		 (declare (ignore char))
		 (or (trie-leaf trie)
		     (trie-word trie)))
	     trie))


(defmethod optimize-edge ((trie trie))
  "Compress edge of trie, when there 
are no other keys."
  (walk-trie
   #'(lambda (key next)
       (declare (ignore key))
       (let ((edge-nodes
	      (walk-trie #'(lambda (char trie)
			     (declare (ignore char))
			     (when (or (trie-leaf trie)
				       (trie-word trie))
			       trie))
			 next)))
	 (when (eql (length edge-nodes) 1)
	   (let ((node (car edge-nodes)))
	     (setf (trie-word next) (trie-word node)
		   (trie-leaf next) (trie-leaf node)
		   (trie-branch next) nil))))
       (values))
   trie)
  trie)
