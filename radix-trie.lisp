(in-package util.trie)

(declaim (inline find-key
		 find-key%
		 insert-indexed-key
		 split-indexed-key
		 insert-word%
		 radix-trie-branch
		 radix-trie-leaf
		 radix-trie-word))
	 
		  
(defstruct radix-trie
  (word nil :type (or null string))
  (leaf nil)
  (branch (make-hash-table :test 'eq) :type hash-table)
  (subseq nil :type sequence))

;;;; searching

(defun find-key (word trie pos)
  (awhen (radix-trie-branch trie)
    (let ((node (gethash (char word pos) self)))
      (when node
	(let ((subseq (radix-trie-subseq node))) 
	  (if subseq
	      (aif (mismatch word subseq :start1 pos)
		   (unless (eql self pos)
		     (values node self))
		   node)
	      (values node (1+ pos))))))))


(defmethod find-word ((word string) (trie radix-trie))
  "Takes a search string and a radix trie, and when 
there is a result returns the values 1. leaf (values), 
2. matched word, and 3. last matching position, or 
nil if an exact match."
  (let (result)
    (declare (list result)
	     (inline find-word%))
    (labels ((find-word% (word trie &optional (pos 0))
	       (multiple-value-bind (next end-pos)
		   (find-key word trie pos)
		 (let ((self (when next
			       (radix-trie-word next))))
		   (cond ((and self
			       (null (mismatch word self)))
			  (setf result (list (radix-trie-leaf next) self nil)))
			 ((and next end-pos
			       (< end-pos (length word)))
			  (when (and self (= end-pos (length self)))
			    (setf result (list (radix-trie-leaf next) self end-pos)))
			  (find-word% word next end-pos)))))))
      (find-word% word trie))
    (when result
      (values-list result))))


;;;; inserting

(defun find-key% (word trie)
  (awhen (radix-trie-branch trie)
    (let ((node (gethash (char word 0) self)))
      (if node
	  (let ((subseq (radix-trie-subseq node))) 
	    (if subseq 
		(awhen (mismatch word subseq) 
		  (unless (eql self 0)
		    (values node trie subseq self)))
		(values node trie nil 1)))
	  (values nil trie)))))


(defun insert-indexed-key (trie key &optional (next-trie (make-radix-trie)))
  "Insert key into branch of next-trie."
  (setf (gethash (char key 0) (radix-trie-branch trie)) next-trie)
  (when (> (length key) 1)
      (setf (radix-trie-subseq next-trie) key))
  next-trie)


(defun split-indexed-key (prefix suffix next)
  (let ((new-node (copy-structure next)))
    (setf (radix-trie-subseq new-node) nil
	  (radix-trie-leaf next) nil
	  (radix-trie-word next) nil
	  (radix-trie-branch next) (make-hash-table :test 'eq)
	  (radix-trie-subseq next) (when (> (length prefix) 1)
				     prefix))
    (insert-indexed-key next suffix new-node)
    next))

(defmethod insert-word% (word (trie radix-trie))
  (let ((wordlength (length word)))
    (multiple-value-bind (next-trie cur-trie key end-pos)
	(find-key% word trie)
      (cond (end-pos
	     (if key
		 (let ((key-length (length key)))
		   (cond ((eql end-pos key-length)
			  ;; Matches key, but not the whole word; recurse.
			  (insert-word% (subseq word end-pos) next-trie))
			 ((< end-pos key-length) 
			  ;; Partial match of key.
			  (let ((new-node (split-indexed-key (subseq key 0 end-pos)
							     (subseq key end-pos key-length)
							     next-trie)))
			    (if (eql end-pos wordlength)
				;; End of whole word, split key, insert word.
				new-node
				;; Split key into prefix and suffix,
				;; insert both, and continue creating
				;; new branch at new-node - recurse.
				(insert-word% (subseq word end-pos) new-node))))))
		 (unless (eql wordlength end-pos)
		   (insert-word% (subseq word end-pos) next-trie))))
	    (next-trie
	     ;; exists already. Insert relevant meta
	     next-trie)
	    (t
	     ;; nothing there - insert key
	     (insert-indexed-key (or cur-trie trie) word))))))



(defmethod insert-word ((word string) (trie radix-trie) &optional (value word) case-sensitive)
  (declare (ignore case-sensitive))
  (awhen (insert-word% word trie)
    (setf (radix-trie-leaf self) value
	  (radix-trie-word self) word)
    trie))
