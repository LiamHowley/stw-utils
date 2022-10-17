(in-package stw.util)

(declaim (inline find-word%
		 find-key
		 find-key%
		 insert-indexed-key
		 remove-indexed-key
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


(defun find-word% (word trie &optional (pos 0))
  (multiple-value-bind (next end-pos)
      (find-key word trie pos)
    (let ((self (and next (radix-trie-leaf next))))
      (cond ((and self
		  (null (mismatch word self)))
	     self)
	    ((and next end-pos
		  (< end-pos (length word)))
	     (find-word% word next end-pos))
	    ((and end-pos (> end-pos 0))
	     (values nil end-pos))))))

(defmethod find-word (word (trie radix-trie))
  (find-word% word trie))


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

(defun remove-indexed-key (trie key &optional (test #'string=))
  "Remove key from trie."
  (let ((trie-branch (radix-trie-branch trie))
	(char (char key 0)))
    (awhen (gethash char trie-branch)
      (when (funcall test (radix-trie-subseq self) key)
	(remhash (char key 0) trie-branch)))))

(defun split-indexed-key (trie key prefix suffix next)
  (let ((new-node (make-radix-trie)))
    (insert-indexed-key trie prefix new-node)
    (insert-indexed-key new-node suffix next)
    (remove-indexed-key trie key #'string=)
    new-node))


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
			  (let ((new-node (split-indexed-key cur-trie key
							     (subseq key 0 end-pos)
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



(defmethod insert-word (word (trie radix-trie) &optional (value word) case-sensitive)
  (declare (ignore case-sensitive))
  (awhen (insert-word% word trie)
    (setf (radix-trie-leaf self) value
	  (radix-trie-word self) word)
    trie))
