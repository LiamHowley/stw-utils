(in-package stw.util)

(declaim (ftype (function (list) list)
		flatten
		reverse-flatten))

(defun flatten (list)
  "Depth first traversal that returns a flattened list."
  (declare (optimize (speed 3) (safety 0)))
  (labels ((walk (inner acc)
	     (cond ((null inner)
		    acc)
		   ((atom inner)
		    (cons inner acc))
		   (t (walk (car inner) (walk (cdr inner) acc))))))
    (walk list nil)))

(defun reverse-flatten (list)
  "Depth first traversal that returns a reversed and flattened list."
  (declare (optimize (speed 3) (safety 0)))
  (labels ((walk (inner acc)
	     (cond ((null inner)
		    acc)
		   ((atom inner)
		    (cons inner acc))
		   (t (walk (cdr inner) (walk (car inner) acc))))))
    (walk list nil)))


(declaim (ftype (function (function list &optional boolean) list) map-tree-depth-first))

(defun map-tree-depth-first (fn list &optional from-end)
  "Depth first traversal. Requires function that takes a single argument. 
Returns flattened results."
  (declare (optimize (speed 3) (safety 0)))
  (let ((result
	  (labels ((walk (inner acc)
		     (cond ((null inner)
			    acc)
			   ((atom inner)
			    (let ((result (funcall (the function fn) inner)))
			      (cond ((and result (eq result t))
				     (cons inner acc))
				    (result
				     (cons result acc))
				    (t acc))))
			   (t (if from-end
				  (walk (car inner) (walk (cdr inner) acc))
				  (walk (cdr inner) (walk (car inner) acc)))))))
	    (walk list nil))))
    (if from-end
	result
	(nreverse result))))


(declaim (ftype (function (function list) list) map-tree-breadth-first))

(defun map-tree-breadth-first (fn list)
  "Breadth first traversal. Requires a function that takes a single argument. 
Returns flattened results."
  (declare (optimize (speed 3) (safety 0)))
  (let ((values))
    (with-queue
      (flet ((walk (inner)
	       (loop for item in inner
		  if (atom item)
		  do (let ((result (funcall (the function fn) item)))
		       (cond ((and result (eq result t))
			      (push item values))
			     (result
			      (push result values))))
		  else do (queue :push item))))
	(walk list)
	(loop
	   for child = (queue :pop)
	   if child
	   do (walk child)
	   else do (return (nreverse values)))))))



(defun find-in-tree (value tree &key (test #'equal))
  "Recursively walks through a TREE testing VALUE against
each node. Once equality is ascertained, the (sub)list/atom containing VALUE
is returned."
  (declare (optimize (speed 3) (safety 0)))
  (labels ((walk (node acc)
	     (if (null node)
		 nil
		 (walk (cdr node)
		       (if (funcall test (car node) value)
			   (return-from find-in-tree node)
			   (if (consp (car node))
			       (walk (car node) acc)
			       (cons (car node) acc)))))))
    (walk tree nil)))


(defun assoc-all (item alist &key (key 'car) (test #'eql))
  "Returns all values in ALIST of key ITEM. In keeping with convention
the default key is #'car and default test is #'eql." 
  (declare (optimize (speed 3) (safety 0)))
  (loop for (k . v) in alist
     if (and (eq key 'car) (funcall test item k))
     collect (cons k v)
     else if (and (eq key 'cdr) (funcall test item v))
     collect (cons k v)))


(declaim (ftype (function (function function list) list) mappend))

(defun mappend (traverse-func map-fn list)
  "Traverses a list/tree using traversal function. 
Maps each leaf with mapping function and appends the results."
  (the list (apply #'append (funcall traverse-func map-fn list))))


(defun ensure-list (item)
  "Return list of item."
  (declare (optimize (speed 3) (safety 0)))
  (if (listp item) item (list item)))


(defun ordered-plist-values (plist &rest ordered-keys)
  "Returns list ordered according to ordered-keys."
  (loop
     for key in ordered-keys
     for value = (getf plist key)
     when value
     collect value))


(defun number-range (min max)
  "Returns list of numbers between min and max."
  (loop for num from min to max
	collect num))


(defun set-difference% (list1 list2 &key (test #'eql))
  "Set-difference for basic lists that returns the first
list and maintains its order."
  (loop
    for item in list1
    unless (member item list2 :test test)
      collect item))


(defun array-to-list (array)
  (map 'list
       #'(lambda (item)
	   (typecase item
	     ((array cons)
	      (array-to-list item))
	     (t item)))
       array))
