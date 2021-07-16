(in-package ctx.util)

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


(declaim (ftype (function (function list) list)
		map-tree-depth-first
		map-tree-breadth-first))

(defun map-tree-depth-first (fn list)
  "Depth first traversal. Requires function that takes a single argument. 
Returns flattened results."
  (declare (optimize (speed 3) (safety 0)))
  (labels ((walk (inner acc)
	     (cond ((null inner)
		    acc)
		   ((atom inner)
		    (cons (funcall fn inner) acc))
		   (t (walk (car inner) (walk (cdr inner) acc))))))
    (walk list nil)))

(defun map-tree-breadth-first (fn list)
  "Breadth first traversal. Requires a function that takes a single argument. Returns flattened results."
  (declare (optimize (speed 3) (safety 0)))
  (labels ((walk (inner acc)
	     (cond ((null inner)
		    acc)
		   ((atom inner)
		    (cons (funcall fn inner) acc))
		   ((atom (car inner))
		    (walk (car inner) (walk (cdr inner) acc)))
		   (t (walk (cdr inner) (walk (car inner) acc))))))
    (walk list nil)))


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
