(in-package stw.util)

(declaim (inline queue
		 enqueue
		 dequeue
		 next-in-queue
		 queue-contents
		 queue-append
		 queue-empty-p))
	
(defun queue (&rest initial-contents)
  "Simple FIFO queue. Derivative of PAIP, page 342.
Accepts initial contents to be enqueued.
Returns a closure that accept the following actions
:CONTENTS :PUSH :POP :NEXT :EMPTYP :APPEND, and an 
optional item to either append or enqueue."
  (let ((q (cons nil initial-contents)))
    (declare (cons q))
    (setf (car q) (last q))
    #'(lambda (action &optional item)
	(declare (keyword action)
		 (optimize (speed 3)(safety 0)))
	(case action
	  (:contents (cdr q))
	  (:push (setf (car q)
		       (setf (cdar q)
			     (cons item nil))))
	  (:pop (prog1
		    (pop (cdr q))
		  (when (null (cdr q))
		    (setf (car q) q))))
	  (:next (cadr q))
	  (:emptyp (null (cdr q)))
	  (:append (setf (car q)
			 (setf (cdar q)
			       item)))))))


(defmacro with-queue (&body body)
  "Encapsulates a queue. Anaphora of the flet queue:
i.e. within the body call (queue :pop) or 
(queue :push item) etc."
  (with-gensyms (q action item)
    `(let ((,q (queue)))
       (declare (inline queue))
       (flet ((queue (,action &optional ,item)
	       (funcall ,q ,action ,item)))
	 ,@body))))


(defun enqueue (q item)
  "Enqueue an item."
  (funcall q :push item))

(defun dequeue (q)
  "Removes next item from queue
and returns the item."
  (funcall q :pop))

(defun queue-contents (q)
  "Retrieve the contents of a queue."
  (funcall q :contents))

(defun next-in-queue (q)
  "Retrieves the next item in a
queue, without removing it."
  (funcall q :next))

(defun queue-append (q list)
  "Append list to queue."
  (funcall q :append list))

(defun queue-empty-p (q)
  "T if empty. Otherwise..."
  (funcall q :emptyp))
