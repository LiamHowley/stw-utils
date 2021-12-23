(in-package stw.util)

(defmacro scase (string &rest tests)
  "case for strings"
  `(cond  
     ,@(loop for option in tests
	  for test = (car option)
	  for result = (cdr option)
	  if (eq test 't)
	  collect `(t ,@(progn result))
	  else
	  collect (typecase test
		    (cons
		     `((or
			,@(loop for test% in test
			     collect `(string= ,string ,test%)))
		       ,@(progn result)))
		    (atom
		     `((string= ,string ,test)
		       ,@(progn result)))))))


(defmacro awhen (condition &body body)
  "Anaphoric macro: result of condition is bound to self
and available for capture."
  `(let ((self ,condition))
     (when self
       ,@body)))

(defmacro aif (condition then else)
  "Anaphoric macro: result of condition is bound to self
and available for capture."
  `(let ((self ,condition))
     (if self ,then ,else)))

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let (,@(loop for binding in bindings
	      if (consp binding)
	      collect `(,(car binding) (gensym ,(cadr binding))) else
	      collect `(,binding (gensym))))
     ,@body))
