(in-package util.macro)

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

(defmacro awhile (condition &body body)
  "Anaphoric macro: while condition is true the result of
condition is bound to self, and available for capture."
  `(loop
      with self = nil
      while (setf self ,condition)
      do (progn
	   ,@body)))

(defmacro with-gensyms ((&rest bindings) &body body)
  `(let (,@(loop for binding in bindings
	      if (consp binding)
	      collect `(,(car binding) (gensym ,(cadr binding))) else
	      collect `(,binding (gensym))))
     ,@body))


(defmacro with-batch-loader ((vars params &optional message) &body body)
  "Batch load forms such as classes and functions. As 
the macro body uses a loop, vars can be any pattern that matches
the params, i.e. ITEM, (KEY VALUE), (KEY . VALUE). Params must be 
a list."
  `(macrolet ((loading ()
		            (when ,message
		              (princ ,message)
		              (terpri))
		            `(progn
		               ,@(loop
		                   for ,vars in ,params
		                   ,@(loop
			                     for form in (cdar body)
			                     collect (caar body)
			                     collect form)))))
     (loading)))



(defmacro iterate-extend (&body body)
  "Extends the macro iterate. Expands the macrolet FOR= into
(for <var> = <value>) expressions; the macrolet COLLECT-INTO into
(collect <value> into <list>) expressions; and the macrolet
COLLECT-ALL into (collect <value>) expressions."
  (with-gensyms (rest a b)
    `(macrolet ((for= (&rest ,rest)
                  `(progn
                     ,@(iter:iterate (iter:for (,a ,b) on ,rest by #'cddr)
                         (iter:collect `(iter:for ,,a = ,,b)))))
                (collect-into (&rest ,rest)
                  `(progn
                     ,@(iter:iterate (iter:for (,a ,b) on ,rest by #'cddr)
                         (iter:collect `(iter:collect ,,a into ,,b)))))
                (collect-all (&rest ,rest)
                  `(progn
                     ,@(iter:iterate (iter:for ,a in ,rest)
                         (iter:collect `(iter:collect ,,a))))))
       (iter:iterate
         ,@body))))
