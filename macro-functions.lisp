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

