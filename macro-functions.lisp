(in-package stw.util)

(defmacro scase (string &rest tests)
  "case for strings"
  `(cond  
     ,@(loop for (test result) in tests
	  if (eq test 't)
	  collect `(t ,result)
	  else
	  collect `((string= ,string ,test)
		    ,result))))
