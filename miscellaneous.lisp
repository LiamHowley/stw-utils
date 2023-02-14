(in-package stw.util)

(declaim (ftype (function (keyword) symbol) keyword->symbol)
	 (ftype (function (string) symbol) string->symbol)
	 (ftype (function (&rest (or string symbol)) symbol) symbol-from)
	 (inline keyword->symbol
		 string->symbol
		 symbol-from))

(defun string->symbol (string)
  (declare (optimize (speed 3) (safety 0)))
  (intern (string-upcase string)))

(defun symbol-from (&rest rest)
  "REST accepts an arbitrary number of arguments 
of differing types, concatenates and interns them. 
Returns a symbol."
  (declare (optimize (speed 3) (safety 0)))
  (let ((stream (make-string-output-stream)))
    (loop
      for string in rest
      do (ensure-string string stream))
    (string->symbol (get-output-stream-string stream))))

(defun keyword->symbol (key)
  (declare (optimize (speed 3) (safety 0)))
  (string->symbol (symbol-name key)))
