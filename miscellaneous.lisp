(in-package stw.util)

(declaim (ftype (function (keyword) symbol) keyword->symbol)
	 (ftype (function (string) symbol) string->symbol)
	 (inline keyword->symbol
		 string->symbol))

(defun string->symbol (string)
  (declare (optimize (speed 3) (safety 0)))
  (intern (string-upcase string)))

(defun keyword->symbol (key)
  (declare (optimize (speed 3) (safety 0)))
  (string->symbol (symbol-name key)))
