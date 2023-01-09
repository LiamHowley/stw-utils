(in-package stw.util)

(defun merge-hash-tables (&rest tables)
  "Gets value of records from the cdr of tables, and adds
them to the car of tables, should they not exist."
  (let ((table (car tables)))
    (loop
      for table% in (cdr tables)
      do (maphash #'(lambda (element class)
		      (unless (gethash element table)
			(setf (gethash element table) class)))
		  table%))
    table))
