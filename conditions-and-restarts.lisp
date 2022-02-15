(in-package stw.util)

(defmacro with-user-input (report question &body form)
  `(restart-case ,@form
     (get-user-input (value)
       :report ,report
       :interactive (lambda ()
		      (write-string ,question *query-io*)
		      (force-output *query-io*)
		      (list (read *query-io*)))
       value)))
