(defsystem #:stw-utils
    :description "A basic utility library."
    :serial t
    :components ((:file "package")
		 (:file "list-functions")
		 (:file "string-functions")
		 (:file "directories-and-files"))
    :in-order-to ((test-op (load-op :stw-utils-test))))
