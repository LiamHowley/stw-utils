(defsystem #:ctx-utils
    :description "A basic utility library."
    :serial t
    :components ((:file "package")
		 (:file "list-functions")
		 (:file "string-functions"))
    :in-order-to ((test-op (load-op :ctx-utils-test))))
