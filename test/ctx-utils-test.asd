(defsystem #:ctx-utils
    :description "A basic utility library."
    :depends-on ("ctx-utils"
		 "parachute")
    :serial t
    :components ((:file "package")
		 (:file "functions"))
    :in-order-to ((test-op (load-op #:ctx-utils-test))))
