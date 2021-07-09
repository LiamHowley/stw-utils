(defsystem #:ctx-utils
    :description "A basic utility library."
    :depends-on ("fare-memoization")
    :components ((:file "package")
		 (:file "functions")))
