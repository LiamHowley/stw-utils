(defsystem #:ctx-utils-test
    :description "Test library for ctx-utils library."
    :depends-on ("ctx-utils" "parachute")
    :serial t
    :components ((:file "package")
		 (:file "functions"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :ctx.util.test)))
