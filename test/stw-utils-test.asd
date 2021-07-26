(defsystem #:stw-utils-test
    :description "Test library for stw-utils library."
    :depends-on ("stw-utils" "parachute")
    :serial t
    :components ((:file "package")
		 (:file "functions"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :stw.util.test)))
