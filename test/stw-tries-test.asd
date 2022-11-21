(defsystem #:stw-tries-test
    :description "Test library for stw-trie library."
    :depends-on ("stw-tries" "parachute")
    :serial t
    :components ((:file "tries-package")
		 (:file "tries"))
    :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :trie.test)))
