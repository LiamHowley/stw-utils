(defsystem #:stw-utils
    :description "A basic utility library."
    :serial t
    :components ((:file "package")
		 (:file "macro-functions")
		 (:file "list-functions")
		 (:file "trie")
		 (:file "lexer")
		 (:file "radix-trie")
		 (:file "string-functions")
		 (:file "control-flow")
		 (:file "directories-and-files"))
    :in-order-to ((test-op (load-op :stw-utils-test))))
