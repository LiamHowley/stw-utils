(defsystem #:stw-utils
    :description "A basic utility library."
    :serial t
    :components ((:file "package")
		 (:file "macro-functions")
		 (:file "queue")
		 (:file "list-functions")
		 (:file "trie")
		 (:file "string-functions")
		 (:file "control-flow")
		 (:file "directories-and-files")
		 (:file "conditions-and-restarts"))
    :in-order-to ((test-op (load-op :stw-utils-test))))
