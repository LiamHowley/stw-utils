(defsystem #:stw-utils
  :description "A basic utility library."
  :serial t
  :depends-on ("babel")
  :components ((:file "packages")
	       (:file "macro-functions")
	       (:file "hashtable-functions")
	       (:file "list-functions")
	       (:file "binary-functions")
	       (:file "trie")
	       (:file "radix-trie")
	       (:file "byte-trie")
	       (:file "string-functions")
	       (:file "number-functions")
	       (:file "lexer")
	       (:file "control-flow")
	       (:file "directories-and-files"))
  :in-order-to ((test-op (load-op :stw-utils-test))))
