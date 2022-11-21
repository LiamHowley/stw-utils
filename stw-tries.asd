(defsystem #:stw-tries
  :author "Liam Howley <liam.howley@thespanningtreeweb.ie"
  :description "A utility library of tries."
  :serial t
  :license "MIT"
  :components ((:file "tries-package")
	       (:file "macro-functions")
	       (:file "binary-functions")
	       (:file "trie")
	       (:file "radix-trie")
	       (:file "byte-trie"))
  :in-order-to ((test-op (load-op :stw-tries-test))))
