(defpackage util.macro
  (:use :cl)

  (:export
   :scase
   :awhile
   :aif
   :awhen
   :self
   :with-gensyms))


(defpackage util.binary
  (:use :cl)

  (:export
   :whitespacep
   :uppercasep
   :lowercasep
   :get-lowercase
   :get-uppercase
   :string-to-octets
   :octets-to-string))


(defpackage util.trie
  (:use :cl)

  (:import-from
   :util.binary
   :lowercasep
   :get-uppercase
   :get-lowercase
   :string-to-octets
   :octets-to-string)

  (:import-from
   :util.macro
   :aif
   :awhen
   :self)

  (:export

   ;; trie
   :trie
   :make-trie
   :next-character
   :find-word
   :find-in-string
   :insert-character
   :insert-word
   :delete-word
   :trie-branch
   :trie-leaf
   :walk-branch
   :walk-trie
   :walk-leaves
   :compress-edge

   ;; radix-trie
   :radix-trie
   :radix-trie-word
   :radix-trie-leaf
   :radix-trie-branch
   :radix-trie-subseq
   :make-radix-trie

   ;; byte-trie
   :byte-trie
   :byte-trie-word
   :byte-trie-leaf
   :byte-trie-branch
   :make-byte-trie))
