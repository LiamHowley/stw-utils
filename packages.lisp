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
   :get-lowercase)

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
   :insert-key
   :delete-word
   :trie-branch
   :trie-leaf
   :trie-word
   :walk-branch
   :walk-trie
   :merge-tries
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


(defpackage stw.util
  (:use
   :cl
   :util.trie
   :util.binary
   :util.macro)

  (:export

   ;; control flow
   :xor
   
   ;; lists
   :flatten
   :reverse-flatten
   :map-tree-depth-first
   :map-tree-breadth-first
   :mappend
   :find-in-tree
   :assoc-all
   :ensure-list
   :ordered-plist-values
   :number-range
   :set-difference%
   :array-to-list

   ;; lexer
   :*document*
   :*char-index*
   :*line-number*
   :*length* 
   :*decoder*
   :*encoder*
   :consume-whitespace
   :with-encoder
   :stw-read-char
   :next
   :prev
   :stw-peek-next-char
   :stw-peek-last-char
   :match-string
   :match-strings
   :match-character
   :consume-until
   :consume-while
   :read-until
   :decode
   :read-and-decode
   :read-and-encode

   ;; strings
   :whitespacep
   :newlinep
   :empty-string/newline-p
   :ensure-string
   :concat-string
   :match-tokens
   :match-token
   :find-all
   :explode-string
   :map-exploding-string
   :find-and-replace
   :make-displaced-array

   ;; numbers
   :get-float

   ;; files/directories/io
   :walk-directory
   :list-files
   :directory-p
   :file-p
   :rename-files
   :parse-stream
   :sequence-from-file
   :sequence-to-file
   :find-in-file
   :find-in-directory
   :alter-file-contents
   :alter-directory-contents
   :concatenate-files
   :concatenate-and-remove-duplicates
   :remove-duplicate-lines-from-file
   :remove-duplicate-lines-from-string

   ;; hash-tables
   :merge-hash-tables)

  (:export
   :scase
   :awhile
   :aif
   :awhen
   :self
   :with-gensyms)

  (:export
   :whitespacep
   :uppercasep
   :lowercasep
   :get-lowercase
   :get-uppercase
   :string-to-octets
   :octets-to-string)

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
   :trie-word
   :walk-branch
   :walk-trie
   :walk-leaves
   :merge-tries
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
   :make-byte-trie

   ;; miscellaneous
   :string->symbol
   :keyword->symbol
   :symbol-from))
