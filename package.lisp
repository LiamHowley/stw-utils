(defpackage stw.util
  (:use :cl)
  (:export
   :scase
   :awhile
   :aif
   :awhen
   :self
   :with-gensyms

   ;; control flow
   :xor

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

   ;;radix-trie
   :radix-trie
   :radix-trie-word
   :radix-trie-leaf
   :radix-trie-branch
   :radix-trie-subseq
   
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
   :*consume-whitespace*
   :stw-read-char
   :next
   :stw-peek-next-char
   :stw-peek-last-char
   :match-string
   :match-character
   :consume-until
   :consume-while
   :read-until
   :read-and-decode

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

   ;; files/directories/io
   :walk-directory
   :list-files
   :directory-p
   :file-p
   :rename-files
   :sequence-from-file
   :sequence-to-file
   :find-in-file
   :find-in-directory
   :alter-file-contents
   :alter-directory-contents
   :concatenate-files
   :concatenate-and-remove-duplicates
   :remove-duplicate-lines-from-file
   :remove-duplicate-lines-from-string))
