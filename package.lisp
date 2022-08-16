(defpackage stw.util
  (:use :cl)
  (:export :scase
	   :aif
	   :awhen
	   :self
	   :with-gensyms

	   ;; control flow
	   :xor

	   ;; queue
	   :queue
	   :with-queue
	   :enqueue
	   :dequeue
	   :next-in-queue
	   :queue-contents
	   :queue-append
	   :queue-empty-p
	   
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
	   :optimize-edge

	   
	   ;; lists
	   :flatten
	   :reverse-flatten
	   :map-tree-depth-first
	   :map-tree-breadth-first
	   :mappend
	   :find-in-tree
	   :assoc-all
	   :ensure-list

	   ;; strings
	   :whitespacep
	   :newlinep
	   :empty-string/newline-p
	   :ensure-string
	   :concat-string
	   :match-tokens
	   :match-token
	   :find-all
	   :split-sequence
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
	   :alter-file-contents
	   :alter-directory-contents
	   :concatenate-files
	   :concatenate-and-remove-duplicates
	   :remove-duplicate-lines-from-file
	   :remove-duplicate-lines-from-string))
