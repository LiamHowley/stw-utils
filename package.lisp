(defpackage stw.util
  (:use :cl)
  (:export :scase

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
	   :match-token
	   :find-all
	   :split-sequence
	   :find-and-replace

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
