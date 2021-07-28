(defpackage stw.util
  (:use :cl)
  (:export :flatten
	   :reverse-flatten
	   :map-tree-depth-first
	   :map-tree-breadth-first
	   :mappend
	   :find-in-tree
	   :assoc-all
	   :ensure-list

	   ;; strings
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
	   :alter-file-contents))
