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

	   :ensure-string
	   :concat-string
	   :match-token
	   :find-all
	   :split-sequence
	   :find-and-replace))
