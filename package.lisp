(defpackage ctx.util
  (:use :cl)
  (:export :flatten
	   :reverse-flatten
	   :map-tree-depth-first
	   :map-tree-breadth-first
	   :mappend
	   :find-in-tree
	   :assoc-all
	   :ensure-list))
