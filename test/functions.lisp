(in-package stw.util.test)


(defvar testlist1 '(:one (:two (:three (:four (:five :six) :seven) :eight) :nine) :ten))

(defvar testlist2 '(1 (2 (4 (8 9) 5 (10 11)) 3 (6 (12 13) 7 (14 15)))))

(defvar alist '((:one . 1) (:two . 2) (:one . 1.0) (:three . 3) (:two . "two")))

(defvar testpath-list
  '((:a ((:one 1)
	 (:two 2)
	 (:three ((:x . "x")
		  (:y . "y")
		  (:z . "z")))))
    (:b ((:one 1)
	 (:two 2)
	 (:three ((:x "x")
		  (:y "y")
		  (:z "z")))))
    (:a ((:one 1)
	 (:two ((:x "1")
		(:y "2")
		(:z "3")))
	 (:three 3)))
    (:a ((:one 1)
	 (:two 2)
	 (:three ((:x 1)
		  (:y 1)
		  (:z 1)))))))

(define-test tree-processing-functions
  :parent stw-util
  (is equal (flatten testlist1)
      '(:one :two :three :four :five :six :seven :eight :nine :ten))
  (is equal (reverse-flatten testlist1)
      '(:ten :nine :eight :seven :six :five :four :three :two :one))
  (is equal (map-tree-depth-first #'oddp testlist2)
      '(1 9 5 11 3 13 7 15))
  (is equal (map-tree-breadth-first #'oddp testlist2)
      '(1 3 5 7 9 11 13 15))
  (is equal (map-tree-breadth-first #'identity testlist2)
      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
  (true (find-in-tree '(:four (:five :six) :seven) testlist1))
  (true (find-in-tree :four testlist1))
  (false (find-in-tree #\e testlist2))
  (true (find-in-tree '(:four (:five :six) :seven) testlist1))
  (is equal (mappend #'map-tree-depth-first #'list testlist1)
      '(:one :two :three :four :five :six :seven :eight :nine :ten))
  (is equal '("x" 1) (map-tree-path testpath-list '(:a :three :x)))
  (is equal '(1) (map-tree-path testpath-list '(:a :three :x) :map #'numberp)))

(define-test list-processing-functions
  :parent stw-util
  (is equal (assoc-all :two alist) '((:two . 2) (:two . "two")))
  (is equal (assoc-all 2 alist :key 'cdr) '((:two . 2)))
  (is equal (assoc-all 1 alist :key 'cdr :test #'equalp) '((:one . 1) (:one . 1.0)))
  (true (dotted-p '(1 . 2)))
  (false (dotted-p '(1 2)))
  (false (dotted-p '(1 2 3 4)))
  (false (dotted-p '(1 2 3 . 4)))
  (true (dotted-list-p '(1 2 3 . 4)))
  (false (dotted-list-p '(1 2 3 4)))
  (true (dotted-alist-p '((1 . 2)(2 . 3))))
  (true (dotted-alist-p '((1 . 2)(2 . 3))))
  (true (dotted-alist-p '((1 . 2)(2 . 3))))
  (false (dotted-alist-p '((1 2)(2 3))))
  (false (dotted-alist-p '((1 . 2)(2 3))))
  (false (dotted-alist-p '((1 . 2)(2 3 4 . 5))))
  (false (dotted-alist-p '((1 . 2)(2 3 4 5))))
  (false (dotted-alist-p '(1 2 3 4 5))))



(defparameter lorem "Lorem ipsum dolor sit amet")

(define-test string-functions
  :parent stw-util
  (of-type string (ensure-string "test"))
  (of-type string (ensure-string 1))
  (of-type string (ensure-string #\a))
  (of-type string (ensure-string `(a b c d)))
  (of-type string (ensure-string '(#\a #\b #\c #\d)))
  (of-type string (ensure-string '(1 2 3 4)))
  (of-type string (ensure-string (make-array 4 :element-type 'character :initial-contents '(#\a #\b #\c #\d))))
  (of-type string (ensure-string (make-array 4 :element-type 'symbol :initial-contents `(a b c d))))
  (of-type string (ensure-string (make-array 4 :element-type 'real :initial-contents '(1 2 3 4))))
  (is equal
      (concat-string '("this" "is" "a" "string") t)
      "this is a string")
  (is equal
      (concat-string (make-array 4 :element-type 'string :initial-contents '("this" "is" "a" "string")) t)
      "this is a string")
  (is equal (concat-string '(#\a #\b #\c #\d)) "abcd")

  ;; explode-string
  (is equal '("L" "or" "e" "m " "ip" "sum " "do" "l" "or" " sit " "am" "e" "t")
      (explode-string lorem '(#\e "ip" "do" #\l "or" "am")))
  (is equal '("Lorem" "ipsum" "dolor" "sit" "amet")
      (explode-string lorem '(#\space) :remove-separators t))
  (is equal '("ip" "s" "um")
      (explode-string lorem '(#\s) :start 6 :end 11))
  (is equal '("ip" "s" "um")
      (explode-string lorem '(#\s) :start 6 :end-test #'(lambda (char)
								(char= char #\space))))

  ;; find-and-replace
  (multiple-value-bind (altered-seq result)
      (find-and-replace lorem '(("ipsum" . "gypsum") (#\o . #\0) ("amet" . "amen")))
    (is equal altered-seq "L0rem gypsum d0l0r sit amen")
    (is equal
	(sort result #'string-lessp :key #'car)
	(sort '(("ipsum" . "gypsum") (#\o . #\0) ("amet" . "amen")) #'string-lessp :key #'car)))

  (multiple-value-bind (altered-seq result)
    (find-and-replace lorem '(("ipsum" . "gypsum") (#\o . #\0) ("amet" . "amen"))
		      :end-test #'(lambda (char)
				    (char= char #\space)))
    (is equal altered-seq "L0rem ipsum dolor sit amet")
    (is equal '((#\o . #\0)) result))

  ;; find-all
  (is equal (find-all lorem '("em" "um" #\i "et")) '((3 5) (6 7) (9 11) (19 20) (24 26))))


(defparameter *xor1* :foo)
(defparameter *xor2* :bar)
(defparameter *xor3* nil)
(defparameter *xor4* nil)
(defparameter *xor5* nil)

(define-test control-flow-functions
  :parent stw-util

  (multiple-value-bind (result xor)
      (xor *xor1* *xor2* *xor3*)
    (is eq result nil)
    (is eq xor nil))

  (multiple-value-bind (result xor)
      (xor *xor3* *xor4* *xor5*)
    (is eq result nil)
    (is eq xor t))

  (multiple-value-bind (result xor)
      (xor *xor1* *xor3* *xor4*)
    (is eq result :foo)
    (is eq xor t))

  (multiple-value-bind (result xor)
      (xor *xor3* *xor2* *xor4*)
    (is eq result :bar)
    (is eq xor t)))
