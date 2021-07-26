(in-package stw.util.test)


(defvar testlist1 '(:one (:two (:three (:four (:five :six) :seven) :eight) :nine) :ten))

(defvar testlist2 '(1 (2 (4 (8 9) 5 (10 11)) 3 (6 (12 13) 7 (14 15)))))

(defvar alist '((:one . 1) (:two . 2) (:one . 1.0) (:three . 3) (:two . "two")))

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
      '(:one :two :three :four :five :six :seven :eight :nine :ten)))


(define-test list-processing-functions
  :parent stw-util
  (is equal (assoc-all :two alist) '((:two . 2) (:two . "two")))
  (is equal (assoc-all 2 alist :key 'cdr) '((:two . 2)))
  (is equal (assoc-all 1 alist :key 'cdr :test #'equalp) '((:one . 1) (:one . 1.0))))



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
  (is equal (split-sequence lorem #\e "ip" "do" #\l "or" "am")
      '("L" "or" "e" "m " "ip" "sum " "do" "l" "or" " sit " "am" "e" "t"))
  (is equal (find-and-replace lorem '(("ipsum" . "gypsum") (#\o . #\0) ("amet" . "amen"))) "L0rem gypsum d0l0r sit amen")
  (is equal (find-all lorem "em" "um" #\i "et") '((3 5) (6 7) (9 11) (19 20) (24 26))))
