(in-package ctx.util.test)


(defvar testlist '(:one (:two (:three (:four (:five :six) :seven) :eight) :nine) :ten))

(defvar alist '((:one . 1) (:two . 2) (:one . 1.0) (:three . 3) (:two . "two")))

(define-test tree-processing-functions
  :parent ctx-util
  (is equal (flatten testlist)
      '(:one :two :three :four :five :six :seven :eight :nine :ten))
  (is equal (reverse-flatten testlist)
      '(:ten :nine :eight :seven :six :five :four :three :two :one))
  (is equal (map-tree-depth-first #'symbol-name testlist)
      '("ONE" "TWO" "THREE" "FOUR" "FIVE" "SIX" "SEVEN" "EIGHT" "NINE" "TEN"))
  (is equal (map-tree-breadth-first #'symbol-name testlist)
      '("ONE" "TEN" "TWO" "NINE" "THREE" "EIGHT" "FOUR" "SEVEN" "FIVE" "SIX"))
  (true (find-in-tree '(:four (:five :six) :seven) testlist))
  (true (find-in-tree :four testlist))
  (false (find-in-tree :eleven testlist))
  (true (find-in-tree '(:four (:five :six) :seven) testlist))
  (is equal (mappend #'map-tree-depth-first #'list testlist)
      '(:one :two :three :four :five :six :seven :eight :nine :ten)))


(define-test list-processing-functions
  :parent ctx-util
  (is equal (assoc-all :two alist) '((:two . 2) (:two . "two")))
  (is equal (assoc-all 2 alist :key 'cdr) '((:two . 2)))
  (is equal (assoc-all 1 alist :key 'cdr :test #'equalp) '((:one . 1) (:one . 1.0))))
