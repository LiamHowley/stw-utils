(in-package stw.util.test)

(defvar *document*)

(define-test matching-functions
  :parent stw-util
  (let* ((*document* "{\"accounts\" : [{\"id\" : 1, \"name\" : \"foo\"}]}")
	 (*char-index* 0)
	 (*length* (length *document*))
	 (find-accounts (match-string "accounts"))
	 (find-delimiter (match-character #\:)))
    (false (funcall find-accounts))
    (setf *char-index* 2)
    (is eql 2 (funcall find-accounts))
    (is eql (+ 2 (length "accounts")) *char-index*)
    (false (funcall find-delimiter (char *document* *char-index*)))
    (setf *char-index* 12)
    (is eql 12 (funcall find-delimiter (char *document* *char-index*)))))

(define-test basic-lexer
  :parent stw-util
  (let* ((*document* "{\"accounts\" : [{\"id\" : 1, \"name\" : \"foo\"}]}")
	 (*char-index* 0)
	 (*length* (length *document*)))
    (is char= #\{ (stw-read-char))
    (is char= #\" (stw-peek-next-char))
    (true (null (read-until (match-character #\{))))
    (is eql 1 (next))
    (is eql 1 *char-index*)
    (is char= #\{ (stw-peek-last-char))
    (next)
    (is string= "accounts" (read-until (match-character #\")))
    (next)
    (consume-while #'whitespacep)
    (is char= #\: (stw-read-char))
    (consume-until (match-character #\[ #\]))
    (is char= #\[ (stw-read-char))
    (consume-until (match-character #\{ #\}))
    (is char= #\{ (stw-read-char))
    (consume-until (match-character #\"))
    (is char= #\" (stw-read-char))
    (next)
    (is string= "id" (read-until (match-character #\" #\,)))
    (consume-until (match-character #\:))
    (next)
    (consume-while #'whitespacep)
    (is string= "1" (read-until (match-character #\" #\,)))
    (consume-until (match-character #\"))
    (next)
    (is string= "name" (read-until (match-character #\" #\,)))
    (consume-until (match-character #\:))
    (next)
    (consume-while #'whitespacep)
    (next)
    (is string= "foo" (read-until (match-character #\" #\,)))
    (consume-until (match-character #\{ #\}))
    (is char= #\} (stw-read-char))
    (consume-until (match-character #\[ #\]))
    (is char= #\] (stw-read-char))
    (consume-until (match-character #\{ #\}))
    (is char= #\} (stw-read-char))))


(defvar *special-entities*
  '((#\< . "&lt;")
    (#\> . "&gt;")
    (#\& . "&amp;")
    (#\" . "&quot;")
    (#\' . "&#39;")))

(define-test decoding
  :parent stw-util
  (let* ((*document* "Just &lt;a href=&quot;/url.com&quot; target=&#39;_blank&#39; class=&#39;class colour position&#39;&gt;")
	 (*char-index* 0)
	 (*length* (length *document*))
	 (*decoder* (let* ((trie (make-trie)))
		      (loop
			for (char . entity) in *special-entities*
			do (insert-word entity trie char))
		      (lambda (index)
			(let ((foundp (walk-branch trie)))
			  (loop
			    with result = nil
			    for i from index below *length*
			    for char = (aref *document* i)
			    for next = (funcall foundp char)
			    for value = (when next
					  (trie-leaf next))
			    when value
			      do (setf result value)
			    while next
			    finally (return (values result i))))))))
    (true (null (read-and-decode (match-character #\J))))
    (is string= "Just <" (read-and-decode (match-character #\a)))
    (is char= #\a (stw-read-char))
    (next 2)
    (is char= #\h (stw-read-char))
    (is string= "href=\"/url.com\"" (read-and-decode (match-character #\space)))
    (is char= #\space (stw-read-char))
    (next)
    (is string= "target='_blank' class='class colour" (read-and-decode (match-string "colour")))
    (is string= " position'>" (read-and-decode))
    (let ((*char-index* 0))
      (is string=
	  "Just <a href=\"/url.com\" target='_blank' class='class colour position'>"
	  (read-and-decode)))))


(define-test encoding
  :parent stw-util
  (with-encoder
      "Just <a href=\"/url.com\" target='_blank&#39; class='class colour position'>"
      #'(lambda (char)
	  (cdr (assoc char *special-entities* :test #'char=)))
    (let* ((*decoder* (let* ((trie (make-trie)))
			(loop
			  for (char . entity) in *special-entities*
			  do (insert-word entity trie char))
			(lambda (index)
			  (let ((foundp (walk-branch trie)))
			    (loop
			      with result = nil
			      for i from index below *length*
			      for char = (aref *document* i)
			      for next = (funcall foundp char)
			      for value = (when next
					    (trie-leaf next))
			      when value
				do (setf result value)
			      while next
			      finally (return (values result i))))))))
      (is string=
	  "Just &lt;"
	  (read-and-encode (match-character #\a)))
      (is char= #\a (stw-read-char))
      (next 2)
      (is char= #\h (stw-read-char))
      (is string=
	  "href=&quot;/url.com&quot;"
	  (read-and-encode (match-character #\space)))
      (is char= #\space (stw-read-char))
      (next)
      (is string=
	  "target=&#39;_blank&#39; class=&#39;class colour"
	  (read-and-encode (match-string "colour")))
      (is string=
	  " position&#39;&gt;"
	  (read-and-encode))
      (let ((*char-index* 0))
	(is string=
	    "Just &lt;a href=&quot;/url.com&quot; target=&#39;_blank&#39; class=&#39;class colour position&#39;&gt;"
	    (read-and-encode))))))
