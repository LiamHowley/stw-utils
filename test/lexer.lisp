(in-package stw.util.test)

(defvar *document*)

(define-test basic-lexer
  :parent stw-util
  (let* ((*document* "{\"accounts\" : [{\"id\" : 1, \"name\" : \"foo\"}]}")
	 (*char-index* 0)
	 (*length* (length *document*))
	 (next-quote (consume-until (match-character #\")))
	 (return-attr (read-until (match-character #\")))
	 (return-value (read-until (match-character #\" #\,)))
	 (consume-whitespace (consume-while #'whitespacep))
	 (array-tag (consume-until (match-character #\[ #\])))
	 (object-tag (consume-until (match-character #\{ #\})))
	 (match-delimiter (consume-until (match-character #\:))))
    (is char= #\{ (stw-read-char))
    (is char= #\" (stw-peek-next-char))
    (is eql 1 (next))
    (is eql 1 *char-index*)
    (is char= #\{ (stw-peek-last-char))
    (next)
    (is string= "accounts" (funcall return-attr))
    (next)
    (funcall consume-whitespace)
    (is char= #\: (stw-read-char))
    (funcall array-tag)
    (is char= #\[ (stw-read-char))
    (funcall object-tag)
    (is char= #\{ (stw-read-char))
    (funcall next-quote)
    (is char= #\" (stw-read-char))
    (next)
    (is string= "id" (funcall return-value))
    (funcall match-delimiter)
    (next)
    (funcall consume-whitespace)
    (is string= "1" (funcall return-value))
    (funcall next-quote)
    (next)
    (is string= "name" (funcall return-value))
    (funcall match-delimiter)
    (next)
    (funcall consume-whitespace)
    (next)
    (is string= "foo" (funcall return-value))
    (funcall object-tag)
    (is char= #\} (stw-read-char))
    (funcall array-tag)
    (is char= #\] (stw-read-char))
    (funcall object-tag)
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
			    finally (return (values (list index i) result)))))))
	 (decode-till-a (read-and-decode (match-character #\a)))
	 (decode-till-space (read-and-decode (match-character #\space)))
	 (decode-till-colour (read-and-decode (match-string "colour")))
	 (decode-till-end (read-and-decode)))
    (is string= "Just <" (funcall decode-till-a))
    (is char= #\a (stw-read-char))
    (next 2)
    (is char= #\h (stw-read-char))
    (is string= "href=\"/url.com\"" (funcall decode-till-space))
    (is char= #\space (stw-read-char))
    (next)
    (is string= "target='_blank' class='class colour" (funcall decode-till-colour))
    (is string= " position'>" (funcall decode-till-end))
    (let ((*char-index* 0))
      (is string=
	  "Just <a href=\"/url.com\" target='_blank' class='class colour position'>"
	  (funcall decode-till-end)))))


(define-test encoding
  :parent stw-util
(with-encoder
  "Just <a href=\"/url.com\" target='_blank' class='class colour position'>"
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
			    finally (return (values (list index i) result)))))))
	 (reader (read-and-encode)))
    (is string=
	"Just &lt;"
	(funcall reader (match-character #\a)))
    (is char= #\a (stw-read-char))
    (next 2)
    (is char= #\h (stw-read-char))
    (is string=
	"href=&quot;/url.com&quot;"
	(funcall reader (match-character #\space)))
    (is char= #\space (stw-read-char))
    (next)
    (is string=
	"target=&#39;_blank&#39; class=&#39;class colour"
	(funcall reader (match-string "colour")))
    (is string=
	" position&#39;&gt;"
	(funcall reader))
    (let ((*char-index* 0))
      (is string=
	  "Just &lt;a href=&quot;/url.com&quot; target=&#39;_blank&#39; class=&#39;class colour position&#39;&gt;"
	  (funcall reader))))))
