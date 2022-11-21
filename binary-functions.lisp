(in-package util.binary)

(declaim (ftype (function ((unsigned-byte 8)) boolean)
		whitespacep
		uppercasep
		lowercasep)
	 (inline whitespacep
		 uppercasep
		 lowercasep))

(define-condition binary-op-error (simple-error)
  ((offender :initarg :offender :initform nil :reader offender)))

(defun binary-op-error (offender format-control &rest format-args)
  (error 'binary-op-error
	 :offender offender
	 :format-control format-control
	 :format-arguments format-args))

(defun whitespacep (byte)
  "Predicate that returns T if the byte represents a
whitespace or line-ending character."
  (declare (optimize (speed 3) (safety 0)))
  (or (= byte #.(char-code #\space))
      (= byte #.(char-code #\tab))
      (= byte #.(char-code #\newline))
      (= byte #.(char-code #\linefeed))
      (= byte #.(char-code #\return))))

(defun uppercasep (byte)
  "Predicate that returns T if the byte represents an
uppercase character."
  (declare (optimize (speed 3) (safety 0)))
  (and (<= byte #.(char-code #\Z))
       (>= byte #.(char-code #\A))))

(defun lowercasep (byte)
  "Predicate that returns T if the byte represents an
lowercase character."
  (declare (optimize (speed 3) (safety 0)))
  (and (<= byte #.(char-code #\z))
       (>= byte #.(char-code #\a))))



(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 8)) get-lowercase get-uppercase)
	 (inline get-lowercase get-uppercase))

(defun get-lowercase (byte)
  "If byte represents an uppercase character, return the byte
representing its lowercase corollary. If already lowercase or 
there is no applicable lowercase return the byte."
  (declare (optimize (speed 3) (safety 0)))
  (when (uppercasep byte)
    (- byte #.(- (char-code #\Z) (char-code #\z)))))

(defun get-uppercase (byte)
  "If byte represents a lowercase character, return the byte
representing its uppercase corollary. If already uppercase or
there is no applicable uppercase return the byte."
  (declare (optimize (speed 3) (safety 0)))
  (when (lowercase byte)
    (+ byte #.(- (char-code #\Z) (char-code #\z)))))



(declaim (ftype (function (character) fixnum) hex-char-to-decimal)
	 (inline hex-char-to-decimal))
	 
(defun hex-char-to-decimal (hex)
  "Convert hexadecimal character to its decimal equivalent. Output is a number."
  (declare (optimize (speed 3) (safety 0)))
  (let ((number (digit-char-p hex)))
    (the fixnum
	 (cond ((and number (< number 10))
		number)
	       (number
		(binary-op-error number
				 "~s is not a hex character. Must correspond to a positive integer less than 10"
				 number))
	       (t (ecase hex
		    ((#\a #\A) 10)
		    ((#\b #\B) 11)
		    ((#\c #\C) 12)
		    ((#\d #\D) 13)
		    ((#\e #\E) 14)
		    ((#\f #\F) 15)))))))


(declaim (ftype (function (string) simple-array) parse-hex-string)
	 (inline parse-hex-string))

(defun parse-hex-string (hex)
  "Parse a hexadecimal string to optional element-type. Defaults
to unsigned-byte 8."
  (declare (optimize (speed 3) (safety 0)))
  (make-array (/ (length hex) 2)
	      :element-type '(unsigned-byte 8)
	      :initial-contents 
	      (loop
		for i from 0 below (length hex)
		when (evenp i)
		collect (+ (* 16 (hex-char-to-decimal (char hex i)))
			   (hex-char-to-decimal (char hex (1+ i)))))))



(declaim (inline string-to-octets octets-to-string))

(defun string-to-octets
    (string &rest rest &key (external-format :default) (start 0) end &allow-other-keys)
  "Compatability function that first calls an implementations definition
before falling back on flexi-streams."
  (declare (ignore external-format start end))
  #+:sbcl
  (apply #'sb-ext:string-to-octets string rest)
  #+:cmucl
  (apply #'ext:string-to-octets string rest)
  #+:allegro
  (apply #'excl:string-to-octets string rest)
  #+:ccl
  (apply #'ccl:encode-string-to-octets string rest)
  #-(or :allegro :sbcl :cmucl :ccl)
  (apply #'flex:string-to-octets string rest))

(defun octets-to-string
    (octets &rest rest &key (external-format :default) (start 0) (end (length octets)) &allow-other-keys)
  "Compatability function that first calls an implementations definition
before falling back on flexi-streams."
  (declare (ignore external-format start end))
  #+:sbcl
  (apply #'sb-ext:octets-to-string octets rest)
  #+:cmucl
  (apply #'ext:octets-to-string octets rest)
  #+:allegro
  (apply #'excl:octets-to-string octets rest)
  #+:ccl
  (apply #'ccl:encode-octets-to-string octets rest)
  #-(or :allegro :sbcl :cmucl :ccl)
  (apply #'flex:octets-to-string octets rest))
