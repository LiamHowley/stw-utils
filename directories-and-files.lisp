(in-package stw.util)


(defun list-files (directory-name &key (name :wild) (type :wild) resolve-symlinks)
  "Lists files in a directory. Note: PATHNAME-DIRECTORY is used to
ensure that directory-name references a directory. As such the 
trailing slash is required or the namestring/pathname will be truncated." 
  (directory (make-pathname :name name :type type :directory (pathname-directory directory-name))
	     :resolve-symlinks resolve-symlinks))
  
(defun directoryp (designator)
  (unless (pathname-name designator)
    t))

(defun file-p (designator)
  (when (pathname-name designator)
    t))

(defun walk-directory (fn directory-name &optional recursive (ignore-directories '(".git")))
  (labels ((walk (name acc)
	     (cond ((null name)
		    acc)
		   ((consp name)
		    (walk (cdr name) (walk (car name) acc)))
		   ((and recursive (directoryp name))
		    (let ((directory-name (car (last (pathname-directory name)))))
		      (if (member directory-name ignore-directories :test #'string=)
			  acc
			  (walk (list-files name) acc))))
		   ((file-p name)
		    (aif (funcall fn name)
			 (cons self acc)
			 acc)))))
    (nreverse
     (walk (list-files directory-name) nil))))


(defun rename-files (directory-name replacement-list &optional recursive)
  "Change the file name of all files in DIRECTORY-NAME.
REPLACEMENT-LIST is an alist of (<to-find> . <replace-with>) pairs."
  (walk-directory 
   #'(lambda (file)
       (when (file-p file)
	 (let* ((filename (namestring file))
		(new-name (find-and-replace filename replacement-list)))
	   (unless (equal new-name filename)
	     (rename-file filename new-name)))))
   directory-name
   recursive))


(declaim (inline parse-stream))

(defun parse-stream (in out)
  (let ((buffer (make-array 4096 :element-type (stream-element-type in))))
    (loop
      for pos = (read-sequence buffer in)
      while (plusp pos)
      do (write-sequence buffer out :end pos))))


(defun sequence-from-file (file &optional (if-does-not-exist :error))
  "Retrieve contents of file and return sequence."
  (handler-bind ((sb-int:stream-decoding-error #'(lambda (c)
						   (declare (ignore c))
						   (invoke-restart 'sb-int:attempt-resync))))
    (with-open-file (in file :direction :input :if-does-not-exist if-does-not-exist);; :external-format :iso-8859-1) 
      (with-output-to-string (out)
	(parse-stream in out)))))


(defun file-exists-p (file if-exists if-does-not-exist)
  (typecase file
    (pathname (case if-exists
		(:supersede
		 (print (format nil "replacing file: ~a" file)))
		(:append
		 (print (format nil "appending to file: ~a" file)))
		(:overwrite
		 (print (format nil "overwriting file: ~a" file)))))
    (t (when (eq if-does-not-exist :create)
	 (print (format nil "creating file: ~a" file))))))


(defun sequence-to-file (file sequence
			 &optional (if-exists :supersede) (if-does-not-exist :create) (element-type 'character) verbose)
  "Write sequence to file. Defaults to overwrite existing content."
  (let ((file-existsp (probe-file file)))
    (with-open-file (out (ensure-directories-exist file :verbose t)
			 :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist) 
      (when verbose
	(file-exists-p file if-exists if-does-not-exist))
      (write-sequence sequence out))
    (values)))


(defvar *blacklist* '("zip" "tar" "md5" "exe" "woff" "ttf" "jpg" "jpeg" "png" "ico" "der" "fasl" "NBF"))

;; the following and it's per file equivalent would probably produce less errors and work
;; much faster if it was performed as integers; using (unsigned-byte 8) as external format.
;; TO DO

(defun find-in-directory (directory args &optional file-type (exclude-files *blacklist*))
  "Find file and location of all instances of character,
string tokens, or functions in DIRECTORY. Recursively
walks directory, invoking FIND-IN-FILE.

Returns the file name where results are found, along with their
indexed location."
  (flet ((walk (file)
	   (let ((file-name (pathname-name file)))
	     (unless (or (and (char= (char file-name 0) #\.)
			      (char= (char file-name 1) #\#))
			 (char= (char file-name 0) #\#))
	       (awhen (find-in-file file args)
		 (list file self))))))
    (walk-directory
     (lambda (file)
       (cond ((and file-type (string-equal (pathname-type file) file-type))
	      (walk file))
	     (file-type
	      nil)
	     ((and exclude-files
		   (member (pathname-type file) exclude-files :test #'string-equal))
	      nil)
	     (t
	      (walk file))))
     directory
     t)))


(defun find-in-file (file &rest args)
  "Find location of all instances of character, string tokens,
or functions in FILE. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5)."
  (apply #'find-all (sequence-from-file file nil) args))


(defun alter-file-contents (file replacement-list &optional verbose)
  "Change the file contents of FILE. REPLACEMENT-LIST 
is an alist of (<to-find> . <replace-with>) pairs."
  (let ((content (sequence-from-file file)))
    (multiple-value-bind (sequence replaced)
	(find-and-replace content replacement-list)
      (when verbose
	(loop for (item . replacement) in replaced
	   do (print (format nil "'~a' replaced with '~a' in file '~a'" item replacement file))))
      (sequence-to-file file sequence))))


(defun alter-directory-contents (directory replacement-list &optional verbose file-type)
  "Change the contents of all files in DIRECTORY. REPLACEMENT-LIST 
is an alist of (<to-find> . <replace-with>) pairs. 
Files can be constrained to a type by adding an optional file-type arg."
  (walk-directory #'(lambda (designator)
		      (when (or (and (file-p designator) (null file-type))
				(and file-type (string-equal (pathname-type designator) file-type)))
			(let ((file-name (pathname-name designator)))
			  (unless (or (and (char= (char file-name 0) #\.)
					   (char= (char file-name 1) #\#))
				      (char= (char file-name 0) #\#))
			    (alter-file-contents designator replacement-list verbose)))))
		  directory
		  t))


(defun unique-line-register (input-stream)
  "Takes input stream and returns a value-less hash table where each line
is a key."
  (let ((table (make-hash-table :test #'equal)))
    (loop for line = (read-line input-stream nil)
       while line
       unless (nth-value 1 (gethash line table))
       do (setf (gethash line table) nil))
    table))


(defun hash-keys-to-output-stream (table output-stream)
  "Takes hash-table and writes each key to output-stream.
Returns output-stream"
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (write-line key output-stream))
	   table))


(defun remove-duplicate-lines-from-file (file)
  "Remove duplicate lines from file"
  (let ((table (with-open-file (input file)
		 (unique-line-register input))))
    (with-open-file (output file :direction :output :if-exists :supersede)
      (hash-keys-to-output-stream table output))))


(defun remove-duplicate-lines-from-string (seq)
  "Remove duplicate lines from string"
  (let ((table (with-input-from-string (stream seq)
		 (unique-line-register stream)))
	(stream (make-string-output-stream)))
    (hash-keys-to-output-stream table stream)
    (get-output-stream-string stream)))
	  
      
(defun concatenate-files (&rest files)
  (apply #'concatenate 'string
	 (loop for file in files
	    collect (sequence-from-file file))))

(defun concatenate-and-remove-duplicates (directory &optional file-type)
  "Walk directory, concatenate files (of optional type - string) and remove duplicates"
  (remove-duplicate-lines-from-string
   (apply #'concatenate-files
	  (walk-directory #'(lambda (designator) 
			      (if file-type
				  (and (file-p designator)
				       (string-equal (pathname-type designator) file-type)) 
				  (file-p designator)))
			  directory t))))
