(in-package stw.util)


(defun list-files (directory-name &key (name :wild) (type :wild))
  "Lists files in a directory. Note: PATHNAME-DIRECTORY is used to
ensure that directory-name references a directory. As such the 
trailing slash is required or the namestring/pathname will be truncated." 
  (directory (make-pathname :name name :type type :directory (pathname-directory directory-name))))
  
(defun directory-p (designator)
  (unless (pathname-name designator)
    t))

(defun file-p (designator)
  (when (pathname-name designator)
    t))

(defun walk-directory (fn directory-name &optional recursive)
  (let* ((names (list-files directory-name))
	 (directories)
	 (files (map-tree-depth-first
		 #'(lambda (name)
		     (when (directory-p name)
		       (push name directories))
		     (funcall fn name))
		 names)))
    (if recursive
	(append
	 files
	 (mappend #'map-tree-depth-first
		  #'(lambda (directory-name)
		      (walk-directory fn directory-name t))
		  (nreverse directories)))
	files)))


(defun rename-files (directory-name replacement-list &optional recursive)
  "Change the file name of all files in DIRECTORY-NAME.
REPLACEMENT-LIST is an alist of (<to-find> . <to-replace>) pairs."
  (walk-directory 
   #'(lambda (file)
       (when (file-p file)
	 (let* ((filename (namestring file))
		(new-name (find-and-replace filename replacement-list)))
	   (unless (equal new-name filename)
	     (rename-file filename new-name)))))
   directory-name
   recursive))


(defun sequence-from-file (file &optional (if-does-not-exist :error))
  "Retrieve contents of file and return sequence."
  (with-output-to-string (output)
    (with-open-file (in file :direction :input :if-does-not-exist if-does-not-exist) 
      (let ((buffer (make-array 4096 :element-type (stream-element-type in))))
	(loop for pos = (read-sequence buffer in)
	   while (plusp pos)
	   do (write-sequence buffer output :end pos))))))

  
(defun sequence-to-file (file sequence &optional (if-exists :supersede) (if-does-not-exist :create))
  "Write sequence to file. Defaults to overwrite existing content."
  (with-open-file (out file :direction :output :if-exists if-exists :if-does-not-exist if-does-not-exist) 
    (let ((seq (make-array (length sequence) :element-type (stream-element-type out))))
      (write-sequence seq out)
      seq)))


(defun find-in-file (file &rest args)
  "Find location of all instances of character, string tokens,
or functions in FILE. 

Any function calls must accept a seq and index, 
(type fixnum), as arguments, and return a list of matching index
and (+ index (length index)), or nil. E.g. Matching \"abc\" in 
\"abcdefgabc\" returns a list of '((0 3) (7 10)) 
while #\e returns (4 5)."
  (apply #'find-all (sequence-from-file file) args))


(defun alter-file-contents (file replacement-list)
  "Change the file contents of FILE. REPLACEMENT-LIST 
is an alist of (<to-find> . <to-replace>) pairs."
  (let ((content (sequence-from-file file)))
    (sequence-to-file file (find-and-replace content replacement-list))))
