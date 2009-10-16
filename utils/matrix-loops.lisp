(in-package :gsll-util)

(use-package :cl-utilities)

;; Macros for looping through matrices and vectors

;; TODO: option to add in assertions that things which are supposed to
;; be a matrix/vector really are.. #'dimensions is a generic function
;; so that won't fire if it's the wrong thing but if a user has a
;; do-matrix form and they give it a vector the error will probably
;; happen when (= ,j ,n) is executed, as n will be nil. Might be good
;; to automatically chuck in an (typep v 'gsll:mvector) form as
;; appropriate

(defmacro do-vector ((vector i &optional elm) &body body)
  "Executes ,@body once for each element of vector, with i bound to
   the index of this element.

   If provided, elm becomes a symbol macro to get/setf the
   current element inside the body."
  (with-gensyms (m)
    (let ((inner-code `(let ((,m (dim0 ,vector)))
			 (declare (fixnum ,m))
			 (dotimes (,i ,m)
			   ,@body))))
      (if elm
	  `(symbol-macrolet ((,elm (maref ,vector ,i)))
	     ,inner-code)
	  inner-code))))

(defmacro do-matrix ((matrix i j &optional elm) &body body)
  "Executes ,@body once for each element of matrix, with i and and j
   bound to the row and column indices respectively.

   If provided, elm becomes a symbol macro to get/setf the
   current element inside the body."
  (with-gensyms (m n)
    (let ((inner-code `(let ((,m (dim0 ,matrix))
			     (,n (dim1 ,matrix)))
			 (declare (fixnum ,m ,n))
			 (dotimes (,i ,m)
			   (dotimes (,j ,n)
			     ,@body)))))
      (if elm
	  `(symbol-macrolet ((,elm (maref ,matrix ,i ,j)))
	     ,inner-code)
	  inner-code))))

(defmacro do-matrix-up-triangular ((matrix i j &optional elm) &body body)
  "Executes ,@body once for each element of the upper triangular
   portion of the matrix, with i and j bound to the row and column
   indices respectively. Includes the major diagonal.

   If provided, elm becomes a symbol macro to get/setf the
   current element inside the body."
  (with-gensyms (m n)
    (let ((inner-code `(let ((,m (dim0 ,matrix))
			     (,n (dim1 ,matrix)))
			 (declare (fixnum ,m ,n))
			 (dotimes (,i ,m)
			   (do ((,j ,i (1+  ,j)))
			       ((= ,j ,n))
			     (declare (fixnum ,j))
			     ,@body)))))
      (if elm
	  `(symbol-macrolet ((,elm (maref ,matrix ,i ,j)))
	     ,inner-code)
	  inner-code))))

(defmacro do-matrix-rows ((matrix r &optional row-name) &body body)
  "Executes ,@body once for each row of the matrix, with r bound the
   the row index.

   If provided, row-name becomes a symbol macro to get/setf the
   current row inside the body."
  (with-gensyms (m)
    (let ((inner-code `(let ((,m (dim0 ,matrix)))
			 (declare (fixnum ,m))
			 (dotimes (,r ,m)
			   ,@body))))
      (if row-name
	  `(symbol-macrolet ((,row-name (row ,matrix ,r)))
	     ,inner-code)
	  inner-code))))

(defmacro do-matrix-cols ((matrix c &optional column-name) &body body)
  "Executes ,@body once for each column of the matrix, with c bound to
   the column index.

   If provided, column-name becomes a symbol-macro to get/setf the
   current column inside the body."
  (with-gensyms (m)
    (let ((inner-code `(let ((,m (dim1 ,matrix)))
			 (declare (fixnum ,m))
			 (dotimes (,c ,m)
			   ,@body))))
      (if column-name
	  `(symbol-macrolet ((,column-name (column ,matrix ,c)))
	     ,inner-code)
	  inner-code))))

(defmacro do-matrix-diag ((matrix i &optional elm) &body body)
  "Executes ,@body once for every diagonal element of the matrix.

   If provided, elm becomes a symbol macro to get/setf the current
   element inside the body."
  (with-gensyms (m)
    (let ((inner-code `(let ((,m (min (dim0 ,matrix) (dim1 ,matrix))))
			 (declare (fixnum ,m))
			 (dotimes (,i ,m)
			   ,@body))))
      (if elm
	  `(symbol-macrolet ((,elm (maref ,matrix ,i ,i)))
	     ,inner-code)
	  inner-code))))