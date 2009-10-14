(in-package :gsll-util)

;; Macros for looping through matrices and vectors

;; TODO: option to add in assertions that things which are supposed to
;; be a matrix/vector really are.. #'dimensions is a generic function
;; so that won't fire if it's the wrong thing but if a user has a
;; do-matrix form and they give it a vector the error will probably
;; happen when (= ,j ,n) is executed, as n will be nil. Might be good
;; to automatically chuck in an (typep v 'gsll:mvector) form as
;; appropriate

(defmacro do-vector ((vector i) &body body)
  "Executes ,@body once for each element of vector, with i
   bound to the index of this element. use-assert argument controls
   whether to include an assertion the vector really is a gsll vector."
  (let ((m (gensym)))
    `(let ((,m (dim0 ,vector)))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,m))
	 (declare (fixnum ,i))
	 ,@body))))

(defmacro do-matrix ((matrix i j) &body body)
  "Executes ,@body once for each element of matrix, with i and
   and j bound to the row and column indices respectively."
  (let ((m (gensym))
	(n (gensym)))
    `(let ((,m (dim0 ,matrix))
	   (,n (dim1 ,matrix)))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,m))
	 (declare (fixnum ,i))
	 (do ((,j 0 (1+ ,j)))
	     ((= ,j ,n))
	   (declare (fixnum ,j))
	   ,@body)))))

(defmacro do-matrix-up-triangular ((matrix i j) &body body)
  "Executes ,@body once for each element of the upper triangular
   portion of the matrix, with i and j bound to the row and column
   indices respectively. Includes the major diagonal."
  (let ((m (gensym)) (n (gensym)))
    `(let ((,m (dim0 ,matrix))
	   (,n (dim1 ,matrix)))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,m))
	 (declare (fixnum ,i))
	 (do ((,j ,i (1+ ,j)))
	     ((= ,j ,n))
	   (declare (fixnum ,j))
	   ,@body)))))

(defmacro do-matrix-rows ((matrix r) &body body)
  "Executes ,@body once for each row of the matrix, with r bound
   the the row index."
  (let ((m (gensym)))
    `(let ((,m (dim0 ,matrix)))
       (do ((,r 0 (1+ ,r)))
	   ((= ,r ,m))
	 (declare (fixnum ,r))
	 ,@body))))

(defmacro do-matrix-cols ((matrix c) &body body)
  "Executes ,@body once for each column of the matrix, with c bound
   to the column index."
  (let ((m (gensym)))
    `(let ((,m (dim1 ,matrix)))
       (do ((,c 0 (1+ ,c)))
	   ((= ,c ,m))
	 (declare (fixnum ,c))
	 ,@body))))

(defmacro do-matrix-diag ((matrix i) &body body)
  "Executes ,@body once for every diagonal element of the matrix."
  (let ((m (gensym)))
    `(let ((,m (min (dim0 ,matrix) (dim1 ,matrix))))
       (do ((,i 0 (1+ ,i)))
	   ((= ,i ,m))
	 (declare (fixnum ,i))
	 ,@body))))