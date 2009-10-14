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
   bound to the index of this element. 

   In the body 'elm' is a symbol macro expanding to (maref vector i), ie
   it is used to get or set the current element."
  (let ((m (gensym)))
    `(symbol-macrolet ((elm (maref ,vector ,i)))
       (let ((,m (dim0 ,vector)))
	 (do ((,i 0 (1+ ,i)))
	     ((= ,i ,m))
	   (declare (fixnum ,i))
	   ,@body)))))

(defmacro do-matrix ((matrix i j) &body body)
  "Executes ,@body once for each element of matrix, with i and
   and j bound to the row and column indices respectively.

   In the body 'elm' is a symbol macro expanding to (maref matrix i j),
   ie it is used to get or set the current element."
  (let ((m (gensym))
	(n (gensym)))
    `(symbol-macrolet ((elm (maref ,matrix ,i ,j)))
       (let ((,m (dim0 ,matrix))
	     (,n (dim1 ,matrix)))
	 (do ((,i 0 (1+ ,i)))
	     ((= ,i ,m))
	   (declare (fixnum ,i))
	   (do ((,j 0 (1+ ,j)))
	       ((= ,j ,n))
	     (declare (fixnum ,j))
	     ,@body))))))

(defmacro do-matrix-up-triangular ((matrix i j) &body body)
  "Executes ,@body once for each element of the upper triangular
   portion of the matrix, with i and j bound to the row and column
   indices respectively. Includes the major diagonal.

   elm is a symbol-macro to access current element, see do-matrix..."
  (let ((m (gensym)) (n (gensym)))
    `(symbol-macrolet ((elm (maref ,matrix ,i ,j)))
       (let ((,m (dim0 ,matrix))
	     (,n (dim1 ,matrix)))
	 (do ((,i 0 (1+ ,i)))
	     ((= ,i ,m))
	   (declare (fixnum ,i))
	   (do ((,j ,i (1+ ,j)))
	       ((= ,j ,n))
	     (declare (fixnum ,j))
	     ,@body))))))

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
    `(symbol-macrolet ((elm (maref ,matrix ,i ,i)))
       (let ((,m (min (dim0 ,matrix) (dim1 ,matrix))))
	 (do ((,i 0 (1+ ,i)))
	     ((= ,i ,m))
	   (declare (fixnum ,i))
	   ,@body)))))