(in-package :gsll-util)

;; functions to horizontally and vertically concatenate matrices and vectors

(defun vcat (v1 v2)
  "Concatenates two vectors end to end.

   TODO: Currently requires both vectors to be of the same type, this
   should probably be changed to return the new vector which is the
   most specific possible thing (so concat a vector of ints with one
   of single-floats and the result will be a vector of single float.

   TODO: make this take args (&rest vectors) so that any number can be
   concatenated at once with only one allocation for the final result."
  (declare (optimize (speed 3) (safety 3))
	   (type gsll:mvector v1 v2))
  (assert (eq (type-of v1) (type-of v2)) (v1 v2)
	  "Cannot concatenate vectors with different types.")
  (let ((v1-size (dim0 v1))
	(v2-size (dim0 v2)))
    (declare (fixnum v1-size v2-size))
    (let ((vnew (make-marray (type-of v1) :dimensions (+ v1-size v2-size))))
      ;; copy elements from first vector..
      (do-vector (v1 i elm)
	(setf (maref vnew i) elm))
      ;; copy from the second vector.
      (do-vector (v2 i elm)
	(setf (maref vnew (the fixnum (+ i v1-size))) elm))
      vnew)))

;; (defun all-of-type (type objects)
;;   "Tests whether every item in objects satisfies (typep ")

;; (defun vcat-many (&rest vectors)
;;   "Concatenates any number of vectors end to end.

;;    TODO: currently requires all vectors to be of the same type."
  
;;   (let ((vec-size))))

(defun mcat-hor (m1 m2)
  "Concatenates two matrices horizontally."
  (declare (optimize (speed 3) (safety 3))
	   (type gsll:matrix m1 m2))
  (let ((rows   (dim0 m1))
	(cols1 (dim1 m1))
	(cols2 (dim1 m2)))
    (declare (fixnum rows cols1 cols2))
    (assert (= (dim0 m1) (dim0 m2)) (m1 m2)
	    "m1 has ~D rows, m2 has ~D rows, cannot horizontally concatenate."
	    (dim0 m1) (dim0 m2))
    (assert (eq (type-of m1) (type-of m2)) (m1 m2)
	    "Cannot concatenate matrices of different types.")
    (let ((mnew (make-marray (type-of m1) :dimensions (list rows (the fixnum
								   (+ cols1 cols2))))))
      ;; copy from first matrix
      (do-matrix (m1 i j elm)
	(setf (maref mnew i j) elm))
      ;; copy from second matrix
      (do-matrix (m2 i j elm) 
	(setf (maref mnew i (the fixnum (+ j cols1))) elm))
      mnew)))

(defun mcat-ver (m1 m2)
  "Concatenates two matrices vertically."
  (declare (optimize (speed 3) (safety 3))
	   (type gsll:matrix m1 m2))
  (let ((cols (dim0 m1))
	(rows1 (dim0 m1))
	(rows2 (dim0 m2)))
    (declare (fixnum rows1 rows2 cols))
    (assert (= (dim1 m1) (dim1 m2)) (m1 m2)
	    "m1 has ~D cols, m2 has ~D cols, cannot vertically concatenate."
	    (dim1 m1) (dim1 m2))
    (assert (eq (type-of m1) (type-of m2)) (m1 m2)
	    "Cannot concatenate matrices of different types.")
    (let ((mnew (make-marray (type-of m1) :dimensions (list (+ rows1 rows2) cols))))
      ;; copy from first matrix
      (do-matrix (m1 i j elm)
	(setf (maref mnew i j) elm))
      ;; copy from second matrix
      (do-matrix (m2 i j elm)
	(setf (maref mnew (the fixnum (+ i rows1)) j) elm))
      mnew)))