(in-package :gsll-util)

;; functions to horizontally and vertically concatenate matrices and vectors

(defun elm-type-v (v)
  (type-of (maref v 0)))

(defun elm-type-m (m)
  (type-of (maref m 0 0)))

(defun all-of-type (type objects)
  "Tests whether every obj in objects satisfies (typep obj type)"
  (and (mapcar #'(lambda (obj) (typep obj type)) objects)))

(defun vcat (&rest vs)
  "Concatenates any number of vectors end to end.

   TODO: currently requires all vectors to be of the same type."
  (declare (optimize (speed 3) (safety 3)))
  (let ((vtype  (type-of (first vs)))
	(length (reduce #'+ (mapcar #'dim0 vs))))
    (assert (all-of-type vtype vs) nil
	    "Cannot concatenate vectors with different types.")
    (let ((vnew     (make-marray (elm-type-v (first vs)) :dimensions length))
	  (dest-idx 0))
      (declare (fixnum dest-idx)
	       (gsll:mvector vnew))
      ;; copy each vector in order
      (dolist (v vs)
	(do-vector (v i elm)
	  (setf (maref vnew (the fixnum (+ dest-idx i))) elm))
	(incf dest-idx (the fixnum (dim0 v))))
      vnew)))

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
    (let ((mnew (make-marray (elm-type-m m1) :dimensions (list rows (the fixnum
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
    (let ((mnew (make-marray (elm-type-m m1) :dimensions (list (+ rows1 rows2) cols))))
      ;; copy from first matrix
      (do-matrix (m1 i j elm)
	(setf (maref mnew i j) elm))
      ;; copy from second matrix
      (do-matrix (m2 i j elm)
	(setf (maref mnew (the fixnum (+ i rows1)) j) elm))
      mnew)))