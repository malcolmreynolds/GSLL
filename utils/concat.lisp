(in-package :gsll-util)

;; functions to horizontally and vertically concatenate matrices and vectors

(defun vcat (&rest vs)
  "Concatenates any number of vectors end to end.

   TODO: currently requires all vectors to be of the same type."
  (declare (optimize (speed 3) (safety 3)))
  (assert vs nil "Cannot concatenate no vectors!")
  (let ((vtype  (type-of (first vs)))
	(length (reduce #'+ (mapcar #'dim0 vs))))
    (assert (all-of-type vtype vs) nil
	    "Cannot concatenate vectors with different types.")
    (let ((vnew     (make-marray vtype :dimensions length))
	  (dest-idx 0))
      (declare (fixnum dest-idx)
	       (gsll:mvector vnew))
      ;; copy each vector in order
      (dolist (v vs)
	(do-vector (v i elm)
	  (setf (maref vnew (the fixnum (+ dest-idx i))) elm))
	(incf dest-idx (the fixnum (dim0 v))))
      vnew)))

(defun mcat-hor (&rest ms)
  "Concatenates any number of matrices horizontally.

   TODO: currently all matrices must be of same type."
  (assert ms nil "Cannot concatenate no matrices!")
  (assert (all-of-type (type-of (first ms)) ms) nil
	  "Cannot concatenate matrices of differing types.")
  (let ((num-rows   (dim0 (first ms)))
	(total-cols (reduce #'+ (mapcar #'dim1 ms))))
    (assert (all #'(lambda (m) (= (dim0 m) num-rows)) ms) nil
	    "Cannot concatenate matrices with different numbers of rows.")
    (let ((mnew (make-marray (type-of (first ms))
			     :dimensions (list num-rows total-cols)))
	  (dest-idx 0))
      ;; copy each matrix in order
      (dolist (m ms)
        (do-matrix (m i j elm)
	  (setf (maref mnew i (+ j dest-idx)) elm))
	(incf dest-idx (dim1 m)))
      mnew)))

(defun mcat-ver (&rest ms)
  "Concatenates any number of matrices vertically.

   TODO: currently all matrices must be of same type."
  (assert ms nil "Cannot concatenate no matrices!")
  (assert (all-of-type (type-of (first ms)) ms) nil
	  "Cannot concatenate matrices of differing types.")
  (let ((num-cols   (dim1 (first ms)))
	(total-rows (reduce #'+ (mapcar #'dim0 ms))))
    (assert (all #'(lambda (m) (= (dim1 m) num-cols)) ms) nil
	    "Cannot concatenate matrices with different numbers of columns.")
    (let ((mnew (make-marray (type-of (first ms))
			     :dimensions (list total-rows num-cols)))
	  (dest-idx 0))
      ;; copy each matrix in order
      (dolist (m ms)
	(do-matrix (m i j elm)
	  (setf (maref mnew (+ i dest-idx) j) elm))
	(incf dest-idx (dim0 m)))
      mnew)))

