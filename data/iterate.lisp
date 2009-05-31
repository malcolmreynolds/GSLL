;; Iterate
;; Norman Werner 2009-05-26 22:23:40EDT iterate.lisp
;; Time-stamp: <2009-05-30 21:40:26EDT iterate.lisp>

(in-package :iter)

#|
As an example try:
(setf m1 (gsll:make-marray 'double-float
                         :initial-contents '((1 2 3) (0 6 8))))

(iterate:iterate (iterate:for e ELEMENT-IN-GSLL-MATRIX m1)
                (format t "~A " e))

(iterate:iterate (iterate:for (row column) INDEXES-IN-GSLL-MATRIX m1)
                (format t "~A " (gsll:maref m1 row column)))
|#


(defclause-sequence :ROW-IN-GSLL-MATRIX
    :ROW-INDEX-OF-GSLL-MATRIX
  :access-fn
  (lambda (marray index)
    (typecase marray
      (gsll::matrix (gsll:row marray index))
      (gsll::mvector
       (error "iterating over the rows in a vector is not supported.~
                          You may want iterate over its elements instead"))
      (t (error "only iterating over rows of an gsll:matrix is supported"))))
  :size-fn
  (lambda (marray)
    (typecase marray
      (gsll::matrix (elt (gsll:dimensions marray) 0))
      (gsll::vector (error "iterating over the rows in a vector is not ~
                    supported. You may want iterate over its elements instead"))
      (t (error "only iterating over rows of an gsll:matrix is supported"))))
  :element-type t :sequence-type t
  :element-doc-string "(copied) rows of a gsll matrix"
  :index-doc-string "index of the rows in a gsll matrix")

(defclause-sequence column-in-gsll-matrix
    column-index-of-gsll-matrix
  :access-fn
  (lambda (marray index)
    (typecase marray
      (gsll::matrix (gsll:column marray index))
      (gsll::vector
       (error "iterating over the columns in a vector is not
                    supported. You may want iterate over its elements instead"))
      (t (error "only iterating over columns of an gsll:matrix is supported"))))
  :size-fn
  (lambda (marray)
    (typecase marray
      (gsll::matrix (elt (gsll:dimensions marray) 1))
      (gsll::vector
       (error "iterating over the columns in a vector is not
                     supported. You may want iterate over its elements instead"))
      (t (error "only iterating over columns of an gsll:matrix is
                   supported"))))
  :element-type t :sequence-type t
  :element-doc-string "(copied) columns of a gsll matrix"
  :index-doc-string "index of the columns in a gsll matrix")

(defclause-sequence element-in-gsll-vector
 index-of-gsll-vector
 :access-fn (lambda (vector index)
              (typecase vector
                (gsll::mvector (gsll:maref vector index))
                (t (error "only iterating over elements of an gsll:vector is supported"))))
 :size-fn (lambda (vector)
            (typecase vector
              (gsll::mvector (elt (gsll:dimensions vector) 0))
              (t (error
		  "only iterating over elements of an gsll:vector is supported"))))
 :element-type t :sequence-type t
 :element-doc-string "(copied) elements of a gsll vector"
 :index-doc-string "index of elements in a gsll vector")

(defmacro-driver (FOR element :ELEMENT-IN-GSLL-MATRIX
		      matrix)
  "Iterates over all (copied) Elements in matrix"
  (let ((row-index (gensym "row-index"))
	(col-index (gensym "col-index"))
	(row-size (gensym "row-size"))
	(col-size (gensym "col-size"))
	(m (gensym "m")))
    (when :generate
      (error "not yet implemented a generate clause for ELEMENT-IN-GSLL-MATRIX"))
    `(progn
       (with ,m = ,matrix)
       (with ,row-index = 0)
       (with ,col-index = 0)
       (with ,row-size =  (elt (gsll:dimensions ,m) 0))
       (with ,col-size =  (elt (gsll:dimensions ,m) 1))
       (:for ,element
	     :next
	     (if (>= ,row-index ,row-size)
		 (:terminate)
		 (if (>= ,col-index ,col-size)
		     (progn
		       (setf ,col-index 0)
		       (incf ,row-index)
		       (if (>= ,row-index ,row-size)
			   (:terminate)
			   (gsll:maref ,m ,row-index ,col-index)))
		     (prog1
			 (gsll:maref ,m ,row-index ,col-index)
		       (incf ,col-index))))))))

(defmacro-driver (FOR indexes :INDEXES-IN-GSLL-MATRIX
		      matrix)
  "Iterates over the indexes in matrix. indexes is a list like (row-index-name
   column-index-name) "
  (let ((row-index (gensym "row-index"))
	(col-index (gensym "col-index"))
	(row-size (gensym "row-size"))
	(col-size (gensym "col-size"))
	(m (gensym "m")))
    (when :generate
      (error "not yet implemented a generate clause for INDEXES-IN-GSLL-MATRIX"))
    `(progn
       (with ,m = ,matrix)
       (with ,row-index = 0)
       (with ,col-index = 0)
       (with ,row-size =  (elt (gsll:dimensions ,m) 0))
       (with ,col-size =  (elt (gsll:dimensions ,m) 1))
       (for ,indexes next (if (>= ,row-index ,row-size)
			      (terminate)
			      (if (>= ,col-index ,col-size)
				  (progn
				    (setf ,col-index 0)
				    (incf ,row-index)
				    (if (>= ,row-index ,row-size)
					(terminate)
					(list ,row-index ,col-index)))
				  (prog1
				      (list ,row-index ,col-index)
				    (incf ,col-index))))))))
