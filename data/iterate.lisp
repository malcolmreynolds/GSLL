;; Iterate
;; Norman Werner 2009-05-26 22:23:40EDT iterate.lisp
;; Time-stamp: <2009-06-04 23:10:41EDT iterate.lisp>

(in-package :iter)

#|
As an example try:
(defparameter m1 #m(1 2 3 ^ 0 6 8))

(iter:iter (iter:for e :matrix-element m1) (format t "~A " e))

(iter:iter (iter:for (row column) :matrix-element-index m1)
	   (format t "~A " (gsll:maref m1 row column)))
|#


(defclause-sequence matrix-row matrix-row-index
  :access-fn
  (lambda (marray index)
    (check-type marray gsl:matrix)
    (gsll:row marray index))
  :size-fn
  (lambda (marray)
    (check-type marray gsl:matrix)
    (gsll:dim0 marray))
  :element-type t :sequence-type t
  :element-doc-string "(copied) rows of a matrix"
  :index-doc-string "index of the rows in a matrix")

(defclause-sequence matrix-column matrix-column-index
  :access-fn
  (lambda (marray index)
    (check-type marray gsl:matrix)
    (gsll:column marray index))
  :size-fn
  (lambda (marray)
    (check-type marray gsl:matrix)
    (gsll:dim1 marray))
  :element-type t :sequence-type t
  :element-doc-string "(copied) columns of a matrix"
  :index-doc-string "index of the columns in a matrix")

(defclause-sequence vector-element vector-element-index
  :access-fn
  (lambda (vector index)
    (check-type vector gsl:mvector)
    (gsll:maref vector index))
  :size-fn
  (lambda (vector)
    (check-type vector gsl:mvector)
    (gsll:dim0 vector))
  :element-type t :sequence-type t
  :element-doc-string "(copied) elements of a vector"
  :index-doc-string "index of elements in a vector")

(defmacro-driver (FOR element matrix-element matrix)
  "Iterates over all (copied) elements in matrix"
  (let ((row-index (gensym "row-index"))
	(col-index (gensym "col-index"))
	(row-size (gensym "row-size"))
	(col-size (gensym "col-size"))
	(m (gensym "m")))
    (when generate
      (error "Not yet implemented a generate clause for matrix-element."))
    `(progn
       (with ,m = ,matrix)
       (with ,row-index = 0)
       (with ,col-index = 0)
       (with ,row-size =  (gsll:dim0 ,m))
       (with ,col-size =  (gsll:dim1 ,m))
       (for ,element
	     :next
	     (if (>= ,row-index ,row-size)
		 (terminate)
		 (if (>= ,col-index ,col-size)
		     (progn
		       (setf ,col-index 0)
		       (incf ,row-index)
		       (if (>= ,row-index ,row-size)
			   (terminate)
			   (gsll:maref ,m ,row-index ,col-index)))
		     (prog1
			 (gsll:maref ,m ,row-index ,col-index)
		       (incf ,col-index))))))))

(defmacro-driver (FOR indexes matrix-element-index
		      matrix)
  "Iterates over the indexes in matrix. indexes is a list like (row-index-name
   column-index-name) "
  (let ((row-index (gensym "row-index"))
	(col-index (gensym "col-index"))
	(row-size (gensym "row-size"))
	(col-size (gensym "col-size"))
	(m (gensym "m")))
    (when generate
      (error "Not yet implemented a generate clause for matrix-element-index."))
    `(progn
       (with ,m = ,matrix)
       (with ,row-index = 0)
       (with ,col-index = 0)
       (with ,row-size =  (gsll:dim0 ,m))
       (with ,col-size =  (gsll:dim1 ,m))
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
