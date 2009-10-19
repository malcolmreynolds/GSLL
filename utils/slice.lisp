(in-package :gsll-util)

;; Functions to slice matrices and vectors, ie something like MATLAB:
;; >> A = [ 1 2 3 4 ; 5 6 7 8 ; 9 10 11 12 ];
;; A =
;;      1     2     3     4
;;      5     6     7     8
;;      9    10    11    12
;; >> B = A(1:3,[2,4])
;; B =
;;      2     4
;;      6     8
;;     10    12

;; A = B(1:3,[2,4]) takes data from first 3 rows and columns 2 and 4

;;;; VECTOR SLICE READ

(defun vslice (vec indices)
  "Returns a new vector containing elements from vec using the indices
   given in indices. Dispatches to various more specialised functions"
  (declare (type gsll:mvector vec))
  ;; Let's allow a bunch of different ways to specify the vector. This
  ;; should maybe be done with a generic function in future? Not sure about
  ;; how much overhead that will cause versus this approach...
  (cond ((uint-vec? indices) (vslice-uintmvec vec indices))
	;; insert more kinds of indices here?
	((listp  indices)    (vslice-list vec indices))
	(t  (error "Indices supplied as a ~A which is unsupported."
		   (type-of indices)))))

(defun vslice-uintmvec (vec indices)
  "Returns a new vector containing elements form vec according to
   indices which is a GSLL vector containing unsigned ints of some
   kind."
  (let ((vnew (make-marray (type-of vec) :dimensions (dim0 indices))))
    (do-vector (vnew i elm)
      (setf elm (maref vec (maref indices i))))
    vnew))

(defun vslice-list (vec list)
  "Returns a new vector containing elements from ec according to indices
   which is a list, hopefully containing non negative integers."
  (let ((vnew (make-marray (type-of vec) :dimensions (length list))))
    (do-vector (vnew i elm)
      (setf elm (maref vec (pop list))))
    vnew))

;;;; VECTOR SLICE WRITE

(defun (setf vslice) (vals vec indices)
  "Allows slices to be used as the place in a setf expression."
  (cond ((uint-vec? indices) (setf (vslice-uintmvec vec indices) vals))
	((listp     indices) (setf (vslice-list     vec indices) vals))
	(t (error "Indices supplied as a ~A which is unsupported."
		  (type-of indices)))))

(defun (setf vslice-uintmvec) (vals vec indices)
  "Sets the locations specified by indices in vec to elements copied from val."
  (do-vector (vals i elm)
    (setf (maref vec (maref indices i)) elm))
  vec)

(defun (setf vslice-list) (vals vec list)
  "Sets the locations specified by indices in vec to elements copied from val."
  (do-vector (vals i elm)
    (setf (maref vec (pop list)) elm))
  vec)




;; Matrix functions

(defun mslice (mtx ind-1 ind-2)
  "Returns a new matrix consisting of the corresponding elements from the
   matrix. Acceptable values for indices 1 or 2 include the keyword :all"
  (cond ((uint-vec? ind-1)
	 (cond ((uint-vec? ind-2) (mslice-uintvec-uintvec mtx ind-1 ind-2))
	       ((listp ind-2)     (mslice-uintvec-list    mtx ind-1 ind-2))
	       ((eq ind-2 :all)   (mslice-uintvec-all     mtx ind-1))
	       (t (error "ind-2 supplied as a ~A which is unsupported."
			 (type-of ind-2)))))
	((listp ind-1)
	 (cond ((uint-vec? ind-2) (mslice-list-uintvec mtx ind-1 ind-2))
	       ((listp ind-2)     (mslice-list-list    mtx ind-1 ind-2))
	       ((eq ind-2 :all)   (mslice-list-all     mtx ind-1))
	       (t (error "ind-2 supplied as a ~A which is unsupported."
			 (type-of ind-2)))))
	((eq ind-1 :all)
	 (cond ((uint-vec? ind-2) (mslice-all-uintvec mtx ind-2))
	       ((listp ind-2)     (mslice-all-list    mtx ind-2))
	       ;; in this case the person wants all of the matrix
	       ;; so just make a copy and be done with it. This is to keep
	       ;; the convention that what is returned from this function is
	       ;; always a fresh object.
	       ((eq ind-2 :all)   (copy mtx))
	       (t (error "ind-2 supplied as a ~A which is unsupported."
			 (type-of ind-2)))))
	(t
	 (error "ind-1 supplied as a ~A which is unsupported."
		(type-of ind-1)))))

(defmacro def-slice-func (name type1 type2)
  "Builds a slice function which operates using indices of the given types."
  (let ((arglist (list 'mtx))
	dim-form-1 dim-form-2 acc-form-1 acc-form-2 save-head)
    (ecase type1
      (mvec (setf dim-form-1 '(dim0 ind-1)
		  acc-form-1 '(maref ind-1 i)))
      (list (setf dim-form-1 '(length ind-1)
		  acc-form-1 '(car ind-1)))
      (all  (setf dim-form-1 '(dim0 mtx)
		  acc-form-1 'i)))
    ;; see whether we need a parameter for this index
    (unless (eq type1 'all)
      (setf arglist (nconc arglist (list 'ind-1))))
        
    (ecase type2
      (mvec (setf dim-form-2 '(dim0 ind-2)
		  acc-form-2 '(maref ind-2 j)))
      (list (setf dim-form-2 '(length ind-2)
		  acc-form-2 '(pop ind-2)
		  save-head  t))
      (all  (setf dim-form-2 '(dim1 mtx)
		  acc-form-2 'j)))
    (unless (eq type2 'all)
      (setf arglist (nconc arglist (list 'ind-2))))
    
    `(defun ,name ,arglist
       (declare (optimize (speed 3) (safety 3)))
       (let* ( ;; if ind-2 is a list we need to save the head
	      ;; to restore it after each row is full. If not
	      ;; the case, well this is one saved reference which
	      ;; we dont use. probably a cost worth paying..
	      (save-head ,(if save-head 'ind-2))
	      (mnew-rows ,dim-form-1)
	      (mnew-cols ,dim-form-2)
	      (mnew (make-marray (type-of mtx)
				 :dimensions (list mnew-rows mnew-cols))))
	 (declare (ignorable save-head)
		  (fixnum mnew-rows mnew-cols)
		  (type gsll:matrix mnew))
	 (dotimes (i mnew-rows mnew)
	   (dotimes (j mnew-cols)
	     ;; this does the actual work - the ecase forms above ensure
	     ;; we have the correct expressions for indexing into mtx.
	     (setf (maref mnew i j)
		   (maref mtx ,acc-form-1 ,acc-form-2)))
	   ;; depending on the type of indices we have, we might need
	   ;; some adjustments after each row. These are called swizzles.
	   ,@(let (swizzles)
		 ;; if ind-2 is a list, restore the head after each row.
		 ;; This is because we will be popping it as we go.
		 (if save-head
		     (push '(setf ind-2 save-head) swizzles))
		 ;; if ind-1 is a list, pop at this point, as we need
		 ;; the next value for the next row.
		 (if (eq type1 'list)
		     (push '(pop ind-1) swizzles))
		 swizzles))))))

;; define the functions using the macro above..
(def-slice-func mslice-uintvec-uintvec mvec mvec)
(def-slice-func mslice-uintvec-list    mvec list)
(def-slice-func mslice-uintvec-all     mvec all)
(def-slice-func mslice-list-uintvec    list mvec)
(def-slice-func mslice-list-list       list list)
(def-slice-func mslice-list-all        list all)
(def-slice-func mslice-all-uintvec     all  mvec)
(def-slice-func mslice-all-list        all  list)
;; NB we don't define mslice-all-all because that devolves
;; to simply calling #'copy.
