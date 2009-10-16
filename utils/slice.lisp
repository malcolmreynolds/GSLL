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

(defun vslice (vec indices)
  "Returns a new vector containing elements from vec using the indices
   given in indices. Dispatches to various more specialised functions"
  (declare (type gsll:mvector vec))
  ;; Let's allow a bunch of different ways to specify the vector. This
  ;; should maybe be done with a generic function in future?
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


;; Matrix functins

(defun mslice (mtx ind-1 ind-2)
  "Returns a new matrix consisting of the corresponding elements from the
   matrix. Acceptable values for indices 1 or 2 include the keyword :all"
  ;; This code is a bit of a state but allowing full flexibility in both
  ;; arguments has a price. Definitely due a second pass.

  ;; Basic idea is, each of the index values can be either a gsll
  ;; vector (of integers), a basic list, or the :all keyword, so we test for each
  ;; of the 9 possble combinations.

  ;; probably some badass closure magic can sort this out..
  (cond ((uint-vec? ind-1)
	 (cond ((uint-vec? ind-2) (mslice-uintvec-uintvec mtx ind-1 ind-2))
;;	       ((listp ind-2)     (mslice-uintvec-list    mtx ind-1 ind-2))
;;	       ((eq ind-2 :all)   (mslice-uintvec-all     mtx ind-1))
	       (t (error "ind-2 supplied as a ~A which is unsupported."
			 (type-of ind-2)))))
	
   ;;; All this is commented out because I refuse to let this code into the wild
   ;;; in such a horrible fashion. uintvec options are left enabled to prove
   ;;; that something is working. Watch this space..
	
;; 	((listp ind-1)
;; 	 (cond ((uint-vec? ind-2) (mslice-list-uintvec mtx ind-1 ind-2))
;; 	       ((listp ind-2)     (mslice-list-list    mtx ind-1 ind-2))
;; 	       ((eq ind-2 :all)   (mslice-list-all     mtx ind-1))
;; 	       (t (error "ind-2 supplied as a ~A which is unsupported."
;; 			 (type-of ind-2)))))
;; 	((eq ind-1 :all)
;; 	 (cond ((uint-vec? ind-2) (mslice-all-uintvec mtx ind-2))
;; 	       ((listp ind-2)     (mslice-all-list    mtx ind-2))
;; 	       ;; in this case the person wants all of the matrix
;; 	       ;; so just make a copy and be done with it. This is to keep
;; 	       ;; the convention that what is returned from this function is
;; 	       ;; always a fresh object.
;; 	       ((eq ind-2 :all)   (copy mtx))
;; 	       (t (error "ind-2 supplied as a ~A which is unsupported."
;; 			 (type-of ind-2)))))
	(t
	 (error "ind-1 supplied as a ~A which is unsupported."
		(type-of ind-1)))))

(defun mslice-uintvec-uintvec (mtx ind-1 ind-2)
  (let ((mnew (make-marray (type-of mtx) :dimensions (list (dim0 ind-1) (dim0 ind-2)))))
    (do-matrix (mnew i j elm)
      (setf elm (maref mtx (maref ind-1 i) (maref ind-2 j))))
    mnew))