;; Use the foreign-friendly arrays package.
;; Liam Healy 2008-03-22 15:40:08EDT ffa.lisp
;; Time-stamp: <2008-11-11 22:13:20EST foreign-friendly.lisp>
;; $Id$

;;; Use Papp's Foreign-friendly arrays
;;; http://www.princeton.edu/~tpapp/software.html

(in-package :gsl)

;;;;****************************************************************************
;;;; Make arrays for possible foreign use 
;;;;****************************************************************************

(export '(make-array*))

(defun make-array*
    (dimensions element-type
     &key (initial-element nil initial-element-p)
     (initial-contents nil initial-contents-p))
  "Make an array of one or two dimensions for possible use in foreign code.
   Syntax is similar to make-array, but note that element-type
   is mandatory and limited to certain types."
  (assert (member element-type *array-element-types* :test 'equal)
	  (element-type)
	  "Specified element-type must be one of *array-element-types*.")
  (if initial-contents-p
      (if (and (subtypep element-type 'complex)
	       (= (length initial-contents)
		  (* 2 (if (listp dimensions) (apply #'* dimensions) dimensions))))
	  (make-ffa* dimensions element-type
			:initial-contents
			(loop for (re im) on initial-contents by #'cddr
			   collect (complex re im)))
	  (make-ffa* dimensions element-type :initial-contents initial-contents))
      (if initial-element-p
	  (make-ffa* dimensions element-type :initial-element initial-element)
	  (make-ffa* dimensions element-type))))

;;; From Tamas Papp's foreign-friendly arrays (FFA)
(defun make-ffa* (dimensions element-type &key
		 (initial-element 0 initial-element-p)
		 (initial-contents nil initial-contents-p))
  "Make an array that is either one-dimensional or displaced to a
   one-dimensional array.  Array is filled with initial-element or
   initial-contents, coerced to the given type."
  ;; element-type is a type spec in CL form
  (assert (or (atom dimensions) (and (listp dimensions) (car dimensions))))
  (let* ((dimensions (if (atom dimensions) (list dimensions) dimensions))
	 (length (reduce #'* dimensions))
	 (array (cond
		  ((and initial-element-p initial-contents-p)
		   (error "you can't supply both initial-element and ~
                           initial-contents"))
		  ;; initial element given
		  (initial-element-p
		   (make-array length :element-type element-type
			       :initial-element (coerce initial-element
							element-type)))
		  ;; contents given, copy or coerce
		  (initial-contents-p
		   (assert (= (length initial-contents) length))
		   (if (typep initial-contents (list 'vector element-type))
		       (copy-seq initial-contents)
		       (map (list 'vector element-type)
			    (lambda (x) (coerce x element-type)) initial-contents)))
		  ;; neither
		  (t (make-array length :element-type element-type)))))
    (if (cdr dimensions)
	(make-array dimensions :element-type element-type 
		    :displaced-to array)
	array)))
