;; Use the foreign-friendly arrays package.
;; Liam Healy 2008-03-22 15:40:08EDT ffa.lisp
;; Time-stamp: <2008-11-11 20:42:01EST foreign-friendly.lisp>
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
	  (ffa:make-ffa dimensions element-type
			:initial-contents
			(loop for (re im) on initial-contents by #'cddr
			   collect (complex re im)))
	  (ffa:make-ffa dimensions element-type :initial-contents initial-contents))
      (if initial-element-p
	  (ffa:make-ffa dimensions element-type :initial-element initial-element)
	  (ffa:make-ffa dimensions element-type))))
