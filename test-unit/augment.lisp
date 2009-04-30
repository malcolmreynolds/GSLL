;; Additional methods for lisp-unit
;; Liam Healy 2009-04-15 23:23:30EDT augment.lisp
;; Time-stamp: <2009-04-30 12:25:37EDT augment.lisp>
;; $Id: $

(in-package :gsl)

(defmethod lisp-unit:numerical-equal
    ((result1 marray) (result2 marray) &key (test #'lisp-unit:number-equal))
  "Return true if the arrays are numerically equal according to :TEST."
  (when (equal (dimensions result1) (dimensions result2))
    (lisp-unit:numerical-equal (cl-array result1) (cl-array result2)
			       :test test)))
