;; Utility definitions
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-01-27 13:28:29EST utility.lisp>
;; $Id: $

(in-package :gsl)

(defun make-symbol-from-strings (&rest strings)
  "Construct a symbol by concatenating words with hyphens."
  (intern (format nil "~{~:@(~a~)~^-~}" (mapcar #'string strings))))

(defparameter *gsl-prefix* 'gsl)

(defparameter *gsl-objects* nil)

(defstruct gsl-object name allocate free set num-alloc-args)

(defmacro set-asf (name allocate free &optional set (num-alloc-args 1))
  "Define an expansion of this name in #'with-gsl-objects
   and use the appropriate functions for allocating, freeing, and setting."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (pushnew
     (make-gsl-object :name ',name :allocate ',allocate :free ',free
      :set ',set :num-alloc-args ,num-alloc-args)
     *gsl-objects*
     :test #'equalp)))

;; (set-asf poly-complex complex-workspace-alloc complex-workspace-free nil 1)
;; (set-asf mfminimizer allocate-mfminimizer free-mfminimizer set-mfminimizer 2)

(defun gslo-letform (specs specfns)
  "Create the let form for GSL objects."
  (mapcar
   (lambda (s sf)
     `(,(second s)
       (,(gsl-object-allocate sf)
	,@(subseq (cddr s) 0 (gsl-object-num-alloc-args sf)))))
   specs specfns))

(defun gslo-setform (specs specfns)
  "Create the set forms for GSL objects."
  (loop for s in specs for sf in specfns
	for addl-set-args
	= (subseq (cddr s) (gsl-object-num-alloc-args sf))
	when (and (gsl-object-set sf) addl-set-args)
	collect
	`(,(gsl-object-set sf)
	  ;; presume the thing is the first arg to the set function,
	  ;; then add the other arguments
	  ,@(cons (second s) addl-set-args))))

(export 'with-gsl-objects)
(defmacro with-gsl-objects (specs &body body)
  "Allocate, optionally set, and free any foreign object
   used in GSL calculations."
  (let ((specfns
	 (mapcar
	  (lambda (s)
	    (or (find (first s) *gsl-objects* :key #'gsl-object-name)
		(error "Could not find GSL object named ~s" (first s))))
	  specs)))
    `(let (,@(gslo-letform specs specfns))
      (unwind-protect
	   (progn			; call set functions
	     ,@(gslo-setform specs specfns)
	     ,@body)
	,@(loop for s in specs for sf in specfns
		collect `(,(gsl-object-free sf) ,(second s)))))))

;;; roots-multi needs lambda list
;;; (solver solver-type function initial)
;;; allocate: solver-type (dim0 ,initial)
;;; set:  solver function initial


#|
(vector-double vec init)
->
(make-data vector-double nil (length init))
(setf (data vec) init)

;;; Example
(set-asf chebyshev allocate-chebyshev free-chebyshev initialize-chebyshev)

(with-gsl-objects ((chebyshev cheb 40 chebyshev-step 0.0d0 1.0d0))
  ...)

(LET ((CHEB (ALLOCATE-CHEBYSHEV 40)))
  (UNWIND-PROTECT
      (PROGN
       (INITIALIZE-CHEBYSHEV CHEB CHEBYSHEV-STEP 0.0d0 1.0d0)
       ...)
    (FREE-CHEBYSHEV CHEB)))


|#
