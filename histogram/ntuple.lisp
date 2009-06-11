;; N-tuples
;; Liam Healy Sat Feb  3 2007 - 12:53
;; Time-stamp: <2009-06-10 22:52:05EDT ntuple.lisp>
;; $Id$

(in-package :gsl)

;;; /usr/include/gsl/gsl_ntuple.h

;;; Writing a file
(defmfun create-ntuple (filename data foreign-type)
  "gsl_ntuple_create"
  ((filename :string) (data :pointer)
   ((cffi:foreign-type-size foreign-type) sizet))
  :c-return :pointer
  :documentation			; FDL
  "Create a new write-only ntuple file filename for
  ntuples of size and return a pointer to the newly created
  ntuple struct.  Any existing file with the same name is truncated to
  zero length and overwritten.  A pointer to memory for the current ntuple
  row data must be supplied---this is used to copy ntuples
  in and out of the file.")

;;; Reading a file
(defmfun open-ntuple (filename data foreign-type)
  "gsl_ntuple_create"
  ((filename :string) (data :pointer)
   ((cffi:foreign-type-size foreign-type) sizet))
  :c-return :pointer
  :documentation			; FDL
  "Open an existing ntuple file filename for reading
  and return a pointer to a corresponding ntuple struct. The ntuples in
  the file must have size size.  A pointer to memory for the current
  ntuple row data must be supplied---this is used to copy
  ntuples in and out of the file.")

;;; Writing ntuples
(defmfun write-ntuple (ntuple)
  "gsl_ntuple_write"
  ((ntuple :pointer))
  :documentation			; FDL
  "Write the current ntuple ntuple->ntuple_data of
   size ntuple->size to the corresponding file.")

(defmfun bookdata-ntuple (ntuple)
  "gsl_ntuple_bookdata"
  ((ntuple :pointer))
  :documentation			; FDL
  "A synonym for #'write-ntuple}.")

;;; Reading ntuples
(defmfun read-ntuple (ntuple)
  "gsl_ntuple_read"
  ((ntuple :pointer))
  :documentation			; FDL
  "Read the current row of the ntuple file and stores the value.")

;;; Closing file
(defmfun close-ntuple (ntuple)
  "gsl_ntuple_close"
  ((ntuple :pointer))
  :documentation			; FDL
  "Closes the ntuple file and frees its associated allocated memory.")

;;; Histogramming ntuple values
#|
;;; This the one function in GSL that requires two cbstructs.  Rather
;;; than modify the entire defmfun apparatus to handle it, we just
;;; manually build the expansion below.

(defmfun project-ntuple (histogram ntuple value-function select-function)
  "gsl_ntuple_project"
  ((histogram :pointer) (ntuple :pointer)
   (valfn :pointer) (selfn :pointer))
  :callbacks
  ;; (valfn fnstruct nil (value-function :pointer (:input :pointer) :slug))
  ;; Need a second cbstruct definition:
  (selfn fnstruct nil (select-function :pointer (:input :pointer) :slug))
  ;;:callback-dynamic (nil (value-function))
  :callback-dynamic (nil (select-function))
  :documentation			; FDL
  "Update the histogram from the ntuple
   using the functions value-function and select-function. For each
   ntuple row where the selection function select-function is non-zero the
   corresponding value of that row is computed using the function
   value-function and added to the histogram.  Those ntuple rows where
   select-function returns zero are ignored.  New entries are added to
   the histogram, so subsequent calls can be used to accumulate further
   data in the same histogram.")
|# 

(PROGN
  (DEFUN PROJECT-NTUPLE (HISTOGRAM NTUPLE VALUE-FUNCTION SELECT-FUNCTION)
    (DECLARE (SPECIAL PROJECT-NTUPLE-DYNFN0 PROJECT-NTUPLE-DYNFN1))
    "Update the histogram from the ntuple
   using the functions value-function and select-function. For each
   ntuple row where the selection function select-function is non-zero the
   corresponding value of that row is computed using the function
   value-function and added to the histogram.  Those ntuple rows where
   select-function returns zero are ignored.  New entries are added to
   the histogram, so subsequent calls can be used to accumulate further
   data in the same histogram."
    (WITH-FOREIGN-OBJECTS ((VALFN 'FNSTRUCT) (SELFN 'fnstruct))
      (SETF PROJECT-NTUPLE-DYNFN0
	    (MAKE-COMPILED-FUNCALLABLE
	     VALUE-FUNCTION
	     '(SELECT-FUNCTION :POINTER (:INPUT :POINTER) :SLUG) NIL (LIST))
	    PROJECT-NTUPLE-DYNFN1
	    (MAKE-COMPILED-FUNCALLABLE
	     SELECT-FUNCTION
	     '(SELECT-FUNCTION :POINTER (:INPUT :POINTER) :SLUG) NIL (LIST)))
      (SET-CBSTRUCT VALFN 'FNSTRUCT NIL (LIST 'FUNCTION 'PROJECT-NTUPLE-CBFN0))
      (SET-CBSTRUCT SELFN 'FNSTRUCT NIL (LIST 'FUNCTION 'PROJECT-NTUPLE-CBFN1))
      (LET ((CRETURN
	     (FOREIGN-FUNCALL "gsl_ntuple_project" :POINTER
			      HISTOGRAM :POINTER NTUPLE
			      :POINTER VALFN :POINTER SELFN
			      :INT)))
	(CHECK-GSL-STATUS CRETURN 'PROJECT-NTUPLE)
	(VALUES))))
  (DEFMCALLBACK PROJECT-NTUPLE-CBFN0 PROJECT-NTUPLE-DYNFN0
    (VALUE-FUNCTION :POINTER (:INPUT :POINTER) :SLUG))
  (DEFMCALLBACK PROJECT-NTUPLE-CBFN1 PROJECT-NTUPLE-DYNFN1
    (SELECT-FUNCTION :POINTER (:INPUT :POINTER) :SLUG))
  (MAP-NAME 'PROJECT-NTUPLE "gsl_ntuple_project")
  (EXPORT 'PROJECT-NTUPLE))

;;; Callback definitions

#|
  ;; FDL
  "The selection function determines which ntuple rows are selected
   for histogramming. The struct component function should return a
   non-zero value for each ntuple row that is to be included in the
   histogram. "
  ;; FDL
  "The value function computes scalar values for those ntuple rows
   selected by the selection function which should return the value
   to be added to the histogram."
|#

;;; Examples and unit test
;;; See gsl-1.11/ntuple/test.c

(defcstruct ntuple-data
  (num :int)
  (x :double)
  (y :double)
  (z :double))

(defun make-ntuple-example-data ()
  (cffi:with-foreign-object (data 'ntuple-data)
    (let ((ntuple (create-ntuple "test.dat" data 'ntuple-data)))
      (dotimes (row 1000)
	(let* ((xi (/ (+ row 1.5d0)))
	       (yi (expt xi 2))
	       (zi (expt xi 3)))
	  (setf (cffi:foreign-slot-value data 'ntuple-data 'num) row
		(cffi:foreign-slot-value data 'ntuple-data 'x) xi
		(cffi:foreign-slot-value data 'ntuple-data 'y) yi
		(cffi:foreign-slot-value data 'ntuple-data 'z) zi))
	(bookdata-ntuple ntuple))
      (close-ntuple ntuple))))
