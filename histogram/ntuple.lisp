;; N-tuples
;; Liam Healy Sat Feb  3 2007 - 12:53
n;; Time-stamp: <2009-07-04 22:20:15EDT ntuple.lisp>

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
  "gsl_ntuple_open"
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
  (((mpointer histogram) :pointer) (ntuple :pointer)
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
			      (MPOINTER HISTOGRAM) :POINTER NTUPLE
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

(defvar *ntuple-example-scale* 1.5d0)

(defun ntuple-example-values (i)
  (let ((xi (/ (+ i 1.5d0))))
    (values xi (expt xi 2) (expt xi 3))))

(defun make-ntuple-example-data (&optional (filename "test.dat"))
  (cffi:with-foreign-object (data 'ntuple-data)
    (let ((ntuple (create-ntuple filename data 'ntuple-data))
	  (answer (make-array 100 :element-type 'fixnum :initial-element 0)))
      (dotimes (row 1000)
	(multiple-value-bind (xi yi zi)
	    (ntuple-example-values row)
	  (setf (cffi:foreign-slot-value data 'ntuple-data 'num) row
		(cffi:foreign-slot-value data 'ntuple-data 'x) xi
		(cffi:foreign-slot-value data 'ntuple-data 'y) yi
		(cffi:foreign-slot-value data 'ntuple-data 'z) zi)
	  (when (< (* xi *ntuple-example-scale*) 0.1d0)
	    (incf (aref answer (truncate (* 100 *ntuple-example-scale* (+ xi yi zi)))))))
	(bookdata-ntuple ntuple))
      (close-ntuple ntuple)
      answer)))

(defun ntuple-example-read (&optional (filename "test.dat"))
  (cffi:with-foreign-object (data 'ntuple-data)
    (let ((ntuple (open-ntuple filename data 'ntuple-data)))
      (dotimes (row 1000)
	(multiple-value-bind (xi yi zi)
	    (ntuple-example-values row)
	  (read-ntuple ntuple)
	  (unless
	      (and (eql row (cffi:foreign-slot-value data 'ntuple-data 'num))
		   (eql xi (cffi:foreign-slot-value data 'ntuple-data 'x))
		   (eql yi (cffi:foreign-slot-value data 'ntuple-data 'y))
		   (eql zi (cffi:foreign-slot-value data 'ntuple-data 'z)))
	    (error "Ntuple test failed."))))
      (close-ntuple ntuple)
      T)))

(defun ntuple-example-sel-func (ntuple-data)
  (< (* (cffi:foreign-slot-value ntuple-data 'ntuple-data 'x)
	*ntuple-example-scale*)
     0.1d0))

(defun ntuple-example-val-func (ntuple-data)
  (< (* (+ (cffi:foreign-slot-value ntuple-data 'ntuple-data 'x)
	   (cffi:foreign-slot-value ntuple-data 'ntuple-data 'y)
	   (cffi:foreign-slot-value ntuple-data 'ntuple-data 'z))
	*ntuple-example-scale*)
     0.1d0))

(defun ntuple-example-histogramming (&optional (filename "test.dat"))
  (cffi:with-foreign-object (data 'ntuple-data)
    (let ((histo (make-histogram 100))
	  (ntuple (open-ntuple filename data 'ntuple-data))
	  (answer (make-ntuple-example-data filename)))
      (set-ranges-uniform histo 0.0d0 1.0d0)
      (project-ntuple
       histo ntuple 'ntuple-example-sel-func 'ntuple-example-val-func)
      (close-ntuple ntuple)
      ;; Check the histogram to see if it matches ntuple-example-values
      (dotimes (row 100)
	(unless (eql (maref histo row) (aref answer row))
	  (error "Histogramming ntuple test failed for row=~d." row))))))
