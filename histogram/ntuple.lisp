;; N-tuples
;; Liam Healy Sat Feb  3 2007 - 12:53
;; Time-stamp: <2008-08-17 09:39:34EDT ntuple.lisp>
;; $Id$

(in-package :gsl)

;;; The basic definitions are here, but a smooth interface to CL 
;;; has not been created, and it has not been tested.  

;;; Writing a file
(defmfun create-ntuple (filename data size)
  "gsl_ntuple_create"
  ((filename :string) (data :pointer) (size sizet))
  :c-return :pointer
  :documentation			; FDL
  "Create a new write-only ntuple file filename for
  ntuples of size and return a pointer to the newly created
  ntuple struct.  Any existing file with the same name is truncated to
  zero length and overwritten.  A pointer to memory for the current ntuple
  row data must be supplied---this is used to copy ntuples
  in and out of the file.")

;;; Reading a file
(defmfun open-ntuple (filename data size)
  "gsl_ntuple_create"
  ((filename :string) (data :pointer) (size sizet))
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
  "Closes the ntuple file ntupe and frees its
   associated allocated memory.")

;;; Histogramming ntuple values
(defmfun project-ntuple (histogram ntuple value-function select-function)
  "gsl_ntuple_project"
  ((histogram :pointer) (ntuple :pointer)
   (value-function :pointer) (select-function :pointer))
  :documentation			; FDL
  "Update the histogram the ntuple
   using the functions value-function and select-function. For each
   ntuple row where the selection function select-function is non-zero the
   corresponding value of that row is computed using the function
   value-function and added to the histogram.  Those ntuple rows where
   select-function returns zero are ignored.  New entries are added to
   the histogram, so subsequent calls can be used to accumulate further
   data in the same histogram.") 

;;; Callback definitions

(export '(def-ntuple-select-function def-ntuple-value-function))

(defmacro def-ntuple-select-function (name arg)
  ;; FDL
  "The selection function determines which ntuple rows are selected
   for histogramming. The struct component function should return a
   non-zero value for each ntuple row that is to be included in the
   histogram. "
  `(def-single-function ,name ,arg :int :pointer))

(defmacro def-ntuple-value-function (name arg)
  ;; FDL
  "The value function computes scalar values for those ntuple rows
   selected by the selection function which should return the value
   to be added to the histogram."
  `(def-single-function ,name ,arg :double :pointer))
