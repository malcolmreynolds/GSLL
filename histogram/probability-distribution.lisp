;********************************************************
; file:        probability-distribution.lisp             
; description: Histogram probability distribution.       
; date:        Mon Jan  1 2007 - 17:51                   
; author:      Liam M. Healy                             
; modified:    Mon Jan  1 2007 - 22:22
;********************************************************
;;; $Id: $

(in-package :gsl)

(defclass histogram-pdf ()
  ((pointer
    :initarg :pointer :accessor pointer
    :documentation
    "A C pointer to the GSL representation of the histogram.")
   (number-of-bins :initarg :number-of-bins :accessor number-of-bins))
  (:documentation
   "A histogram, including bin boundaries and bin contents."))

(defun-gsl alloc-1 (object)
  "gsl_histogram_pdf_alloc"
  (((number-of-bins object) :size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defun-gsl alloc-2 (object)
  "gsl_histogram2d_pdf_alloc"
  (((number-of-bins object) :size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defmethod alloc ((object histogram-pdf))
  (histo-1d2d object alloc))

(defun-gsl free-1 (object)
  "gsl_histogram_pdf_free"
  (((pointer object) :pointer))
  :export nil
  :index free
  :c-return :void)

(defun-gsl free-2 (object)
  "gsl_histogram2d_pdf_free"
  (((pointer object) :pointer))
  :export nil
  :index free
  :c-return :void)

(defmethod free ((object histogram-pdf))
  (histo-1d2d object free))

(defun-gsl pdf-init-1 (pdf histogram)
  "gsl_histogram_pdf_init"
  (((pointer pdf) :pointer) ((pointer histogram) :pointer))
  :return ()
  :export nil
  :index pdf-init
  :documentation
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values.")

(defun-gsl pdf-init-2 (pdf histogram)
  "gsl_histogram2d_pdf_init"
  (((pointer pdf) :pointer) ((pointer histogram) :pointer))
  :return ()
  :export nil
  :index pdf-init
  :documentation
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values.")

(export 'pdf-init)
(defun pdf-init (pdf histogram)
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values."
  (histo-1d2d pdf pdf-init (histogram)))

(defun-gsl sample-1 (pdf value)
  "gsl_histogram_pdf_sample"
  (((pointer pdf) :pointer) (value :double))
  :c-return :double
  :export nil
  :index sample
  :documentation
  "Given a uniform random number between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample @math{s} is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where @math{i} is the index which satisfies 
   @math{sum[i] <=  r < sum[i+1]} and @math{delta} is 
   @math{(r - sum[i])/(sum[i+1] - sum[i])}.")

(defun-gsl sample-2 (pdf value)
  "gsl_histogram2d_pdf_sample"
  (((pointer pdf) :pointer) (value :double))
  :c-return :double
  :export nil
  :index sample
  :documentation
  "Given a uniform random number between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample @math{s} is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where @math{i} is the index which satisfies 
   @math{sum[i] <=  r < sum[i+1]} and @math{delta} is 
   @math{(r - sum[i])/(sum[i+1] - sum[i])}.")

(export 'sample)
(defun sample (pdf value)
  (histo-1d2d pdf sample (value)))
