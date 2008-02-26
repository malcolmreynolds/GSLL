;; Histogram probability distribution.
;; Liam Healy, Mon Jan  1 2007 - 17:51
;; Time-stamp: <2008-02-17 18:39:49EST probability-distribution.lisp>
;; $Id$

(in-package :gsl)

(defclass histogram-pdf ()
  ((pointer
    :initarg :pointer :accessor pointer
    :documentation
    "A C pointer to the GSL representation of the histogram.")
   (number-of-bins :initarg :number-of-bins :accessor number-of-bins))
  (:documentation
   "A histogram, including bin boundaries and bin contents."))

(defmfun alloc-1 (object)
  "gsl_histogram_pdf_alloc"
  (((number-of-bins object) size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defmfun alloc-2 (object)
  "gsl_histogram2d_pdf_alloc"
  (((number-of-bins object) size))
  :export nil
  :index alloc
  :c-return (cr :pointer)
  :return ((assign-pointer object cr)))

(defmethod alloc ((object histogram-pdf))
  (histo-1d2d object alloc))

(defmfun free-1 (object)
  "gsl_histogram_pdf_free"
  (((pointer object) :pointer))
  :export nil
  :index free
  :c-return :void)

(defmfun free-2 (object)
  "gsl_histogram2d_pdf_free"
  (((pointer object) :pointer))
  :export nil
  :index free
  :c-return :void)

(defmethod free ((object histogram-pdf))
  (histo-1d2d object free))

(defmfun pdf-init-1 (pdf histogram)
  "gsl_histogram_pdf_init"
  (((pointer pdf) :pointer) ((pointer histogram) :pointer))
  :return ()
  :export nil
  :index pdf-init
  :documentation			; FDL
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values.")

(defmfun pdf-init-2 (pdf histogram)
  "gsl_histogram2d_pdf_init"
  (((pointer pdf) :pointer) ((pointer histogram) :pointer))
  :return ()
  :export nil
  :index pdf-init
  :documentation			; FDL
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values.")

(export 'pdf-init)
(defun pdf-init (pdf histogram)		; FDL
  "Initialize the probability distribution pdf with the contents
   of the histogram.  If any of the bins are negative then an
   EDOM error is signalled because a probability distribution
   cannot contain negative values."
  (histo-1d2d pdf pdf-init (histogram)))

(defmfun sample-1 (pdf value)
  "gsl_histogram_pdf_sample"
  (((pointer pdf) :pointer) (value :double))
  :c-return :double
  :export nil
  :index sample
  :documentation			; FDL
  "Given a uniform random number between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample s is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where i is the index which satisfies 
   sum[i] <=  r < sum[i+1] and delta is 
   (r - sum[i])/(sum[i+1] - sum[i]).")

(defmfun sample-2 (pdf value)
  "gsl_histogram2d_pdf_sample"
  (((pointer pdf) :pointer) (value :double))
  :c-return :double
  :export nil
  :index sample
  :documentation			; FDL
  "Given a uniform random number between zero and one,
   compute a single random sample from the probability distribution
   'pdf.  The algorithm used to compute the sample s is given by
   s = range[i] + delta * (range[i+1] - range[i])
   where i is the index which satisfies
   sum[i] <=  r < sum[i+1] and delta is 
   (r - sum[i])/(sum[i+1] - sum[i]).")
