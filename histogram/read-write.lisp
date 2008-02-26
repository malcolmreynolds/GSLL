;; Reading and writing histograms.
;; Liam Healy 2008-02-17 17:11:23EST read-write.lisp
;; Time-stamp: <2008-02-17 17:13:15EST read-write.lisp>
;; $Id$

(in-package :gsl)

(defmfun write-binary-1 (object stream)
  "gsl_histogram_fwrite"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index write-binary)

(defmfun write-binary-2 (object stream)
  "gsl_histogram2d_fwrite"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index write-binary)

(defmethod write-binary ((object histogram) stream)
  (histo-1d2d object write-binary (stream)))

(defmfun read-binary-1 (object stream)
  "gsl_histogram_fread"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index read-binary)

(defmfun read-binary-2 (object stream)
  "gsl_histogram2d_fread"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index read-binary)

(defmethod read-binary ((object histogram) stream)
  (histo-1d2d object read-binary (stream)))

(defmfun write-formatted-1 (object stream format)
  "gsl_histogram_fprintf"
  ((stream :pointer) ((pointer object) :pointer)
   ((first format) :string) ((second format) :string))
  :export nil
  :index write-formatted)

(defmfun write-formatted-2 (object stream format)
  "gsl_histogram2d_fprintf"
  ((stream :pointer) ((pointer object) :pointer)
   ((first format) :string) ((second format) :string))
  :export nil
  :index write-formatted)

(defmethod write-formatted ((object histogram) stream format)
  ;; FDL
  "This function writes the ranges and bins of the histogram
   line-by-line to the stream using the format specifiers
   (first format) for range format and (second format) for bin format.
   These should be one of the %g, %e or %f C formats for floating point
   numbers.  The histogram output is formatted in three columns,
   and the columns are separated by spaces,
   like this,
   range[0] range[1] bin[0]
   range[1] range[2] bin[1]
   range[2] range[3] bin[2]
   ....
   range[n-1] range[n] bin[n-1]
   The values of the ranges are formatted using range format and the
   value of the bins are formatted using bin format.  Each line
   contains the lower and upper limit of the range of the bins and the
   value of the bin itself.  Since the upper limit of one bin is the lower
   limit of the next there is duplication of these values between lines but
   this allows the histogram to be manipulated with line-oriented tools."
  (histo-1d2d object write-formatted (stream format)))

(defmfun read-formatted-1 (object stream format)
  "gsl_histogram_fscanf"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index read-formatted)

(defmfun read-formatted-2 (object stream format)
  "gsl_histogram2d_fscanf"
  ((stream :pointer) ((pointer object) :pointer))
  :export nil
  :index read-formatted)

(defmethod read-formatted ((object histogram) stream format)
  ;; FDL
  "Read formatted data from the stream into the histogram.  The
   data is assumed to be in the three-column format used by
   #'write-formatted.  The histogram must be preallocated with
   the correct length since the function uses the size of the
   histogram to determine how many numbers to read.  The
   argument 'format is ignored."
  (histo-1d2d object read-formatted (stream format)))
