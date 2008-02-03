;; Interpolation allocation, initialization, and freeing.
;; Liam Healy, Sun Nov  4 2007 - 17:24
;; Time-stamp: <2008-02-02 22:22:59EST interpolation.lisp>
;; $Id: $

(in-package :gsl)

;;; A spline is an interpolation that also stores the arrays xa and ya,
;;; so they need not be supplied on each call.

(defun-letm interpolation (type xa-or-size &optional ya)
  (list
   `(allocate-interpolation ,type ,(if ya `(dim0 ,ya) xa-or-size))
   'free-spline
   (when ya
     (lambda (symb)
       `(initialize-interpolation ,symb ,xa-or-size ,ya (dim0 ,ya))))))

;;; Interpolation
(defun-gsl allocate-interpolation (type size)
  "gsl_interp_alloc"
  ((type :pointer) (size :size))
  :c-return :pointer
  :export nil
  :index (letm interpolation)
  :documentation
  "Allocate an interpolation object of type for size data-points,
   and return the pointer to it.")

(defun-gsl initialize-interpolation (interpolation xa ya size)
  "gsl_interp_init"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (size :size))
  :documentation
  "Initialize the interpolation object @var{interp} for the
  data (@var{xa},@var{ya}) where @var{xa} and @var{ya} are arrays of size
  @var{size}.  The interpolation object (@code{gsl_interp}) does not save
  the data arrays @var{xa} and @var{ya} and only stores the static state
  computed from the data.  The @var{xa} data array is always assumed to be
  strictly ordered; the behavior for other arrangements is not defined.")

(defun-gsl free-interpolation (interpolation)
  "gsl_interp_free"
  ((interpolation :pointer))
  :c-return :void
  :export nil
  :index (letm interpolation)
  :documentation
  "Frees the interpolation object @var{interp}.")

(defun-letm spline (type xa-or-size &optional ya)
  (list
   `(allocate-spline ,type ,(if ya `(dim0 ,ya) xa-or-size))
   'free-spline
   (when ya
     (lambda (symb)
       `(initialize-spline ,symb ,xa-or-size ,ya (dim0 ,ya))))))

;;; Spline
(defun-gsl allocate-spline (type size)
  "gsl_spline_alloc"
  ((type :pointer) (size :size))
  :c-return :pointer
  :documentation
  "Allocate an interpolation object of type for size data-points,
   and return the pointer to it.")

(defun-gsl initialize-spline (interpolation xa ya size)
  "gsl_spline_init"
  ((interpolation :pointer) (xa :pointer) (ya :pointer) (size :size))
  :documentation
  "Initialize the interpolation object @var{interp} for the
  data (@var{xa},@var{ya}) where @var{xa} and @var{ya} are arrays of size
  @var{size}.  The interpolation object (@code{gsl_spline}) does not save
  the data arrays @var{xa} and @var{ya} and only stores the static state
  computed from the data.  The @var{xa} data array is always assumed to be
  strictly ordered; the behavior for other arrangements is not defined.")

(defun-gsl free-spline (interpolation)
  "gsl_spline_free"
  ((interpolation :pointer))
  :c-return :void
  :documentation
  "Frees the interpolation object @var{interp}.")
