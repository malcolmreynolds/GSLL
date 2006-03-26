;********************************************************
; file:        dilogarithm.lisp                          
; description: Dilogarithm                               
; date:        Fri Mar 17 2006 - 18:44                   
; author:      Liam M. Healy
; modified:    Sat Mar 25 2006 - 22:11
;********************************************************

(in-package :gsl)

;;; dilog merge complex and real
(defun dilogarithm (x)
  "The dilogarithm."
  (etypecase x
    (double-float
     (funcall
      (defun-gsl :lambda ((x :double)) 
	"gsl_sf_dilog_e"
	:return (sf-result))
      x))
    (complex
     (multiple-value-bind (re re-err im im-err)
	 (funcall
	  ;; returns two gsl_sf_result
	  (defun-gsl :lambda
	      ((radius :double) (angle :double))
	    "gsl_sf_complex_dilog_e"
	    :return (sf-result sf-result))
	  (abs x)
	  (phase x))
       (values 
	(complex re im)
	(complex re-err im-err))))))
