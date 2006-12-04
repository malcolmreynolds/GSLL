;********************************************************
; file:        utility.lisp                              
; description: Utility definitions                       
; date:        Sun Dec  3 2006 - 10:21                   
; author:      Liam M. Healy                             
; modified:    Sun Dec  3 2006 - 10:22
;********************************************************
;;; $Id: $

(in-package :gsl)

(defun make-symbol-from-strings (&rest strings)
  "Construct a symbol by concatenating words with hyphens."
  (intern (format nil "~{~:@(~a~)~^-~}" (mapcar #'string strings))))

(defparameter *gsl-prefix* 'gsl)
