;; Utility definitions
;; Liam Healy, Sun Dec  3 2006 - 10:21
;; Time-stamp: <2008-02-16 10:35:30EST utility.lisp>
;; $Id$

(in-package :gsl)

(defun make-symbol-from-strings (&rest strings)
  "Construct a symbol by concatenating words with hyphens."
  (intern (format nil "~{~:@(~a~)~^-~}" (mapcar #'string strings))))

(defparameter *gsl-prefix* 'gsl)
