;; Utility definitions
;; Liam Healy 2009-04-02 22:33:15EDT utility.lisp
;; Time-stamp: <2009-04-02 22:49:53EDT utility.lisp>
;; $Id: $

(in-package :gsl)

;;; http://common-lisp.net/project/bdb/qbook/mycl-util/api/function_005FMYCL-UTIL_003A_003AMAPPEND.html
(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun make-symbol-cardinal (name i &optional intern-package)
  (if (listp name)
      (make-symbol-cardinal (format nil "~{~a~^-~}" name) i intern-package)
      (if intern-package
	  (intern (format nil "~:@(~a~)~d" name i) intern-package)
	  (make-symbol (format nil "~:@(~a~)~d" name i)))))

(defun make-symbol-cardinals (name max-count &optional intern-package)
  (loop for i from 0 below max-count
       collect (make-symbol-cardinal name i intern-package)))
