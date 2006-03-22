;********************************************************
; file:        cffi-array.lisp                           
; description: Addition to CFFI                          
; date:        Tue Mar 21 2006 - 18:57                   
; author:      Liam M. Healy                             
; modified:    Tue Mar 21 2006 - 19:02
;********************************************************
;;; $Id: $

;;; http://article.gmane.org/gmane.lisp.cffi.devel/283

(in-package :cffi)

(defun indexes-to-row-major-index (dimensions &rest subscripts)
  (apply #'+ (maplist #'(lambda (x y)
     (* (car x) (apply #'* (cdr y))))
        subscripts
        dimensions)))

(defun row-major-index-to-indexes (index dimensions)
  (loop with idx = index
 with rank = (length dimensions)
 with indexes = (make-list rank)
 for dim-index from (- rank 1) downto 0 do
 (setf (values idx (nth dim-index indexes)) (floor idx (nth dim-index
dimensions)))
 finally (return indexes)))

(defun lisp-array-to-foreign (array ptr cffi-type dimensions)
  "Copy elements from a Lisp array to ptr."
  (loop with foreign-type-size = (foreign-type-size cffi-type)
	with size = (reduce #'* dimensions)
	for i from 0 below size
	for offset = (* i foreign-type-size)
	for element = (apply #'aref array
			     (row-major-index-to-indexes i dimensions))
        do (setf (mem-ref ptr cffi-type offset) element)))

(defun foreign-array-to-lisp (ptr cffi-type dimensions)
  "Copy elements from ptr into a Lisp array.
If ptr is a null pointer, returns nil."
  (unless (null-pointer-p ptr)
    (let ((array (make-array dimensions)))
      (loop with foreign-type-size = (foreign-type-size cffi-type)
     with size = (reduce #'* dimensions)
     for i from 0 below size
     for offset = (* i foreign-type-size)
            for element = (mem-ref ptr cffi-type offset)
            do (setf (apply #'aref array (row-major-index-to-indexes i
dimensions)) element))
      array)))

(defun foreign-array-alloc (array cffi-type dimensions)
  "Allocate a foreign array containing the elements of lisp array.
The foreign array must be freed with foreign-array-free."
  (check-type array array)
  (let ((ptr (foreign-alloc cffi-type :count (reduce #'* dimensions))))
    (lisp-array-to-foreign array ptr cffi-type dimensions)
    ptr))

(defun foreign-array-free (ptr)
  "Free a foreign array allocated by foreign-array-alloc."
  (foreign-free ptr))

(defmacro with-foreign-array
    ((var lisp-array cffi-type dimensions) &body body)
  "Bind var to a foreign array containing lisp-array elements in body."
  `(with-foreign-pointer (,var (* (reduce #'* ,dimensions)
    (foreign-type-size ,cffi-type)))
    (lisp-array-to-foreign ,lisp-array ,var ,cffi-type ,dimensions)
    ,@body))

(defun foreign-aref (ptr cffi-type dimensions &rest indexes)
  (let ((offset (* (foreign-type-size cffi-type)
     (apply #'indexes-to-row-major-index dimensions indexes))))
    (mem-ref ptr cffi-type offset)))

(defun (setf foreign-aref) (value ptr cffi-type dimensions &rest indexes)
  (let ((offset (* (foreign-type-size cffi-type)
     (apply #'indexes-to-row-major-index dimensions indexes))))
    (setf (mem-ref ptr cffi-type offset) value)))
