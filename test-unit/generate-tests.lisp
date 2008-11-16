;; Make tests from examples.
;; Liam Healy 2008-09-07 21:00:48EDT generate-tests.lisp
;; Time-stamp: <2008-11-16 12:10:07EST generate-tests.lisp>
;; $Id: $

;;; Througout the GSLL interface definition files are #'save-test
;;; forms.  These serve to define both examples and tests.  Getting an
;;; example involves calling (examples) to get a list of names, then
;;; the calling the function with a particular name to get the
;;; examples, e.g.
;;; (examples)
;;; (examples 'matrix-m+)

;;; To do all the tests,
;;; (lisp-unit:run-tests)

;;; The files that define the tests are in tests/.  These files are
;;; generated automatically and checked into the repository; they
;;; shouldn't be changed very often.  Rarely, it may be necessary to
;;; generate such a file.  In this case, #'write-test-to-file recreates
;;; the file, e.g.
;;; (write-test-to-file 'matrix-m+ "test/")

(in-package :gsl)

(defun numerical-serialize (form)
  (if (typep form 'list)
      (cons 'list (mapcar #'numerical-serialize form))
      form))

;;; (make-test '(legendre-conicalP-half 3.5d0 10.0d0))
(defun make-test (form)
  "Make a test for lisp-unit."
  (let ((vals (multiple-value-list (ignore-errors (eval form)))))
    (if (typep (second vals) 'condition)
	`(lisp-unit::assert-error
	  ',(type-of (second vals))
	  ,form)
	`(lisp-unit::assert-numerical-equal
	  ,(numerical-serialize vals)
	  (multiple-value-list ,form)))))

(defun create-test (test-name)
  "Find the saved test by name and create it, with the generated results."
  (append
   `(lisp-unit:define-test ,test-name)
   (mapcar #'make-test (getf *all-generated-tests* test-name))))

(defun write-test-to-file (test path)
  "Write the test to a file with the same name under path.
   Use this function with caution; it will replace an existing
   test file and thus the opportunity for regression test will be lost."
  (let ((pathname (merge-pathnames (format nil "~(~a~).lisp" test) path))
	(*read-default-float-format* 'single-float))
    (with-open-file
	(stream pathname :direction :output :if-exists :rename)
      (format
       stream
       ";; Regression test ~a for GSLL, automatically generated~%~%" test)
      (format stream "(in-package :gsl)~%~%")
      (format t "Writing test ~a to file ~a~&" test pathname)
      (format stream "~s~%~%" (create-test test)))))


;;; This is commented out because it shouldn't normally be run.  It
;;; will regenerate all tests, so there will be no regression tests to
;;; previous versions.  DON'T FORGET THE TRAILING SLASH ON THE PATH
#+(or)
(defun write-tests (path)
  "Write all the tests to the appropriate file."
  (iter:iter
    (iter:for (key val) on *all-generated-tests* by #'cddr)
    (write-test-to-file key path)))
