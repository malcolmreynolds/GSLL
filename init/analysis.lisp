;; Analysis of interface status
;; Liam Healy 2008-11-16 16:17:14EST analysis.lisp
;; Time-stamp: <2008-11-16 17:21:10EST analysis.lisp>
;; $Id: $

;;; These definitions are to monitor the status of the GSLL interface
;;; with respect to the GSL library for the purpose of directing
;;; development of GSLL.  Therefore, they are not loaded in the
;;; system.  They require the port system.

(in-package :gsl)

(defparameter *gsl-library-path*
  (namestring
   (cffi::foreign-library-handle (cffi::get-foreign-library 'cl-user::libgsl))))

(defparameter *gsl-library-symbols*
  (with-open-stream
      (instream
       (port:pipe-input			; linux commands
	"/usr/bin/nm" "-D" "--defined-only" "-g" *gsl-library-path*))
    (loop for symbol = (read-line instream nil nil)
       while symbol
       collect (subseq symbol 19)))
  "All the external symbols in the GSL library.")

(defun in-gsl (string)
  "The symbol occurs in the GSL library."
  (find string *gsl-library-symbols* :test 'string-equal))

(defparameter *gsll-defined-symbols*
  (loop for gslsymb being the hash-key of *gsl-symbol-equivalence* collect gslsymb)
  "All the exported definitions in GSLL.")

(defun name-special-function-no-error (string)
  "Names a function for a special function that does not return an error estimate."
  (and (search "gsl_sf_" string :test 'string-equal)
       (not (string-equal (subseq string (- (length string) 2)) "_e"))
       (in-gsl (concatenate 'string string "_e"))))

(defun defined-without-strings (defined-list &rest strings)
  "A list of definitions in GSL where none of the given strings occurs."
  (loop for string in strings
     for from = defined-list then list
     for list = (remove-if (lambda (x) (search string x :test 'string-equal)) from)
     finally (return list)))

(defun gsll-defined-with-string (string)
  "A list of definitions in GSLL where the given strings occurs."
  (remove-if-not
   (lambda (x) (search string x :test 'string-equal))
   *gsll-defined-symbols*))

;;; Step through the list of GSL definitions, removing those things
;;; we're not going to port.

(defparameter *target-port-1*
  (defined-without-strings *gsl-library-symbols* "fscan" "fread" "print" "view"))

(defparameter *target-port-2*
  (remove-if 'name-special-function-no-error *target-port-1*))

;;; Step through the list of GSLL definitions, removing those things
;;; that we're not advertising we've ported, but did port anyway for
;;; internal use or for whatever reason.

(defparameter *have-port-1*
  (defined-without-strings *gsll-defined-symbols* "fscan" "fread" "print" "view"))
