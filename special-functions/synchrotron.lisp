;; Synchrotron functions
;; Liam Healy, Mon May  1 2006 - 22:29
;; Time-stamp: <2008-02-16 22:48:24EST synchrotron.lisp>
;; $Id: $

(in-package :gsl)

(defmfun synchrotron-1 (x)
  "gsl_sf_synchrotron_1_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The first synchrotron function x \int_x^\infty dt K_{5/3}(t)} for x >= 0.")

(defmfun synchrotron-2 (x)
  "gsl_sf_synchrotron_2_e" ((x :double) (ret sf-result))
  :documentation			; FDL
  "The second synchrotron function x K_{2/3}(x)} for x >= 0.")

;;;;****************************************************************************
;;;; Examples and unit test
;;;;****************************************************************************

#|
(make-tests synchrotron
  (synchrotron-1 4.0d0)
  (synchrotron-2 4.0d0))
|#

(LISP-UNIT:DEFINE-TEST SYNCHROTRON
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.052827396697912476d0 4.825849878208132d-14)
   (MULTIPLE-VALUE-LIST (SYNCHROTRON-1 4.0d0)))
  (LISP-UNIT::ASSERT-NUMERICAL-EQUAL
   (LIST 0.04692320582614684d0 6.854449168174663d-14)
   (MULTIPLE-VALUE-LIST (SYNCHROTRON-2 4.0d0))))
