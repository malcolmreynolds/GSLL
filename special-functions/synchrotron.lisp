;; Synchrotron functions
;; Liam Healy, Mon May  1 2006 - 22:29
;; Time-stamp: <2008-10-25 11:30:06EDT synchrotron.lisp>
;; $Id$

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

(save-test synchrotron
  (synchrotron-1 4.0d0)
  (synchrotron-2 4.0d0))

