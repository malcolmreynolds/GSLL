(defpackage gsll-util
  (:nicknames :gslu)
  (:use :common-lisp :gsll :cl-utilities)
  (:export
   ;; matrix-loops.lisp
   :do-vector
   :do-matrix
   :do-matrix-up-triangular
   :do-matrix-rows
   :do-matrix-cols
   :do-matrix-diag

   ;; concat.lisp
   :vcat
   :mcat-ver
   :mcat-hor

   ;; slice.lisp
   :mslice
   :vslice))