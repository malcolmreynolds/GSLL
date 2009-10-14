(asdf:defsystem "gsll-util"
    :name "gsll-util"
    :description "Utility functions for the GNU Scientific Library for Lisp."
    :version "0"
    :author "Malcolm Reynolds"
    :licence "LLGPL v3, FDL"
    :depends-on (gsll)
    :components
    ((:module utils
	      :components
	      ((:file "init")
	       (:file "matrix-loops" :depends-on ("init"))
	       (:file "concat" :depends-on ("init" "matrix-loops"))))))
