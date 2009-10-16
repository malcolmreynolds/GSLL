(asdf:defsystem "gsll-util"
    :name "gsll-util"
    :description "Utility functions for the GNU Scientific Library for Lisp."
    :version "0"
    :author "Malcolm Reynolds"
    :licence "LLGPL v3, FDL"
    :depends-on (gsll cl-utilities)
    :components
    ((:module utils
	      :components
	      ((:file "init")
	       (:file "utils" :depends-on ("init"))
	       (:file "matrix-loops" :depends-on ("init"))
	       (:file "concat" :depends-on ("init" "matrix-loops" "utils"))
	       (:file "slice" :depends-on ("init" "matrix-loops"))))))
