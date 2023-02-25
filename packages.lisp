(macrolet ((defmock ()
             `(defpackage #:mock-macros
                (:use #:cl)
                #+clisp (:nicknames "M")
                (:shadow ,@(loop for s being the external-symbols of :CL
                                 when (macro-function s)
                                   collect s))
                (:export ,@(loop for s being the external-symbols of :CL
                                 collect s)))))
  (defmock))

(defpackage #:compile-bench
  (:use #:cl)
  #+clisp (:nicknames "B")
  (:shadow #:compile)
  #-clisp
  (:local-nicknames (#:m #:mock-macros))
  (:export #:defbench)
  (:export #:*compiler* #:time-benchmark-compilation))
