(in-package #:compile-bench)

(defmacro ITUs (form)
  (alexandria:with-gensyms (start)
    `(let ((,start (get-internal-run-time)))
       ,form
       (- (get-internal-run-time) ,start))))

(defmacro runtime (form)
  `(/ (float (ITUs ,form) 1d0) internal-time-units-per-second))

;;;

(defvar *compiler*)

(defun compile (lambda-expression)
  (funcall *compiler* lambda-expression))

(defun build-benchmark (forms)
  ;; Execute one at a time as if by LOAD.
  (loop for form in forms
        do (funcall (compile `(lambda () (progn ,form))))))

(defun benchmark-compilation-ITUs (forms niters)
  ;; First make sure everything's built, so as to avoid unknown function
  ;; warnings and the like.
  (build-benchmark forms)
  ;; Now just do it repeatedly.
  (ITUs (loop repeat niters do (build-benchmark forms))))

(defun benchmark-compilation-runtime (forms niters)
  (/ (float (benchmark-compilation-ITUs forms niters) 1d0)
     internal-time-units-per-second))

;;;

(defvar *compilation-iterations*)

(defclass benchmark ()
  ((%forms :initarg :forms :reader forms)
   (%calls :initarg :calls :reader calls)))

(defgeneric bench (benchmark))

(defmethod bench ((benchmark benchmark))
  (list* (cons 'compile
               (benchmark-compilation-runtime
                (forms benchmark) *compilation-iterations*))
         (loop for (niters fname . args) in (calls benchmark)
               for f = (fdefinition fname)
               ;;do (write (list* fname args) :length 2 :level 2)
               ;;   (terpri) (finish-output)
               collect (cons fname (runtime
                                    (loop repeat niters
                                          do (apply f args)))))))

(defmacro defbench ((name &rest calls) &body definitions)
  `(defparameter ,name
     (make-instance 'benchmark
       :forms ',definitions
       :calls ',calls)))
