(in-package #:compile-bench)

(defmacro m:when (test &body forms)
  `(if ,test (progn ,@forms) nil))

(defmacro m:unless (test &body forms)
  `(if ,test nil (progn ,@forms)))

(defmacro m:psetq (&rest pairs)
  (let ((valsyms (loop repeat (floor (length pairs) 2)
                       collect (gensym "VALUE"))))
    `(let (,@(loop for (var value) on pairs by #'cddr
                   for vsym in valsyms
                   collect `(,vsym ,value)))
       (setq ,@(loop for (var) on pairs by #'cddr
                     for vsym in valsyms
                     nconc (list var vsym)))
       nil)))

(defmacro setf-1 (place value)
  (etypecase place
    (symbol `(setq ,place ,value))
    ((cons (eql aref)) (cl:macroexpand-1 `(cl:setf ,place ,value)))))

(defmacro m:setf (&rest pairs)
  `(progn ,@(loop for (place value) on pairs by #'cddr
                  collect `(setf-1 ,place ,value))))

(defmacro m:do (&whole form varlist (endtest &body endforms) &body body)
  (let ((varlist
          (loop for thing in varlist
                if (symbolp thing)
                  collect (list thing nil)
                else
                  collect thing))
        (gloop (gensym "LOOP")) (gtest (gensym "TEST")))
    (multiple-value-bind (body decls)
        (alexandria:parse-body body :whole form)
      `(let (,@(loop for (var value) in varlist
                     collect (list var value)))
         ,@decls
         (tagbody
            (go ,gtest)
           ,gloop
            ,@body
            (m:psetq ,@(loop for (var . rest) in varlist
                             when (consp (cdr rest))
                               nconc (list var (second rest))))
           ,gtest
            (if ,endtest
                (progn ,@endforms)
                (go ,gloop)))))))

(defmacro m:dotimes ((var count &optional result) &body body)
  (alexandria:once-only (count)
    `(m:do ((,var 0 (1+ ,var)))
           ((>= ,var ,count) ,result)
       ,@body)))

(defmacro m:assert (test-form &optional places datum &rest args)
  ;; Signals a full error rather than continuable
  (declare (ignore places))
  `(m:unless ,test-form
     ,(if datum
          `(error ,datum ,@args)
          `(error "Assertion failed: ~a" ',test-form))))

(locally
    ;; make sbcl shut up about &optional &key.
    #+sbcl(declare (sb-ext:muffle-conditions style-warning))
  (defmacro m:with-output-to-string ((var &optional string
                                      &key (element-type ''character))
                                     &body body)
    (when string
      (error "~s argument to ~s is not supported"
             :string 'm:with-output-to-string))
    `(let ((,var
             (make-string-output-stream :element-type ,element-type)))
       ,@body
       (get-output-stream-string ,var))))
