(defpackage #:compile-bench.crc40
  (:use #:cl)
  #-clisp
  (:local-nicknames (#:b #:compile-bench)
                    (#:m #:mock-macros))
  (:export #:crc40))

(in-package #:compile-bench.crc40)

(b:defbench (crc40
             (1 calculate-crc40 10))

  (defun crc-division-step (bit rmdr poly msb-mask)
    (declare (type (signed-byte 56) rmdr poly msb-mask)
	     (type bit bit))
    ;; Shift in the bit into the LSB of the register (rmdr)
    (let ((new-rmdr (logior bit (* rmdr 2))))
      ;; Divide by the polynomial, and return the new remainder
      (if (zerop (logand msb-mask new-rmdr))
	  new-rmdr
	  (logxor new-rmdr poly))))

  (defun compute-adjustment (poly n)
    (declare (type (signed-byte 56) poly)
	     (fixnum n))
    ;; Precompute X^(n-1) mod poly
    (let* ((poly-len-mask (ash 1 (1- (integer-length poly))))
	   (rmdr (crc-division-step 1 0 poly poly-len-mask)))
      (m:dotimes (k (- n 1))
        (m:setf rmdr (crc-division-step 0 rmdr poly poly-len-mask)))
      rmdr))

  (defun calculate-crc40 (iterations)
    (declare (fixnum iterations))
    (let ((crc-poly 1099587256329)
	  (len 3014633)
	  (answer 0))
      (m:dotimes (k iterations)
        (declare (fixnum k))
        (m:setf answer (compute-adjustment crc-poly len)))
      answer)))
