(in-package #:web3/crypto)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare keccak256 (types:Bytes -> types:Bytes))
  (define (keccak256 data)
    "Compute Keccak-256 hash of data"
    (lisp types:Bytes (data)
      (cl:let* ((digest (ironclad:make-digest :keccak/256))
                (vec (cl:make-array (cl:length data)
                                    :element-type '(cl:unsigned-byte 8)
                                    :initial-contents data))
                (result-digest (ironclad:digest-sequence digest vec))
                (result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
        (cl:loop :for i :from 0 :below 32
                 :do (cl:setf (cl:aref result i) (cl:aref result-digest i)))
        result))))
