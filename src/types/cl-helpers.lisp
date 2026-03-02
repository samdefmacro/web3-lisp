;;;; CL-level helpers shared across modules
;;;;
;;;; These operate at the Common Lisp level (not Coalton) and are used by
;;;; modules that parse JSON-RPC responses (receipt, block, gas, ws-provider,
;;;; logs, siwe).

(cl:in-package #:web3/types)

;;; =========================================================================
;;; Hex Parsing
;;; =========================================================================

(cl:defun %parse-hex-ufix (hex-str)
  "Parse hex string to UFix"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:parse-integer (cl:subseq hex-str 2) :radix 16)
         0))

(cl:defun %parse-hex-u256 (hex-str)
  "Parse hex string to U256"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:let ((int-val (cl:parse-integer (cl:subseq hex-str 2) :radix 16)))
           (u256-from-integer int-val))
         (u256-from-integer 0)))

(cl:defun %parse-hex-bytes (hex-str)
  "Parse hex string to Bytes (adjustable vector for Coalton)"
  (cl:if (cl:and hex-str (cl:> (cl:length hex-str) 2))
         (cl:let* ((hex-part (cl:subseq hex-str 2))
                   (len (cl:floor (cl:length hex-part) 2))
                   (bytes (cl:make-array len :element-type 'cl:t
                                         :fill-pointer len
                                         :adjustable cl:t)))
           (cl:dotimes (i len bytes)
             (cl:setf (cl:aref bytes i)
                      (cl:parse-integer hex-part :start (cl:* i 2) :end (cl:+ (cl:* i 2) 2) :radix 16))))
         (cl:make-array 0 :element-type 'cl:t :fill-pointer 0 :adjustable cl:t)))

(cl:defun %parse-hex-bytes32 (hex-str)
  "Parse hex string to Bytes (32 bytes, adjustable vector for Coalton)"
  (cl:let ((bytes (%parse-hex-bytes hex-str)))
    (cl:if (cl:= (cl:length bytes) 32)
           bytes
           (cl:let ((result (cl:make-array 32 :element-type 'cl:t
                                           :fill-pointer 32
                                           :adjustable cl:t
                                           :initial-element 0)))
             (cl:dotimes (i (cl:min 32 (cl:length bytes)) result)
               (cl:setf (cl:aref result i) (cl:aref bytes i)))))))

;;; =========================================================================
;;; Coalton Result/Optional helpers (CL level)
;;; =========================================================================

(cl:defun %result-ok-p (result)
  "Check if Coalton Result is Ok"
  (cl:typep result 'coalton-library/classes::result/ok))

(cl:defun %unwrap-ok (result)
  "Extract the inner value from Ok"
  (cl:slot-value result 'coalton-library/classes::|_0|))

(cl:defun %is-some-p (opt)
  "Check if Coalton Optional is Some"
  (cl:typep opt 'coalton-library/classes::optional/some))

(cl:defun %unwrap-some (opt)
  "Extract the inner value from Some"
  (cl:slot-value opt 'coalton-library/classes::|_0|))

;; Export CL-level helpers
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:export '(%parse-hex-ufix
               %parse-hex-u256
               %parse-hex-bytes
               %parse-hex-bytes32
               %result-ok-p
               %unwrap-ok
               %is-some-p
               %unwrap-some)
             (cl:find-package '#:web3/types)))
