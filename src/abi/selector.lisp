(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  (declare function-selector (String -> types:Bytes))
  (define (function-selector signature)
    "Compute the 4-byte function selector from a function signature string.
     Example: (function-selector \"transfer(address,uint256)\") => 0xa9059cbb"
    (let ((sig-bytes
            (lisp types:Bytes (signature)
              (cl:let* ((octets (cl:map 'cl:vector #'cl:char-code signature))
                        (result (cl:make-array (cl:length octets)
                                               :fill-pointer (cl:length octets)
                                               :adjustable cl:t
                                               :initial-contents octets)))
                result))))
      (types:bytes-take 4 (crypto:keccak256 sig-bytes))))

  (declare event-topic (String -> types:Bytes))
  (define (event-topic signature)
    "Compute the 32-byte event topic (keccak256 of the event signature).
     Example: (event-topic \"Transfer(address,address,uint256)\")"
    (let ((sig-bytes
            (lisp types:Bytes (signature)
              (cl:let* ((octets (cl:map 'cl:vector #'cl:char-code signature))
                        (result (cl:make-array (cl:length octets)
                                               :fill-pointer (cl:length octets)
                                               :adjustable cl:t
                                               :initial-contents octets)))
                result))))
      (crypto:keccak256 sig-bytes))))
