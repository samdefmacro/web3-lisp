(defpackage #:web3/crypto
  (:documentation "Cryptographic primitives - keccak256, secp256k1")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:types #:web3/types))
  (:export
   ;; Keccak256
   #:keccak256

   ;; secp256k1
   #:private-key-to-public-key
   #:public-key-to-uncompressed

   ;; Signature
   #:Signature
   #:make-signature
   #:signature-r
   #:signature-s
   #:signature-v
   #:sign-hash
   #:recover-public-key
   #:signature-to-bytes
   #:signature-from-bytes))

(in-package #:web3/crypto)
(named-readtables:in-readtable coalton:coalton)
