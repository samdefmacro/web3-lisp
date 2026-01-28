(defpackage #:web3/rlp
  (:documentation "RLP (Recursive Length Prefix) encoding/decoding")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator)
   (#:list #:coalton-library/list)
   (#:types #:web3/types))
  (:export
   ;; RLP item type
   #:RlpItem
   #:RlpBytes
   #:RlpList

   ;; Encoding
   #:rlp-encode
   #:rlp-encode-bytes
   #:rlp-encode-string
   #:rlp-encode-integer

   ;; Decoding
   #:rlp-decode))

(in-package #:web3/rlp)
(named-readtables:in-readtable coalton:coalton)
