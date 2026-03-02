(defpackage #:web3/abi
  (:documentation "Ethereum ABI encoding/decoding")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:list #:coalton-library/list)
   (#:types #:web3/types)
   (#:crypto #:web3/crypto))
  (:export
   ;; ABI types
   #:AbiType
   #:AbiUint
   #:AbiInt
   #:AbiAddress
   #:AbiBool
   #:AbiBytes
   #:AbiBytesFixed
   #:AbiString
   #:AbiArray
   #:AbiFixedArray
   #:AbiTuple

   ;; ABI values
   #:AbiValue
   #:AbiUintVal
   #:AbiIntVal
   #:AbiAddressVal
   #:AbiBoolVal
   #:AbiBytesVal
   #:AbiBytesFixedVal
   #:AbiStringVal
   #:AbiArrayVal
   #:AbiFixedArrayVal
   #:AbiTupleVal

   ;; Encoding/decoding
   #:abi-encode
   #:abi-decode
   #:abi-encode-with-selector

   ;; Selector computation
   #:function-selector
   #:event-topic))

(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)
