;;;; EIP-712 package definition
;;;; Typed structured data hashing and signing

(defpackage #:web3/eip712
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address)
   (#:abi #:web3/abi))
  (:export
   ;; Domain types
   #:EIP712Domain
   #:make-domain
   #:domain-separator
   #:domain-separator-hex

   ;; Type definitions
   #:TypedField
   #:make-field
   #:TypedStruct
   #:make-struct

   ;; Typed values
   #:TypedValue
   #:TypedUint256
   #:TypedAddress
   #:TypedBytes32
   #:TypedBool
   #:TypedString
   #:TypedBytes

   ;; Hashing functions
   #:encode-type
   #:type-hash
   #:encode-data
   #:hash-struct
   #:typed-data-hash

   ;; Signing
   #:sign-typed-data
   #:recover-typed-data-signer

   ;; Common domain presets
   #:permit-domain
   #:permit-struct-hash
   #:eip2612-permit-type-hash))
