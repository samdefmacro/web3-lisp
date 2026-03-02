(defpackage #:web3/types
  (:documentation "Web3 core types - Bytes, U256, hex encoding, errors")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:iter #:coalton-library/iterator))
  (:export
   ;; Error types
   #:Web3Error
   #:HexError
   #:RlpError
   #:CryptoError
   #:AddressError
   #:AbiError
   #:TransactionError
   #:ProviderError
   #:WalletError
   #:Web3Result

   ;; Bytes type
   #:Bytes
   #:bytes-length
   #:bytes-ref
   #:bytes-ref-unsafe
   #:make-bytes
   #:bytes-from-list
   #:bytes-slice
   #:bytes-append
   #:bytes-set!
   #:bytes-copy
   #:bytes-equal?
   #:bytes-empty
   #:bytes-concat-many
   #:bytes-take
   #:bytes-drop
   #:bytes-reverse
   #:bytes-pad-left
   #:bytes-pad-right

   ;; Hex encoding/decoding
   #:hex-encode
   #:hex-decode
   #:hex-encode-prefixed
   #:hex-decode-prefixed

   ;; String utilities
   #:string-to-bytes
   #:string-downcase

   ;; U256 type
   #:U256
   #:u256-make
   #:u256-word
   #:u256-zero
   #:u256-one
   #:u256-from-integer
   #:u256-to-integer
   #:u256-add
   #:u256-sub
   #:u256-mul
   #:u256-div
   #:u256-mod
   #:u256-zero?
   #:u256-equal?
   #:u256-less-than?
   #:u256-greater-than?
   #:u256-to-bytes
   #:u256-from-bytes
   #:u256-max

   ;; Unit conversion
   #:wei-to-gwei
   #:gwei-to-wei
   #:wei-to-ether-string
   #:ether-to-wei))

(in-package #:web3/types)
(named-readtables:in-readtable coalton:coalton)
