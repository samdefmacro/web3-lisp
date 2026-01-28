(in-package #:web3/abi)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; ABI Type descriptors

  (define-type AbiType
    "Solidity ABI type descriptor"
    (AbiUint UFix)          ; uint<N> where N = bits (8,16,...,256)
    (AbiInt UFix)           ; int<N>
    AbiAddress              ; address (uint160)
    AbiBool                 ; bool
    AbiBytes                ; bytes (dynamic)
    (AbiBytesFixed UFix)    ; bytes<N> (1-32)
    AbiString               ; string (dynamic)
    (AbiArray AbiType)      ; T[] (dynamic array)
    (AbiFixedArray AbiType UFix)  ; T[N] (fixed array)
    (AbiTuple (List AbiType)))    ; (T1, T2, ...) tuple

  ;;; ABI Values

  (define-type AbiValue
    "ABI-encoded value"
    (AbiUintVal types:U256)          ; uint<N> value
    (AbiIntVal Integer)              ; int<N> value (signed)
    (AbiAddressVal types:Bytes)      ; address as 20 bytes
    (AbiBoolVal Boolean)             ; bool value
    (AbiBytesVal types:Bytes)        ; bytes value
    (AbiBytesFixedVal types:Bytes)   ; bytesN value
    (AbiStringVal String)            ; string value
    (AbiArrayVal (List AbiValue))    ; dynamic array
    (AbiFixedArrayVal (List AbiValue))  ; fixed array
    (AbiTupleVal (List AbiValue)))   ; tuple

  ;;; Helper to check if a type is dynamic (requires indirection)

  (declare abi-type-dynamic? (AbiType -> Boolean))
  (define (abi-type-dynamic? typ)
    "Check if an ABI type is dynamic (requires offset pointer)"
    (match typ
      ((AbiUint _) False)
      ((AbiInt _) False)
      ((AbiAddress) False)
      ((AbiBool) False)
      ((AbiBytesFixed _) False)
      ((AbiBytes) True)
      ((AbiString) True)
      ((AbiArray _) True)
      ((AbiFixedArray inner _)
       (abi-type-dynamic? inner))
      ((AbiTuple components)
       (list:any abi-type-dynamic? components)))))
