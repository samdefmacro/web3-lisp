(in-package #:web3/abi-call)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; Each helper performs (eth_call -> abi-decode of a single AbiType ->
  ;;; extract the contained value). LABEL is included verbatim in the error
  ;;; message on shape mismatches; callers pass a fully-qualified tag like
  ;;; "erc20:name" so the diagnostic identifies the calling surface.

  (declare call-decode-string
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result String)))
  (define (call-decode-string provider contract-address calldata label)
    "eth_call + abi-decode for a single string return value."
    (do (raw     <- (provider:eth-call provider None contract-address calldata))
        (decoded <- (abi:abi-decode (Cons abi:AbiString Nil) raw))
        (match decoded
          ((Cons (abi:AbiStringVal s) (Nil)) (Ok s))
          (_ (Err (types:AbiError
                   (lisp String (label)
                     (cl:format cl:nil "~A: unexpected response format" label))))))))

  (declare call-decode-u256
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result types:U256)))
  (define (call-decode-u256 provider contract-address calldata label)
    "eth_call + abi-decode for a single uint256 return value."
    (do (raw     <- (provider:eth-call provider None contract-address calldata))
        (decoded <- (abi:abi-decode (Cons (abi:AbiUint 256) Nil) raw))
        (match decoded
          ((Cons (abi:AbiUintVal u) (Nil)) (Ok u))
          (_ (Err (types:AbiError
                   (lisp String (label)
                     (cl:format cl:nil "~A: unexpected response format" label))))))))

  (declare call-decode-address
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result addr:Address)))
  (define (call-decode-address provider contract-address calldata label)
    "eth_call + abi-decode for a single address return value."
    (do (raw     <- (provider:eth-call provider None contract-address calldata))
        (decoded <- (abi:abi-decode (Cons abi:AbiAddress Nil) raw))
        (match decoded
          ((Cons (abi:AbiAddressVal addr-bytes) (Nil))
           (addr:address-from-bytes addr-bytes))
          (_ (Err (types:AbiError
                   (lisp String (label)
                     (cl:format cl:nil "~A: unexpected response format" label))))))))

  (declare call-decode-bool
           (provider:HttpProvider -> addr:Address -> types:Bytes -> String
            -> (types:Web3Result Boolean)))
  (define (call-decode-bool provider contract-address calldata label)
    "eth_call + abi-decode for a single bool return value."
    (do (raw     <- (provider:eth-call provider None contract-address calldata))
        (decoded <- (abi:abi-decode (Cons abi:AbiBool Nil) raw))
        (match decoded
          ((Cons (abi:AbiBoolVal b) (Nil)) (Ok b))
          (_ (Err (types:AbiError
                   (lisp String (label)
                     (cl:format cl:nil "~A: unexpected response format" label)))))))))
