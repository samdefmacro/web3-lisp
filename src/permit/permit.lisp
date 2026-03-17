;;;; EIP-2612 Permit Implementation
;;;;
;;;; Gasless ERC-20 approvals via EIP-712 typed data signatures.
;;;; https://eips.ethereum.org/EIPS/eip-2612
;;;;
;;;; Allows token holders to approve spenders via off-chain signatures,
;;;; enabling gasless approvals and single-transaction approve+transfer flows.

(in-package #:web3/permit)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Function Selectors
  ;;; =========================================================================

  (declare selector-nonces types:Bytes)
  (define selector-nonces
    "Function selector for nonces(address) -> 0x7ecebe00"
    (abi:function-selector "nonces(address)"))

  (declare selector-domain-separator types:Bytes)
  (define selector-domain-separator
    "Function selector for DOMAIN_SEPARATOR() -> 0x3644e515"
    (abi:function-selector "DOMAIN_SEPARATOR()"))

  (declare selector-permit types:Bytes)
  (define selector-permit
    "Function selector for permit(address,address,uint256,uint256,uint8,bytes32,bytes32) -> 0xd505accf"
    (abi:function-selector "permit(address,address,uint256,uint256,uint8,bytes32,bytes32)"))

  ;;; =========================================================================
  ;;; On-chain Reads
  ;;; =========================================================================

  (declare permit-nonces (provider:HttpProvider -> addr:Address -> addr:Address
                          -> (types:Web3Result types:U256)))
  (define (permit-nonces provider token-address owner)
    "Read the permit nonce for an owner from the token contract."
    (let ((calldata (abi:abi-encode-with-selector
                     selector-nonces
                     (Cons (abi:AbiAddressVal (addr:address-bytes owner)) Nil))))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiUint 256) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiUintVal u256) (Nil)) (Ok u256))
              (_ (Err (types:AbiError "Unexpected response format for nonces()"))))))))))

  (declare permit-domain-separator (provider:HttpProvider -> addr:Address
                                    -> (types:Web3Result types:Bytes)))
  (define (permit-domain-separator provider token-address)
    "Read the EIP-712 DOMAIN_SEPARATOR from the token contract."
    (let ((calldata selector-domain-separator))
      (match (provider:eth-call provider None token-address calldata)
        ((Err e) (Err e))
        ((Ok result)
         (match (abi:abi-decode (Cons (abi:AbiBytesFixed 32) Nil) result)
           ((Err e) (Err e))
           ((Ok decoded)
            (match decoded
              ((Cons (abi:AbiBytesFixedVal b) (Nil)) (Ok b))
              (_ (Err (types:AbiError "Unexpected response format for DOMAIN_SEPARATOR()"))))))))))

  ;;; =========================================================================
  ;;; Permit Signing
  ;;; =========================================================================

  (declare sign-permit (types:Bytes -> eip712:EIP712Domain
                        -> addr:Address -> addr:Address
                        -> types:U256 -> types:U256 -> types:U256
                        -> (types:Web3Result crypto:Signature)))
  (define (sign-permit private-key domain owner spender value nonce deadline)
    "Sign an EIP-2612 permit message.
     Returns the ECDSA signature (r, s, v) for use with the permit() function."
    (let ((domain-sep (eip712:domain-separator domain))
          (struct-hash (eip712:permit-struct-hash
                        (addr:address-bytes owner)
                        (addr:address-bytes spender)
                        value nonce deadline)))
      (eip712:sign-typed-data private-key domain-sep struct-hash)))

  ;;; =========================================================================
  ;;; Calldata Builder
  ;;; =========================================================================

  (declare permit-data (addr:Address -> addr:Address -> types:U256 -> types:U256
                        -> crypto:Signature -> types:Bytes))
  (define (permit-data owner spender value deadline sig)
    "Build calldata for permit(address owner, address spender, uint256 value,
     uint256 deadline, uint8 v, bytes32 r, bytes32 s)."
    (let ((v-val (crypto:.signature-v sig))
          (r-bytes (crypto:.signature-r sig))
          (s-bytes (crypto:.signature-s sig)))
      (abi:abi-encode-with-selector
       selector-permit
       (Cons (abi:AbiAddressVal (addr:address-bytes owner))
             (Cons (abi:AbiAddressVal (addr:address-bytes spender))
                   (Cons (abi:AbiUintVal value)
                         (Cons (abi:AbiUintVal deadline)
                               (Cons (abi:AbiUintVal (types:u256-from-integer
                                                      (lisp Integer (v-val)
                                                        (cl:the cl:integer v-val))))
                                     (Cons (abi:AbiBytesFixedVal r-bytes)
                                           (Cons (abi:AbiBytesFixedVal s-bytes)
                                                 Nil))))))))))

  ;;; =========================================================================
  ;;; High-level: Sign + Build Calldata
  ;;; =========================================================================

  (declare erc20-permit (types:Bytes -> eip712:EIP712Domain
                         -> addr:Address -> addr:Address
                         -> types:U256 -> types:U256 -> types:U256
                         -> (types:Web3Result types:Bytes)))
  (define (erc20-permit private-key domain owner spender value nonce deadline)
    "Sign an EIP-2612 permit and return the calldata for permit().
     Combines sign-permit + permit-data into a single call."
    (match (sign-permit private-key domain owner spender value nonce deadline)
      ((Err e) (Err e))
      ((Ok sig) (Ok (permit-data owner spender value deadline sig))))))
