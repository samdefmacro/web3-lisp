(in-package #:web3/erc4337)
(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel

  ;;; =========================================================================
  ;;; Shared Hash Helper
  ;;; =========================================================================
  ;;;
  ;;; userOpHash = keccak256(abi.encode(
  ;;;     keccak256(pack(userOp)),
  ;;;     entryPoint,
  ;;;     chainId
  ;;; ))

  (declare %compute-op-hash (types:Bytes -> addr:Address -> U64 -> types:Bytes))
  (define (%compute-op-hash packed-fields entrypoint chain-id)
    "Compute userOpHash from ABI-encoded packed fields, entrypoint, and chain ID."
    (let ((packed-hash (crypto:keccak256 packed-fields)))
      (crypto:keccak256
       (abi:abi-encode
        (Cons (abi:AbiBytesFixedVal packed-hash)
              (Cons (abi:AbiAddressVal (addr:address-bytes entrypoint))
                    (Cons (abi:AbiUintVal (types:u256-from-integer
                                           (lisp Integer (chain-id) chain-id)))
                          Nil)))))))

  ;;; =========================================================================
  ;;; v0.6 UserOperation Hash
  ;;; =========================================================================
  ;;;
  ;;; pack(userOp) = abi.encode(
  ;;;     sender, nonce, keccak256(initCode), keccak256(callData),
  ;;;     callGasLimit, verificationGasLimit, preVerificationGas,
  ;;;     maxFeePerGas, maxPriorityFeePerGas, keccak256(paymasterAndData)
  ;;; )

  (declare %pack-user-op (UserOperation -> types:Bytes))
  (define (%pack-user-op op)
    "ABI-encode the UserOperation fields (excluding signature) for hashing"
    (abi:abi-encode
     (Cons (abi:AbiAddressVal (addr:address-bytes (.user-op-sender op)))
           (Cons (abi:AbiUintVal (.user-op-nonce op))
                 (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.user-op-init-code op)))
                       (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.user-op-call-data op)))
                             (Cons (abi:AbiUintVal (.user-op-call-gas-limit op))
                                   (Cons (abi:AbiUintVal (.user-op-verification-gas-limit op))
                                         (Cons (abi:AbiUintVal (.user-op-pre-verification-gas op))
                                               (Cons (abi:AbiUintVal (.user-op-max-fee-per-gas op))
                                                     (Cons (abi:AbiUintVal (.user-op-max-priority-fee-per-gas op))
                                                           (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.user-op-paymaster-and-data op)))
                                                                 Nil))))))))))))

  (declare user-op-hash (UserOperation -> addr:Address -> U64 -> types:Bytes))
  (define (user-op-hash op entrypoint chain-id)
    "Compute the ERC-4337 v0.6 UserOperation hash.
     This is the hash that gets signed by the account owner."
    (%compute-op-hash (%pack-user-op op) entrypoint chain-id))

  ;;; =========================================================================
  ;;; v0.7 PackedUserOperation Hash
  ;;; =========================================================================
  ;;;
  ;;; pack(packedUserOp) = abi.encode(
  ;;;     sender, nonce, keccak256(initCode), keccak256(callData),
  ;;;     accountGasLimits, preVerificationGas, gasFees,
  ;;;     keccak256(paymasterAndData)
  ;;; )

  (declare %pack-packed-user-op (PackedUserOperation -> types:Bytes))
  (define (%pack-packed-user-op op)
    "ABI-encode the PackedUserOperation fields (excluding signature) for hashing"
    (abi:abi-encode
     (Cons (abi:AbiAddressVal (addr:address-bytes (.packed-op-sender op)))
           (Cons (abi:AbiUintVal (.packed-op-nonce op))
                 (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.packed-op-init-code op)))
                       (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.packed-op-call-data op)))
                             (Cons (abi:AbiBytesFixedVal (.packed-op-account-gas-limits op))
                                   (Cons (abi:AbiUintVal (.packed-op-pre-verification-gas op))
                                         (Cons (abi:AbiBytesFixedVal (.packed-op-gas-fees op))
                                               (Cons (abi:AbiBytesFixedVal (crypto:keccak256 (.packed-op-paymaster-and-data op)))
                                                     Nil))))))))))

  (declare packed-user-op-hash (PackedUserOperation -> addr:Address -> U64 -> types:Bytes))
  (define (packed-user-op-hash op entrypoint chain-id)
    "Compute the ERC-4337 v0.7 PackedUserOperation hash.
     This is the hash that gets signed by the account owner."
    (%compute-op-hash (%pack-packed-user-op op) entrypoint chain-id)))
