(defpackage #:web3/erc4337
  (:documentation "ERC-4337 Account Abstraction - UserOperation types, hashing, and bundler RPC")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; EntryPoint addresses
   #:entrypoint-v06
   #:entrypoint-v07

   ;; v0.6 UserOperation
   #:UserOperation
   #:make-user-operation
   #:.user-op-sender
   #:.user-op-nonce
   #:.user-op-init-code
   #:.user-op-call-data
   #:.user-op-call-gas-limit
   #:.user-op-verification-gas-limit
   #:.user-op-pre-verification-gas
   #:.user-op-max-fee-per-gas
   #:.user-op-max-priority-fee-per-gas
   #:.user-op-paymaster-and-data
   #:.user-op-signature

   ;; v0.7 PackedUserOperation
   #:PackedUserOperation
   #:make-packed-user-operation
   #:.packed-op-sender
   #:.packed-op-nonce
   #:.packed-op-init-code
   #:.packed-op-call-data
   #:.packed-op-account-gas-limits
   #:.packed-op-pre-verification-gas
   #:.packed-op-gas-fees
   #:.packed-op-paymaster-and-data
   #:.packed-op-signature

   ;; v0.7 packing helpers
   #:pack-account-gas-limits
   #:unpack-account-gas-limits
   #:pack-gas-fees
   #:unpack-gas-fees
   #:pack-paymaster-data
   #:unpack-paymaster-data

   ;; Conversion between v0.6 and v0.7
   #:user-op-to-packed
   #:packed-to-user-op

   ;; Hash computation
   #:user-op-hash
   #:packed-user-op-hash

   ;; Bundler RPC methods (v0.6)
   #:eth-send-user-operation
   #:eth-estimate-user-operation-gas
   ;; Bundler RPC methods (v0.7)
   #:eth-send-packed-user-operation
   #:eth-estimate-packed-user-operation-gas
   ;; Bundler RPC methods (shared)
   #:eth-get-user-operation-by-hash
   #:eth-get-user-operation-receipt
   #:eth-supported-entry-points

   ;; Gas estimate result
   #:GasEstimate
   #:.gas-estimate-pre-verification-gas
   #:.gas-estimate-verification-gas-limit
   #:.gas-estimate-call-gas-limit
   #:.gas-estimate-paymaster-verification-gas-limit
   #:.gas-estimate-paymaster-post-op-gas-limit))

(in-package #:web3/erc4337)
(named-readtables:in-readtable coalton:coalton)
