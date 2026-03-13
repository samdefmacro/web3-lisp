;;;; EIP-2612 Permit package definition
;;;; Gasless ERC-20 approvals via off-chain signatures

(defpackage #:web3/permit
  (:documentation "EIP-2612 Permit - gasless ERC-20 token approvals via signed messages")
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address)
   (#:abi #:web3/abi)
   (#:eip712 #:web3/eip712)
   (#:provider #:web3/provider))
  (:export
   ;; On-chain reads
   #:permit-nonces
   #:permit-domain-separator

   ;; Signing
   #:sign-permit

   ;; Function selectors
   #:selector-nonces
   #:selector-permit

   ;; Calldata builder
   #:permit-data

   ;; High-level: sign + build calldata
   #:erc20-permit))
