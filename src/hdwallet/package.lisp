;;;; HD Wallet package definition
;;;; BIP-39 mnemonic phrases and BIP-32 hierarchical deterministic keys

(defpackage #:web3/hdwallet
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:crypto #:web3/crypto)
   (#:addr #:web3/address))
  (:export
   ;; BIP-39 Mnemonic
   #:generate-mnemonic
   #:mnemonic-to-seed
   #:validate-mnemonic
   #:entropy-to-mnemonic

   ;; BIP-32 HD Keys
   #:HDKey
   #:master-key-from-seed
   #:derive-child
   #:derive-path
   #:hd-private-key
   #:hd-public-key
   #:hd-chain-code
   #:.hd-private-key
   #:.hd-public-key
   #:.hd-chain-code

   ;; Ethereum helpers
   #:derive-ethereum-key
   #:derive-ethereum-address
   #:mnemonic-to-private-key
   #:mnemonic-to-address

   ;; Standard paths
   #:ethereum-path
   #:ethereum-ledger-path))
