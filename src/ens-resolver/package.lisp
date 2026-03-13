;;;; ENS Resolver - Live ENS resolution via JSON-RPC provider

(defpackage #:web3/ens-resolver
  (:documentation "ENS name resolution via JSON-RPC provider")
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:types #:web3/types)
   (#:addr #:web3/address)
   (#:ens #:web3/ens)
   (#:abi #:web3/abi)
   (#:provider #:web3/provider))
  (:export
   ;; Forward resolution
   #:resolve-name
   #:get-resolver

   ;; Reverse resolution
   #:lookup-address
   #:lookup-address-unchecked

   ;; Multi-coin address resolution (EIP-2304)
   #:resolve-address
   #:coin-type-btc
   #:coin-type-eth
   #:coin-type-sol
   #:coin-type-matic

   ;; Records
   #:get-text-record
   #:get-contenthash))

(in-package #:web3/ens-resolver)
(named-readtables:in-readtable coalton:coalton)
