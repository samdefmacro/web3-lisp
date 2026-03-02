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
   (#:provider #:web3/provider))
  (:export
   #:resolve-name
   #:lookup-address
   #:get-resolver
   #:get-text-record
   #:get-contenthash))

(in-package #:web3/ens-resolver)
(named-readtables:in-readtable coalton:coalton)
